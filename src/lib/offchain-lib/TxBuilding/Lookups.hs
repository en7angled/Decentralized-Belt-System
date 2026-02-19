-- | On-chain and oracle lookups for transaction building (profiles, ranks, memberships, achievements, oracle params, dust).
module TxBuilding.Lookups where

import Control.Monad.Except ()
import Control.Monad.Reader.Class (MonadReader, asks)
import Data.Maybe
import Data.Typeable (cast)
import DomainTypes.Core.Actions
import DomainTypes.Core.Types
import DomainTypes.Transfer.Types
import GeniusYield.TxBuilder
import GeniusYield.TxBuilder.Errors ()
import GeniusYield.Types (GYDatum, GYNetworkId, GYScriptHash, GYTxOutRef, GYUTxO, filterUTxOs, utxoRef, utxoValue, utxosToList)
import GeniusYield.Types.Address
import GeniusYield.Types.Value
import Onchain.CIP68 (CIP68Datum (..))
import Onchain.LinkedList (NodeDatum (..))
import Onchain.Protocol qualified as Onchain
import Onchain.Protocol.Id qualified as OnchainId
import Onchain.Protocol.Types qualified as OnchainTypes
import PlutusLedgerApi.V1.Value (CurrencySymbol, Value, flattenValue)
import TxBuilding.Context (DeployedScriptsContext (..))
import TxBuilding.Exceptions (TxBuildingException (..))
import TxBuilding.Conversions (onchainAchievementToAchievement, onchainMembershipHistoryToMembershipHistory, onchainMembershipIntervalToMembershipInterval, onchainPromotionToPromotionInformation, onchainRankToRankInformation, profileDatumToProfile, profileDatumToProfileData)
import TxBuilding.Utils (achievementAndValueFromUTxO, achievementDatumFromDatum, extractNFTAssetClass, getInlineDatumAndValue, membershipDatumFromDatum, oracleParamsFromDatum, profileAndValueFromUTxO, profileDatumFromDatum, rankAndValueFromUTxO, rankDatumFromDatum)
import TxBuilding.Validators (achievementsValidatorHashGY, membershipsValidatorHashGY, oracleValidatorGY, profilesValidatorHashGY, ranksValidatorHashGY)

------------------------------------------------------------------------------------------------

-- * OnChainProfileData Lookup Functions

------------------------------------------------------------------------------------------------

-- | Return the single UTxO from a list, or throw a domain error for empty or multiple.
singleUTxOOrThrow ::
  (GYTxQueryMonad m) =>
  [GYUTxO] ->
  TxBuildingException ->
  TxBuildingException ->
  m GYUTxO
singleUTxOOrThrow utxos errEmpty errMultiple = case utxos of
  [u] -> return u
  [] -> throwError (GYApplicationException errEmpty)
  _ -> throwError (GYApplicationException errMultiple)

-- | Get UTxO by NFT asset class; rethrow 'NFTNotFound' as the given domain exception.
getUTxOWithNFTOrThrow :: (GYTxQueryMonad m) => GYAssetClass -> TxBuildingException -> m GYUTxO
getUTxOWithNFTOrThrow gyAC onNotFound =
  catchError (getUTxOWithNFT gyAC) $ \e ->
    case e of
      GYApplicationException ex ->
        case cast ex :: Maybe TxBuildingException of
          Just NFTNotFound -> throwError (GYApplicationException onNotFound)
          _ -> throwError e
      _ -> throwError e

getUTxOWithTokenAtAddresses :: (GYTxQueryMonad m) => GYAssetClass -> [GYAddress] -> m GYUTxO
getUTxOWithTokenAtAddresses nftAC addrs = do
  utxos <- utxosAtAddresses addrs
  let utxosWithNFT = filterUTxOs (\utxo -> valueAssetPresent (utxoValue utxo) nftAC) utxos
  singleUTxOOrThrow (utxosToList utxosWithNFT) ProfileNotFound MultipleUtxosFound

getUTxOWithNFT :: (GYTxQueryMonad m) => GYAssetClass -> m GYUTxO
getUTxOWithNFT gyAC = do
  nonAdaToken <- maybe (throwError (GYApplicationException InvalidAssetClass)) return (nonAdaTokenFromAssetClass gyAC)
  utxos <- utxosWithAsset nonAdaToken
  singleUTxOOrThrow (utxosToList utxos) NFTNotFound MultipleUtxosFound

-- | Get profile state datum and value from asset class
getProfileStateDatumAndValue :: (GYTxQueryMonad m) => GYAssetClass -> m (CIP68Datum Onchain.OnchainProfile, Value)
getProfileStateDatumAndValue profileRefAC = do
  profileStateUTxO <- getUTxOWithNFTOrThrow profileRefAC ProfileNotFound
  case profileAndValueFromUTxO profileStateUTxO of
    Just (profile, value) -> return (profile, value)
    Nothing -> throwError (GYApplicationException DatumParseError)

getRankStateDatumAndValue :: (GYTxQueryMonad m) => GYAssetClass -> m (Onchain.OnchainRank, Value)
getRankStateDatumAndValue rankRefAC = do
  rankStateUTxO <- getUTxOWithNFTOrThrow rankRefAC RankNotFound
  case rankAndValueFromUTxO rankStateUTxO of
    Just (rank, value) -> return (rank, value)
    Nothing -> throwError (GYApplicationException RankNotFound)

getProfileRanks :: (GYTxQueryMonad m) => GYAssetClass -> m [Onchain.OnchainRank]
getProfileRanks profileRef = do
  (profileDatum, _profileValue) <- getProfileStateDatumAndValue profileRef
  let profile = extra profileDatum
  case Onchain.profileType profile of
    Onchain.Organization -> throwError (GYApplicationException WrongProfileType)
    Onchain.Practitioner -> do
      currentRank <- assetClassFromPlutus' $ Onchain.getCurrentRankId profile
      getRankList currentRank
  where
    getRankList :: (GYTxQueryMonad m) => GYAssetClass -> m [Onchain.OnchainRank]
    getRankList rankRef = do
      (rankData, _rankValue) <- getRankStateDatumAndValue rankRef
      case rankData of
        Onchain.Promotion {} -> throwError (GYApplicationException WrongRankDataType)
        Onchain.Rank {} -> case Onchain.rankPreviousRankId rankData of
          Nothing -> return [rankData]
          Just previousRankId -> do
            gyPreviousRankId <- assetClassFromPlutus' previousRankId
            previousRanks <- getRankList gyPreviousRankId
            return (rankData : previousRanks)

-- | Query all UTxOs at a validator script address and parse their inline datums.
getAllParsedDatumsAtValidator :: (GYTxQueryMonad m) => GYNetworkId -> GYScriptHash -> (GYDatum -> Maybe a) -> m [a]
getAllParsedDatumsAtValidator nid scriptHash parser = do
  let addr = addressFromScriptHash nid scriptHash
  allDatums <- fmap snd <$> utxosAtAddressesWithDatums [addr]
  return $ mapMaybe parser (catMaybes allDatums)

getAllOnchainValidRanks :: (GYTxQueryMonad m) => GYNetworkId -> m [Onchain.OnchainRank]
getAllOnchainValidRanks nid = getAllParsedDatumsAtValidator nid ranksValidatorHashGY rankDatumFromDatum

getAllPromotions :: (GYTxQueryMonad m) => GYNetworkId -> m [Promotion]
getAllPromotions nid = do
  onChainRanks <- getAllOnchainValidRanks nid
  catMaybes <$> mapM onchainPromotionToPromotionInformation onChainRanks

getAllRanks :: (GYTxQueryMonad m) => GYNetworkId -> m [Rank]
getAllRanks nid = do
  onChainRanks <- getAllOnchainValidRanks nid
  catMaybes <$> mapM onchainRankToRankInformation onChainRanks

getAllOnchainProfiles :: (GYTxQueryMonad m) => GYNetworkId -> m [CIP68Datum Onchain.OnchainProfile]
getAllOnchainProfiles nid = getAllParsedDatumsAtValidator nid profilesValidatorHashGY profileDatumFromDatum

getAllProfilesCount :: (GYTxQueryMonad m) => GYNetworkId -> m Int
getAllProfilesCount nid = length <$> getAllOnchainProfiles nid

getAllProfiles :: (GYTxQueryMonad m) => GYNetworkId -> m [Profile]
getAllProfiles nid = do
  allProfiles <- getAllOnchainProfiles nid
  mapM profileDatumToProfile allProfiles

------------------------------------------------------------------------------------------------

-- * Profile Information  Lookup Functions

------------------------------------------------------------------------------------------------

getPractitionerInformation :: (GYTxQueryMonad m) => ProfileRefAC -> m PractitionerProfileInformation
getPractitionerInformation profileRefAC = do
  (profileDatum, _profileValue) <- getProfileStateDatumAndValue profileRefAC
  let ProfileData {profileDataName, profileDataDescription, profileDataImageURI} = profileDatumToProfileData profileDatum
  case Onchain.profileType (extra profileDatum) of
    Onchain.Practitioner -> do
      ranks <- getProfileRanks profileRefAC
      ranksInfos <- catMaybes <$> mapM onchainRankToRankInformation ranks
      let currentRank = head ranksInfos
      let previousRanks = tail ranksInfos
      return $
        PractitionerProfileInformation
          { practitionerId = profileRefAC,
            practitionerName = profileDataName,
            practitionerDescription = profileDataDescription,
            practitionerImageURI = profileDataImageURI,
            practitionerCurrentRank = currentRank,
            practitionerPreviousRanks = previousRanks
          }
    _ -> throwError (GYApplicationException WrongProfileType)

getOrganizationInformation :: (GYTxQueryMonad m) => ProfileRefAC -> m OrganizationProfileInformation
getOrganizationInformation profileRefAC = do
  (profileDatum, _profileValue) <- getProfileStateDatumAndValue profileRefAC
  let ProfileData {profileDataName, profileDataDescription, profileDataImageURI} = profileDatumToProfileData profileDatum
  case Onchain.profileType (extra profileDatum) of
    Onchain.Organization -> do
      return $
        OrganizationProfileInformation
          { organizationId = profileRefAC,
            organizationName = profileDataName,
            organizationDescription = profileDataDescription,
            organizationImageURI = profileDataImageURI
          }
    _ -> throwError (GYApplicationException WrongProfileType)

------------------------------------------------------------------------------------------------

-- * Oracle Lookup Functions

------------------------------------------------------------------------------------------------

-- | Query the oracle UTxO and parse its 'OracleParams' datum.
-- Looks up the oracle UTxO by finding the oracle NFT at the oracle validator address.
-- Returns the parsed params, the UTxO reference, and the UTxO value.
queryOracleParams ::
  (GYTxQueryMonad m, MonadReader DeployedScriptsContext m) =>
  m (OnchainTypes.OracleParams, GYTxOutRef, GYValue)
queryOracleParams = do
  oracleAC <- asks oracleNFTAssetClass
  oracleAddr <- scriptAddress oracleValidatorGY
  utxo <- getUTxOWithTokenAtAddresses oracleAC [oracleAddr]
  case getInlineDatumAndValue utxo of
    Just (gyDatum, _) ->
      case oracleParamsFromDatum gyDatum of
        Just params -> return (params, utxoRef utxo, utxoValue utxo)
        Nothing -> throwError (GYApplicationException OracleDatumInvalid)
    Nothing -> throwError (GYApplicationException OracleNotFound)

------------------------------------------------------------------------------------------------

-- * Dust / Cleanup Lookup Functions

------------------------------------------------------------------------------------------------

-- | Find dust UTxOs at a validator address: those that contain no token
-- with the protocol's 'CurrencySymbol'. These are griefing/dust UTxOs
-- that can be cleaned up via the permissionless 'Cleanup' redeemer.
getDustUTxOs ::
  (GYTxQueryMonad m) =>
  GYAddress -> CurrencySymbol -> m [GYUTxO]
getDustUTxOs addr protocolCS = do
  allUtxos <- utxosAtAddresses [addr]
  return $ filter (not . hasProtocolToken) (utxosToList allUtxos)
  where
    hasProtocolToken utxo =
      any (\(cs, _, _) -> cs == protocolCS) $ flattenValue $ valueToPlutus $ utxoValue utxo

------------------------------------------------------------------------------------------------

-- * Membership Lookup Functions

------------------------------------------------------------------------------------------------

-- | Get a MembershipDatum (list node or interval) by its NFT asset class.
getMembershipDatumAndValue :: (GYTxQueryMonad m) => GYAssetClass -> m (OnchainTypes.MembershipDatum, Value)
getMembershipDatumAndValue nftAC = do
  utxo <- getUTxOWithNFTOrThrow nftAC MembershipListNodeNotFound
  case getInlineDatumAndValue utxo of
    Just (gyDatum, gyValue) ->
      case membershipDatumFromDatum gyDatum of
        Just md -> return (md, valueToPlutus gyValue)
        Nothing -> throwError (GYApplicationException DatumParseError)
    Nothing -> throwError (GYApplicationException MembershipListNodeNotFound)

-- | Get a membership list node datum and value by its NFT asset class.
getMembershipListNodeDatumAndValue :: (GYTxQueryMonad m) => GYAssetClass -> m (OnchainTypes.MembershipHistoriesListNode, Value)
getMembershipListNodeDatumAndValue nodeAC = do
  (md, val) <- getMembershipDatumAndValue nodeAC
  case md of
    OnchainTypes.ListNodeDatum node -> return (node, val)
    _ -> throwError (GYApplicationException MembershipListNodeNotFound)

-- | Find the insert point for a new membership history.
--
-- The list is sorted by node key (practitioner id). We traverse from the root
-- to find the last node that is strictly before the new key, then decide:
--
--   - Append: (lastNodeAC, Nothing) — new key is greater than all existing; spend last node,
--     new node goes after it. Tx only spends the last node.
--
--   - Insert: (leftNodeAC, Just rightNodeAC) — new key lies between left and right; spend left
--     node, add right node as reference input, new node goes between. Tx spends left and refs right.
--
-- Returns (leftNodeAC, Maybe rightNodeAC) for use in 'createMembershipHistoryTX'.
findInsertPointForNewMembership ::
  (GYTxQueryMonad m) =>
  GYAssetClass ->
  GYAssetClass ->
  m (GYAssetClass, Maybe GYAssetClass)
findInsertPointForNewMembership gyOrgProfileRefAC gyNewPractitionerRefAC = do
  let plutusOrgRef = assetClassToPlutus gyOrgProfileRefAC
      -- Key of the node we are about to create (practitioner id; list is sorted by this).
      newKey = Just (assetClassToPlutus gyNewPractitionerRefAC)

  gyRootAC <- assetClassFromPlutus' (Onchain.deriveMembershipHistoriesListId plutusOrgRef)
  (rootNode, _) <- getMembershipListNodeDatumAndValue gyRootAC

  case nextNodeKey (OnchainTypes.nodeInfo rootNode) of
    -- Empty list: root has no next → append after root.
    Nothing ->
      return (gyRootAC, Nothing)
    -- Non-empty: compare new key with first node's key.
    Just firstKey -> do
      gyFirstAC <- assetClassFromPlutus' (OnchainId.deriveMembershipHistoryId plutusOrgRef firstKey)
      (firstNode, _) <- getMembershipListNodeDatumAndValue gyFirstAC
      let firstKeyMaybe = Just firstKey
      if newKey < firstKeyMaybe
        then -- New key is smallest → insert between root and first node.
          return (gyRootAC, Just gyFirstAC)
        else -- New key >= first → walk list to find predecessor or end.
          go newKey gyFirstAC firstNode
  where
    -- Walk from leftNode: either we are at the end (append) or we find a next node and decide insert vs continue.
    go key leftAC leftNode =
      case nextNodeKey (OnchainTypes.nodeInfo leftNode) of
        -- No next node → we are at the tail; append after leftNode.
        Nothing -> return (leftAC, Nothing)
        Just nextKey -> do
          let orgId = OnchainTypes.organizationId leftNode
              rightHistoryId = OnchainId.deriveMembershipHistoryId orgId nextKey
          gyRightAC <- assetClassFromPlutus' rightHistoryId
          (rightNode, _) <- getMembershipListNodeDatumAndValue gyRightAC
          let nextKeyMaybe = Just nextKey
          if key < nextKeyMaybe
            then -- New key sits between leftNode and rightNode → insert between them.
              return (leftAC, Just gyRightAC)
            else -- New key >= nextKey → keep walking (rightNode becomes new left).
              go key gyRightAC rightNode

-- | Get a membership interval datum and value by its NFT asset class.
getMembershipIntervalDatumAndValue :: (GYTxQueryMonad m) => GYAssetClass -> m (OnchainTypes.OnchainMembershipInterval, Value)
getMembershipIntervalDatumAndValue intervalAC = do
  (md, val) <- getMembershipDatumAndValue intervalAC
  case md of
    OnchainTypes.IntervalDatum interval -> return (interval, val)
    _ -> throwError (GYApplicationException MembershipIntervalNotFound)

-- | Get all membership datums at the memberships validator address.
getAllMembershipDatums :: (GYTxQueryMonad m) => GYNetworkId -> m [OnchainTypes.MembershipDatum]
getAllMembershipDatums nid = getAllParsedDatumsAtValidator nid membershipsValidatorHashGY membershipDatumFromDatum

-- | Extract onchain membership histories from membership datums (list nodes only).
membershipHistoriesFromDatums :: [OnchainTypes.MembershipDatum] -> [OnchainTypes.OnchainMembershipHistory]
membershipHistoriesFromDatums allDatums =
  [ hist
  | OnchainTypes.ListNodeDatum OnchainTypes.MembershipHistoriesListNode {OnchainTypes.nodeInfo = nodeInfo} <- allDatums,
    Just hist <- [nodeData nodeInfo]
  ]

-- | Get all membership histories for a given organization profile.
getMembershipHistoriesForOrganization :: (GYTxQueryMonad m) => GYNetworkId -> ProfileRefAC -> m [MembershipHistory]
getMembershipHistoriesForOrganization nid orgProfileAC = do
  histories <- membershipHistoriesFromDatums <$> getAllMembershipDatums nid
  catMaybes <$> mapM safeConvert histories
  where
    safeConvert hist = do
      mh <- onchainMembershipHistoryToMembershipHistory hist
      gyOrgId <- assetClassFromPlutus' (OnchainTypes.membershipHistoryOrganizationId hist)
      if gyOrgId == orgProfileAC
        then return (Just mh)
        else return Nothing

-- | Get all membership histories at the validator (unfiltered).
getAllMembershipHistories :: (GYTxQueryMonad m) => GYNetworkId -> m [MembershipHistory]
getAllMembershipHistories nid = do
  histories <- membershipHistoriesFromDatums <$> getAllMembershipDatums nid
  mapM onchainMembershipHistoryToMembershipHistory histories

-- | Get all membership intervals at the validator.
-- Derives each interval's NFT ID from its UTxO value.
getAllMembershipIntervals :: (GYTxQueryMonad m) => GYNetworkId -> m [MembershipInterval]
getAllMembershipIntervals nid = do
  let addr = addressFromScriptHash nid membershipsValidatorHashGY
  allUtxos <- utxosAtAddressesWithDatums [addr]
  let intervalPairs =
        [ (iv, extractNFTAssetClass (utxoValue utxo))
        | (utxo, Just gyDatum) <- allUtxos,
          Just (OnchainTypes.IntervalDatum iv) <- [membershipDatumFromDatum gyDatum]
        ]
  sequence
    [ onchainMembershipIntervalToMembershipInterval gyId iv
    | (iv, Just gyId) <- intervalPairs
    ]

-- | Match an interval to a history: same practitioner and derived interval id equals interval id.
intervalBelongsToHistory :: MembershipHistory -> MembershipInterval -> Bool
intervalBelongsToHistory h iv =
  membershipIntervalPractitionerId iv == membershipHistoryPractitionerId h
    && OnchainId.deriveMembershipIntervalId
      (OnchainId.deriveMembershipHistoryId
        (assetClassToPlutus (membershipHistoryOrganizationId h))
        (assetClassToPlutus (membershipHistoryPractitionerId h)))
      (membershipIntervalNumber iv)
    == assetClassToPlutus (membershipIntervalId iv)

-- | Build interval information from interval and owning history's organization id.
intervalToInformation :: MembershipInterval -> ProfileRefAC -> MembershipIntervalInformation
intervalToInformation iv orgId =
  MembershipIntervalInformation
    { membershipIntervalInformationId = membershipIntervalId iv,
      membershipIntervalInformationStartDate = membershipIntervalStartDate iv,
      membershipIntervalInformationEndDate = membershipIntervalEndDate iv,
      membershipIntervalInformationIsAccepted = membershipIntervalIsAccepted iv,
      membershipIntervalInformationPractitionerId = membershipIntervalPractitionerId iv,
      membershipIntervalInformationNumber = membershipIntervalNumber iv,
      membershipIntervalInformationOrganizationId = orgId
    }

-- | Get all membership history informations (history + intervals) from chain.
getAllMembershipHistoryInformation :: (GYTxQueryMonad m) => GYNetworkId -> m [MembershipHistoryInformation]
getAllMembershipHistoryInformation nid = do
  histories <- getAllMembershipHistories nid
  intervals <- getAllMembershipIntervals nid
  let intervalInfosForHistory h =
        map (\iv -> intervalToInformation iv (membershipHistoryOrganizationId h))
          (filter (intervalBelongsToHistory h) intervals)
  return
    [ MembershipHistoryInformation
        { membershipHistoryInformationId = membershipHistoryId h,
          membershipHistoryInformationPractitionerId = membershipHistoryPractitionerId h,
          membershipHistoryInformationOrganizationId = membershipHistoryOrganizationId h,
          membershipHistoryInformationIntervals = intervalInfosForHistory h
        }
    | h <- histories
    ]

-- | Get all membership interval informations (interval + org id) from chain.
getAllMembershipIntervalInformation :: (GYTxQueryMonad m) => GYNetworkId -> m [MembershipIntervalInformation]
getAllMembershipIntervalInformation nid = do
  histories <- getAllMembershipHistories nid
  intervals <- getAllMembershipIntervals nid
  return
    [ intervalToInformation iv (membershipHistoryOrganizationId h)
    | iv <- intervals,
      h <- histories,
      intervalBelongsToHistory h iv
    ]

------------------------------------------------------------------------------------------------

-- * Achievement Lookup Functions

------------------------------------------------------------------------------------------------

-- | Get an achievement CIP68 datum and value by its NFT asset class.
getAchievementDatumAndValue :: (GYTxQueryMonad m) => GYAssetClass -> m (CIP68Datum OnchainTypes.OnchainAchievement, Value)
getAchievementDatumAndValue achievementAC = do
  utxo <- getUTxOWithNFTOrThrow achievementAC AchievementNotFound
  case achievementAndValueFromUTxO utxo of
    Just (datum, value) -> return (datum, value)
    Nothing -> throwError (GYApplicationException AchievementNotFound)

-- | Get all achievements at the achievements validator address.
getAllAchievements :: (GYTxQueryMonad m) => GYNetworkId -> m [Achievement]
getAllAchievements nid = do
  allDatums <- getAllParsedDatumsAtValidator nid achievementsValidatorHashGY achievementDatumFromDatum
  catMaybes <$> mapM safeConvert allDatums
  where
    safeConvert datum = catchError (Just <$> onchainAchievementToAchievement datum) (const $ return Nothing)
