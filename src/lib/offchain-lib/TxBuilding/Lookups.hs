module TxBuilding.Lookups where

import Control.Monad.Reader.Class (MonadReader, asks)
import Data.Maybe
import DomainTypes.Core.Actions
import DomainTypes.Core.Types
import DomainTypes.Transfer.Types
import GeniusYield.TxBuilder
import GeniusYield.Types (GYDatum, GYNetworkId, GYScriptHash, GYTxOutRef, GYUTxO, filterUTxOs, utxoRef, utxoValue, utxosToList)
import GeniusYield.Types.Address
import GeniusYield.Types.Datum (datumToPlutus')
import GeniusYield.Types.Value
import Onchain.CIP68 (CIP68Datum (..))
import Onchain.Protocol (OnchainProfile (..), OnchainRank (..), getCurrentRankId)
import Onchain.Protocol qualified as Onchain
import Onchain.Protocol.Id (deriveMembershipHistoryId)
import PlutusLedgerApi.V1.Value (AssetClass (..), CurrencySymbol, Value, flattenValue)
import PlutusTx (fromBuiltinData)
import TxBuilding.Context (DeployedScriptsContext (..))
import TxBuilding.Exceptions (TxBuildingException (..))
import TxBuilding.Functors
import TxBuilding.Utils
import Onchain.Protocol.Types (MembershipDatum (..), MembershipHistoriesListNode (..), OracleParams (..))
import Onchain.Protocol.Types qualified as OnchainTypes
import Onchain.LinkedList (NodeDatum (..))
import TxBuilding.Validators (oracleValidatorGY, profilesValidatorHashGY, ranksValidatorHashGY, membershipsValidatorHashGY)

------------------------------------------------------------------------------------------------

-- * OnChainProfileData Lookup Functions

------------------------------------------------------------------------------------------------

getUtxoWithTokenAtAddresses :: (GYTxQueryMonad m) => GYAssetClass -> [GYAddress] -> m GYUTxO
getUtxoWithTokenAtAddresses nftAC addrs = do
  utxos <- utxosAtAddresses addrs
  let utxosWithNFT = filterUTxOs (\utxo -> valueAssetPresent (utxoValue utxo) nftAC) utxos
  case utxosToList utxosWithNFT of
    [utxo] -> return utxo
    [] -> throwError (GYApplicationException ProfileNotFound)
    _ -> throwError (GYApplicationException MultipleUtxosFound)

getUTxOWithNFT :: (GYTxQueryMonad m) => GYAssetClass -> m GYUTxO
getUTxOWithNFT gyAC = do
  case nonAdaTokenFromAssetClass gyAC of
    Nothing -> throwError (GYApplicationException InvalidAssetClass)
    Just nonAdaToken -> do
      utxos <- utxosWithAsset nonAdaToken
      case utxosToList utxos of
        [utxo] -> return utxo
        [] -> throwError (GYApplicationException ProfileNotFound)
        _ -> throwError (GYApplicationException MultipleUtxosFound)

-- | Get profile state data and value from asset class
getProfileStateDataAndValue :: (GYTxQueryMonad m) => GYAssetClass -> m (CIP68Datum OnchainProfile, Value)
getProfileStateDataAndValue profileRefAC = do
  profileStateUTxO <- getUTxOWithNFT profileRefAC
  case profileAndValueFromUTxO profileStateUTxO of
    Just (profile, value) -> return (profile, value)
    Nothing -> throwError (GYApplicationException DatumParseError)

getRankStateDataAndValue :: (GYTxQueryMonad m) => GYAssetClass -> m (OnchainRank, Value)
getRankStateDataAndValue rankRefAC = do
  rankStateUTxO <- getUTxOWithNFT rankRefAC
  case rankAndValueFromUTxO rankStateUTxO of
    Just (rank, value) -> return (rank, value)
    Nothing -> throwError (GYApplicationException RankNotFound)

getProfileRanks :: (GYTxQueryMonad m) => GYAssetClass -> m [OnchainRank]
getProfileRanks profileRef = do
  (profileDatum, _profileValue) <- getProfileStateDataAndValue profileRef
  let profile = extra profileDatum
  case Onchain.profileType profile of
    Onchain.Organization -> throwError (GYApplicationException WrongProfileType)
    Onchain.Practitioner -> do
      currentRank <- assetClassFromPlutus' $ getCurrentRankId profile
      getRankList currentRank
  where
    getRankList :: (GYTxQueryMonad m) => GYAssetClass -> m [OnchainRank]
    getRankList rankRef = do
      (rankData, _rankValue) <- getRankStateDataAndValue rankRef
      case rankData of
        Onchain.Promotion {} -> throwError (GYApplicationException WrongRankDataType)
        Onchain.Rank {} -> case rankPreviousRankId rankData of
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

getAllOnchainValidRanks :: (GYTxQueryMonad m) => GYNetworkId -> m [OnchainRank]
getAllOnchainValidRanks nid = getAllParsedDatumsAtValidator nid ranksValidatorHashGY rankDatumFromDatum

getAllPromotions :: (GYTxQueryMonad m) => GYNetworkId -> m [Promotion]
getAllPromotions nid = do
  onChainRanks <- getAllOnchainValidRanks nid
  catMaybes <$> mapM onchainPromotionToPromotionInformation onChainRanks

getAllRanks :: (GYTxQueryMonad m) => GYNetworkId -> m [Rank]
getAllRanks nid = do
  onChainRanks <- getAllOnchainValidRanks nid
  catMaybes <$> mapM onchainRankToRankInformation onChainRanks

getAllOnchainProfiles :: (GYTxQueryMonad m) => GYNetworkId -> m [CIP68Datum OnchainProfile]
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
  (profileDatum, _profileValue) <- getProfileStateDataAndValue profileRefAC
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
  (profileDatum, _profileValue) <- getProfileStateDataAndValue profileRefAC
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
  m (OracleParams, GYTxOutRef, GYValue)
queryOracleParams = do
  oracleAC <- asks oracleNFTAssetClass
  oracleAddr <- scriptAddress oracleValidatorGY
  utxo <- getUtxoWithTokenAtAddresses oracleAC [oracleAddr]
  case getInlineDatumAndValue utxo of
    Just (gyDatum, _) ->
      case fromBuiltinData (datumToPlutus' gyDatum) of
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
getMembershipDatumAndValue :: (GYTxQueryMonad m) => GYAssetClass -> m (MembershipDatum, Value)
getMembershipDatumAndValue nftAC = do
  utxo <- getUTxOWithNFT nftAC
  case getInlineDatumAndValue utxo of
    Just (gyDatum, gyValue) ->
      case fromBuiltinData (datumToPlutus' gyDatum) of
        Just md -> return (md, valueToPlutus gyValue)
        Nothing -> throwError (GYApplicationException DatumParseError)
    Nothing -> throwError (GYApplicationException MembershipListNodeNotFound)

-- | Get a membership list node datum and value by its NFT asset class.
getMembershipListNodeDatumAndValue :: (GYTxQueryMonad m) => GYAssetClass -> m (MembershipHistoriesListNode, Value)
getMembershipListNodeDatumAndValue nodeAC = do
  (md, val) <- getMembershipDatumAndValue nodeAC
  case md of
    ListNodeDatum node -> return (node, val)
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

  case nextNodeKey (nodeInfo rootNode) of
    -- Empty list: root has no next → append after root.
    Nothing ->
      return (gyRootAC, Nothing)
    -- Non-empty: compare new key with first node's key.
    Just firstKey -> do
      gyFirstAC <- assetClassFromPlutus' (deriveMembershipHistoryId plutusOrgRef firstKey)
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
      case nextNodeKey (nodeInfo leftNode) of
        -- No next node → we are at the tail; append after leftNode.
        Nothing -> return (leftAC, Nothing)
        Just nextKey -> do
          let orgId = OnchainTypes.organizationId leftNode
              rightHistoryId = deriveMembershipHistoryId orgId nextKey
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
    IntervalDatum interval -> return (interval, val)
    _ -> throwError (GYApplicationException MembershipIntervalNotFound)

-- | Get all membership datums at the memberships validator address.
getAllMembershipDatums :: (GYTxQueryMonad m) => GYNetworkId -> m [MembershipDatum]
getAllMembershipDatums nid = getAllParsedDatumsAtValidator nid membershipsValidatorHashGY membershipDatumParser
  where
    membershipDatumParser gyDatum = fromBuiltinData (datumToPlutus' gyDatum)

-- | Get all membership histories for a given organization profile.
getMembershipHistoriesForOrganization :: (GYTxQueryMonad m) => GYNetworkId -> ProfileRefAC -> m [MembershipHistory]
getMembershipHistoriesForOrganization nid orgProfileAC = do
  allDatums <- getAllMembershipDatums nid
  let histories =
        [ hist
          | ListNodeDatum MembershipHistoriesListNode {nodeInfo} <- allDatums,
            Just hist <- [nodeData nodeInfo]
        ]
  catMaybes <$> mapM safeConvert histories
  where
    safeConvert hist = do
      mh <- onchainMembershipHistoryToMembershipHistory hist
      gyOrgId <- assetClassFromPlutus' (OnchainTypes.membershipHistoryOrganizationId hist)
      if gyOrgId == orgProfileAC
        then return (Just mh)
        else return Nothing

-- | Get all membership intervals at the validator.
getAllMembershipIntervals :: (GYTxQueryMonad m) => GYNetworkId -> m [MembershipInterval]
getAllMembershipIntervals nid = do
  allDatums <- getAllMembershipDatums nid
  let intervals = [iv | IntervalDatum iv <- allDatums]
  mapM onchainMembershipIntervalToMembershipInterval intervals
