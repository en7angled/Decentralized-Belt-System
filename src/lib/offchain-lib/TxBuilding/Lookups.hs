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
import Onchain.Protocol.Types (OracleParams)
import PlutusLedgerApi.V1.Value
import PlutusTx (fromBuiltinData)
import TxBuilding.Context (DeployedScriptsContext (..))
import TxBuilding.Exceptions (TxBuildingException (..))
import TxBuilding.Functors
import TxBuilding.Utils
import TxBuilding.Validators (oracleValidatorGY, profilesValidatorHashGY, ranksValidatorHashGY)

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
