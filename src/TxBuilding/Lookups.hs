module TxBuilding.Lookups where

import DomainTypes.Profile.Types
import GeniusYield.TxBuilder
import GeniusYield.Types (GYAddress, GYUTxO, filterUTxOs, utxoValue, utxosToList)
import GeniusYield.Types.Time
import GeniusYield.Types.Value
import Onchain.BJJ (intToBelt)
import Onchain.CIP68 (CIP68Datum (..), getMetadataFields)
import Onchain.Protocol (OnchainProfile (..), OnchainRank (..), getCurrentRankId)
import qualified Onchain.Protocol as Onchain
import PlutusLedgerApi.V1.Value
import TxBuilding.Exceptions (ProfileException (..))
import TxBuilding.Functors
import TxBuilding.Utils

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
    _ -> throwError (GYApplicationException InvalidAssetClass)

getUTxOWithNFT :: (GYTxQueryMonad m) => GYAssetClass -> m GYUTxO
getUTxOWithNFT gyAC = do
  case nonAdaTokenFromAssetClass gyAC of
    Nothing -> throwError (GYApplicationException InvalidAssetClass)
    Just nonAdaToken -> do
      utxos <- utxosWithAsset nonAdaToken
      case utxosToList utxos of
        [utxo] -> return utxo
        [] -> throwError (GYApplicationException ProfileNotFound)
        _ -> throwError (GYApplicationException InvalidAssetClass)

-- | Get profile state data and value from asset class
getProfileStateDataAndValue :: (GYTxQueryMonad m) => GYAssetClass -> m (CIP68Datum OnchainProfile, Value)
getProfileStateDataAndValue profileRefAC = do
  profileStateUTxO <- getUTxOWithNFT profileRefAC
  case profileAndValueFromUTxO profileStateUTxO of
    Just (profile, value) -> return (profile, value)
    Nothing -> throwError (GYApplicationException ProfileNotFound)

getRankStateDataAndValue :: (GYTxQueryMonad m) => GYAssetClass -> m (OnchainRank, Value)
getRankStateDataAndValue rankRefAC = do
  rankStateUTxO <- getUTxOWithNFT rankRefAC
  case rankAndValueFromUTxO rankStateUTxO of
    Just (rank, value) -> return (rank, value)
    Nothing -> throwError (GYApplicationException RankNotFound)

getProfileRanks :: (GYTxQueryMonad m) => GYAssetClass -> m [OnchainRank]
getProfileRanks profileRef = do
  (CIP68Datum _metadata _version profile, _profileValue) <- getProfileStateDataAndValue profileRef
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
        PendingRank {} -> throwError (GYApplicationException WrongRankDataType)
        Rank {} -> case rankPreviousRankId rankData of
          Nothing -> return [rankData]
          Just previousRankId -> do
            gyPreviousRankId <- assetClassFromPlutus' previousRankId
            previousRanks <- getRankList gyPreviousRankId
            return (rankData : previousRanks)

------------------------------------------------------------------------------------------------

-- * Domain Lookup Functions

------------------------------------------------------------------------------------------------

onchainRankToRankInformation :: (GYTxQueryMonad m) => OnchainRank -> m RankInformation
onchainRankToRankInformation (Onchain.Rank {..}) = do
  gyRankId <- assetClassFromPlutus' rankId
  gyRankAchievedByProfileId <- assetClassFromPlutus' rankAchievedByProfileId
  gyRankAwardedByProfileId <- assetClassFromPlutus' rankAwardedByProfileId

  return
    RankInformation
      { rankInfoId = gyRankId,
        rankInfoBelt = intToBelt rankNumber,
        rankInfoAchievedByProfileId = gyRankAchievedByProfileId,
        rankInfoAwardedByProfileId = gyRankAwardedByProfileId,
        rankInfoAchievementDate = timeFromPlutus rankAchievementDate
      }
onchainRankToRankInformation (Onchain.PendingRank {}) = throwError (GYApplicationException WrongRankDataType)

getProfileInformation :: (GYTxQueryMonad m) => GYAssetClass -> m ProfileInformation
getProfileInformation profileRefAC = do
  (profileDatum, _profileValue) <- getProfileStateDataAndValue profileRefAC
  let ProfileData {profileName, profileDescription, profileImageURI} = metadataFieldsToProfileData (getMetadataFields profileDatum)
  case Onchain.profileType (extra profileDatum) of
    Onchain.Organization -> do
      return $
        OrganizationProfileInformation
          { organizationId = profileRefAC,
            organizationName = profileName,
            organizationDescription = profileDescription,
            organizationImageURI = profileImageURI
          }
    Onchain.Practitioner -> do
      ranks <- getProfileRanks profileRefAC
      ranksInfos <- mapM onchainRankToRankInformation ranks
      let currentRank = head ranksInfos
      let previousRanks = tail ranksInfos
      return $
        PractitionerProfileInformation
          { practitionerId = profileRefAC,
            practitionerName = profileName,
            practitionerDescription = profileDescription,
            practitionerImageURI = profileImageURI,
            practitionerCurrentRank = currentRank,
            practitionerPreviousRanks = previousRanks
          }
