module TxBuilding.Functors where

import Data.Text qualified as T
import DomainTypes.Core.Types
import DomainTypes.Core.Actions
import GeniusYield.TxBuilder
import GeniusYield.Types
import Onchain.BJJ (intToBelt)
import Onchain.CIP68 (MetadataFields (..), getMetadataFields, extra, CIP68Datum)
import Onchain.Protocol qualified as Onchain
import Onchain.Protocol.Id (deriveMembershipHistoryIdFromHistory, deriveMembershipIntervalId)
import Onchain.Protocol.Types (OnchainAchievement (..), OnchainMembershipHistory (..), OnchainMembershipInterval (..))
import PlutusLedgerApi.V3
import PlutusTx.Builtins (decodeUtf8)
import PlutusTx.Builtins.HasOpaque (stringToBuiltinByteStringUtf8)

profileDataToMetadataFields :: ProfileData -> MetadataFields
profileDataToMetadataFields ProfileData {profileDataName, profileDataDescription, profileDataImageURI} =
  Metadata222
    { metadataName = textToBuiltinByteString profileDataName,
      metadataDescription = textToBuiltinByteString profileDataDescription,
      metadataImageURI = textToBuiltinByteString profileDataImageURI
    }

metadataFieldsToProfileData :: MetadataFields -> ProfileData
metadataFieldsToProfileData Metadata222 {metadataName, metadataDescription, metadataImageURI} =
  ProfileData
    { profileDataName = fromBuiltinByteStringUtf8 metadataName,
      profileDataDescription = fromBuiltinByteStringUtf8 metadataDescription,
      profileDataImageURI = fromBuiltinByteStringUtf8 metadataImageURI
    }

textToBuiltinByteString :: T.Text -> BuiltinByteString
textToBuiltinByteString = stringToBuiltinByteStringUtf8 . T.unpack

fromBuiltinByteStringUtf8 :: BuiltinByteString -> T.Text
fromBuiltinByteStringUtf8 = T.pack . init . tail . show . decodeUtf8


profileTypeToOnchainProfileType :: ProfileType -> Onchain.OnchainProfileType
profileTypeToOnchainProfileType Practitioner = Onchain.Practitioner
profileTypeToOnchainProfileType Organization = Onchain.Organization

onchainProfileTypeToProfileType :: Onchain.OnchainProfileType -> ProfileType
onchainProfileTypeToProfileType Onchain.Practitioner = Practitioner
onchainProfileTypeToProfileType Onchain.Organization = Organization

onchainRankToRankInformation :: (MonadError GYTxMonadException m) => Onchain.OnchainRank -> m (Maybe Rank)
onchainRankToRankInformation (Onchain.Rank {..}) = do
  gyRankId <- assetClassFromPlutus' rankId
  gyRankAchievedByProfileId <- assetClassFromPlutus' rankAchievedByProfileId
  gyRankAwardedByProfileId <- assetClassFromPlutus' rankAwardedByProfileId

  return $
    Just
      Rank
        { rankId = gyRankId,
          rankBelt = intToBelt rankNumber,
          rankAchievedByProfileId = gyRankAchievedByProfileId,
          rankAwardedByProfileId = gyRankAwardedByProfileId,
          rankAchievementDate = timeFromPlutus rankAchievementDate
        }
onchainRankToRankInformation (Onchain.Promotion {}) = return Nothing

onchainPromotionToPromotionInformation :: (MonadError GYTxMonadException m) => Onchain.OnchainRank -> m (Maybe Promotion)
onchainPromotionToPromotionInformation (Onchain.Promotion {..}) = do
  gyRankId <- assetClassFromPlutus' promotionId
  gyRankAchievedByProfileId <- assetClassFromPlutus' promotionAwardedTo
  gyRankAwardedByProfileId <- assetClassFromPlutus' promotionAwardedBy

  return $
    Just
      Promotion
        { promotionId = gyRankId,
          promotionBelt = intToBelt promotionRankNumber,
          promotionAchievedByProfileId = gyRankAchievedByProfileId,
          promotionAwardedByProfileId = gyRankAwardedByProfileId,
          promotionAchievementDate = timeFromPlutus promotionAchievementDate
        }
onchainPromotionToPromotionInformation (Onchain.Rank {}) = return Nothing



profileDatumToProfileData :: CIP68Datum Onchain.OnchainProfile ->  ProfileData
profileDatumToProfileData  = metadataFieldsToProfileData . getMetadataFields


profileDatumToProfile :: (MonadError GYTxMonadException m) => CIP68Datum Onchain.OnchainProfile -> m Profile
profileDatumToProfile datum = do
  let ProfileData {..} = profileDatumToProfileData datum
      onchainProfile =  extra datum
  let profileType = onchainProfileTypeToProfileType $ Onchain.profileType onchainProfile
  gyProfileId <- assetClassFromPlutus' (Onchain.profileId onchainProfile)
  return $ Profile
    { profileId = gyProfileId,
      profileName = profileDataName,
      profileDescription = profileDataDescription,
      profileImageURI = profileDataImageURI,
      profileType = profileType
    }

------------------------------------------------------------------------------------------------

-- * Membership Conversions

------------------------------------------------------------------------------------------------

onchainMembershipHistoryToMembershipHistory :: (MonadError GYTxMonadException m) => OnchainMembershipHistory -> m MembershipHistory
onchainMembershipHistoryToMembershipHistory history@OnchainMembershipHistory {..} = do
  gyHistoryId <- assetClassFromPlutus' (deriveMembershipHistoryIdFromHistory history)
  gyPractitionerId <- assetClassFromPlutus' membershipHistoryPractitionerId
  gyOrganizationId <- assetClassFromPlutus' membershipHistoryOrganizationId
  return
    MembershipHistory
      { membershipHistoryId = gyHistoryId,
        membershipHistoryPractitionerId = gyPractitionerId,
        membershipHistoryOrganizationId = gyOrganizationId
      }

-- | Convert with a pre-computed interval GYAssetClass (from UTxO value or derivation).
onchainMembershipIntervalToMembershipInterval :: (MonadError GYTxMonadException m) => GYAssetClass -> OnchainMembershipInterval -> m MembershipInterval
onchainMembershipIntervalToMembershipInterval gyIntervalId OnchainMembershipInterval {..} = do
  gyPractitionerId <- assetClassFromPlutus' membershipIntervalPractitionerId
  return
    MembershipInterval
      { membershipIntervalId = gyIntervalId,
        membershipIntervalStartDate = timeFromPlutus membershipIntervalStartDate,
        membershipIntervalEndDate = timeFromPlutus <$> membershipIntervalEndDate,
        membershipIntervalIsAccepted = membershipIntervalIsAck,
        membershipIntervalPractitionerId = gyPractitionerId,
        membershipIntervalNumber = membershipIntervalNumber
      }

-- | Derive the interval ID from the organization's ProfileId and convert.
onchainMembershipIntervalToMembershipIntervalWithOrgId :: (MonadError GYTxMonadException m) => Onchain.ProfileId -> OnchainMembershipInterval -> m MembershipInterval
onchainMembershipIntervalToMembershipIntervalWithOrgId orgId interval@OnchainMembershipInterval {..} = do
  let historyId = Onchain.deriveMembershipHistoryId orgId membershipIntervalPractitionerId
      intervalId = deriveMembershipIntervalId historyId membershipIntervalNumber
  gyIntervalId <- assetClassFromPlutus' intervalId
  onchainMembershipIntervalToMembershipInterval gyIntervalId interval

------------------------------------------------------------------------------------------------

-- * Achievement Conversions

------------------------------------------------------------------------------------------------

onchainAchievementToAchievement :: (MonadError GYTxMonadException m) => CIP68Datum OnchainAchievement -> m Achievement
onchainAchievementToAchievement datum = do
  let OnchainAchievement {..} = extra datum
      ProfileData {..} = metadataFieldsToProfileData (getMetadataFields datum)
  gyAchievementId <- assetClassFromPlutus' achievementId
  gyAwardedTo <- assetClassFromPlutus' achievementAwardedTo
  gyAwardedBy <- assetClassFromPlutus' achievementAwardedBy
  return
    Achievement
      { achievementId = gyAchievementId,
        achievementAwardedTo = gyAwardedTo,
        achievementAwardedBy = gyAwardedBy,
        achievementDate = timeFromPlutus achievementDate,
        achievementIsAccepted = achievementIsAccepted,
        achievementName = profileDataName,
        achievementDescription = profileDataDescription,
        achievementImageURI = profileDataImageURI
      }