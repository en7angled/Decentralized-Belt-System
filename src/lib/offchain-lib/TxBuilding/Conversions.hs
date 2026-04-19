-- | Bidirectional conversions between on-chain Plutus types and off-chain domain types.
module TxBuilding.Conversions where

import Data.Bifunctor qualified
import Data.List (sortOn)
import Data.Text qualified as T
import DomainTypes.Core.Actions
import DomainTypes.Core.Types
import GeniusYield.TxBuilder
import GeniusYield.Types
import Onchain.CIP68 (CIP68Datum (..), MetadataFields (..), extra, getMetadataFields)
import Onchain.Protocol qualified as Onchain
import Onchain.Protocol.Id qualified as OnchainId
import Onchain.Protocol.Types qualified as OnchainTypes
import PlutusLedgerApi.V3
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Builtins (decodeUtf8)
import PlutusTx.Builtins.HasOpaque (stringToBuiltinByteStringUtf8)
import TxBuilding.SafeOnchainLogic (safeIntToBelt)

-- | Convert domain 'ProfileData' to on-chain CIP-68 metadata fields.
profileDataToMetadataFields :: ProfileData -> MetadataFields
profileDataToMetadataFields ProfileData {profileDataName, profileDataDescription, profileDataImageURI} =
  Metadata222
    { metadataName = textToBuiltinByteString profileDataName,
      metadataDescription = textToBuiltinByteString profileDataDescription,
      metadataImageURI = textToBuiltinByteString profileDataImageURI
    }

-- | Convert on-chain CIP-68 metadata fields to domain 'ProfileData'.
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
fromBuiltinByteStringUtf8 bs =
  let shown = show (decodeUtf8 bs)
   in T.pack $ case shown of
        ('"' : rest) | not (null rest) -> Prelude.init rest
        other -> other

-- | Map domain 'ProfileType' to its on-chain representation.
profileTypeToOnchainProfileType :: ProfileType -> Onchain.OnchainProfileType
profileTypeToOnchainProfileType Practitioner = Onchain.Practitioner
profileTypeToOnchainProfileType Organization = Onchain.Organization

-- | Map on-chain profile type to domain 'ProfileType'.
onchainProfileTypeToProfileType :: Onchain.OnchainProfileType -> ProfileType
onchainProfileTypeToProfileType Onchain.Practitioner = Practitioner
onchainProfileTypeToProfileType Onchain.Organization = Organization

-- | Convert an on-chain rank datum to a domain 'Rank'. Returns 'Nothing' for promotion datums.
onchainRankToRankInformation :: (MonadError GYTxMonadException m) => Onchain.OnchainRank -> m (Maybe Rank)
onchainRankToRankInformation (Onchain.Rank {..}) = do
  gyRankId <- assetClassFromPlutus' rankId
  gyRankAchievedByProfileId <- assetClassFromPlutus' rankAchievedByProfileId
  gyRankAwardedByProfileId <- assetClassFromPlutus' rankAwardedByProfileId
  rankBelt <- safeIntToBelt rankNumber

  return $
    Just
      Rank
        { rankId = gyRankId,
          rankBelt,
          rankAchievedByProfileId = gyRankAchievedByProfileId,
          rankAwardedByProfileId = gyRankAwardedByProfileId,
          rankAchievementDate = timeFromPlutus rankAchievementDate
        }
onchainRankToRankInformation (Onchain.Promotion {}) = return Nothing

-- | Convert an on-chain promotion datum to a domain 'Promotion'. Returns 'Nothing' for rank datums.
onchainPromotionToPromotionInformation :: (MonadError GYTxMonadException m) => Onchain.OnchainRank -> m (Maybe Promotion)
onchainPromotionToPromotionInformation (Onchain.Promotion {..}) = do
  gyRankId <- assetClassFromPlutus' promotionId
  gyRankAchievedByProfileId <- assetClassFromPlutus' promotionAwardedTo
  gyRankAwardedByProfileId <- assetClassFromPlutus' promotionAwardedBy
  promotionBelt <- safeIntToBelt promotionRankNumber

  return $
    Just
      Promotion
        { promotionId = gyRankId,
          promotionBelt,
          promotionAchievedByProfileId = gyRankAchievedByProfileId,
          promotionAwardedByProfileId = gyRankAwardedByProfileId,
          promotionAchievementDate = timeFromPlutus promotionAchievementDate,
          promotionState = PromotionPending
        }
onchainPromotionToPromotionInformation (Onchain.Rank {}) = return Nothing

-- | Extract 'ProfileData' from a CIP-68 profile datum's metadata fields.
profileDatumToProfileData :: CIP68Datum Onchain.OnchainProfile -> ProfileData
profileDatumToProfileData = metadataFieldsToProfileData . getMetadataFields

-- | Convert a full CIP-68 profile datum to a domain 'Profile' (metadata + extra fields).
profileDatumToProfile :: (MonadError GYTxMonadException m) => CIP68Datum Onchain.OnchainProfile -> m Profile
profileDatumToProfile datum = do
  let ProfileData {..} = profileDatumToProfileData datum
      onchainProfile = extra datum
  let profileType = onchainProfileTypeToProfileType $ Onchain.profileType onchainProfile
  gyProfileId <- assetClassFromPlutus' (Onchain.profileId onchainProfile)
  return $
    Profile
      { profileId = gyProfileId,
        profileName = profileDataName,
        profileDescription = profileDataDescription,
        profileImageURI = profileDataImageURI,
        profileType = profileType
      }

------------------------------------------------------------------------------------------------

-- * Membership Conversions

------------------------------------------------------------------------------------------------

-- | Convert an on-chain membership history datum to a domain 'MembershipHistory'.
onchainMembershipHistoryToMembershipHistory :: (MonadError GYTxMonadException m) => OnchainTypes.OnchainMembershipHistory -> m MembershipHistory
onchainMembershipHistoryToMembershipHistory history@OnchainTypes.OnchainMembershipHistory {..} = do
  gyHistoryId <- assetClassFromPlutus' (OnchainId.deriveMembershipHistoryIdFromHistory history)
  gyPractitionerId <- assetClassFromPlutus' membershipHistoryPractitionerId
  gyOrganizationId <- assetClassFromPlutus' membershipHistoryOrganizationId
  return
    MembershipHistory
      { membershipHistoryId = gyHistoryId,
        membershipHistoryPractitionerId = gyPractitionerId,
        membershipHistoryOrganizationId = gyOrganizationId
      }

-- | Convert with a pre-computed interval GYAssetClass (from UTxO value or derivation).
onchainMembershipIntervalToMembershipInterval :: (MonadError GYTxMonadException m) => GYAssetClass -> OnchainTypes.OnchainMembershipInterval -> m MembershipInterval
onchainMembershipIntervalToMembershipInterval gyIntervalId OnchainTypes.OnchainMembershipInterval {..} = do
  gyPractitionerId <- assetClassFromPlutus' membershipIntervalPractitionerId
  return
    MembershipInterval
      { membershipIntervalId = gyIntervalId,
        membershipIntervalStartDate = timeFromPlutus membershipIntervalStartDate,
        membershipIntervalEndDate = timeFromPlutus <$> membershipIntervalEndDate,
        membershipIntervalAccepted = membershipIntervalIsAck,
        membershipIntervalPractitionerId = gyPractitionerId,
        membershipIntervalIntervalNumber = membershipIntervalNumber
      }

-- | Derive the interval ID from the organization's ProfileId and convert.
onchainMembershipIntervalToMembershipIntervalWithOrgId :: (MonadError GYTxMonadException m) => Onchain.ProfileId -> OnchainTypes.OnchainMembershipInterval -> m MembershipInterval
onchainMembershipIntervalToMembershipIntervalWithOrgId orgId interval@OnchainTypes.OnchainMembershipInterval {..} = do
  let historyId = Onchain.deriveMembershipHistoryId orgId membershipIntervalPractitionerId
      intervalId = OnchainId.deriveMembershipIntervalId historyId membershipIntervalNumber
  gyIntervalId <- assetClassFromPlutus' intervalId
  onchainMembershipIntervalToMembershipInterval gyIntervalId interval

------------------------------------------------------------------------------------------------

-- * Achievement Conversions

------------------------------------------------------------------------------------------------

-- | CIP-68 metadata map keys reserved for standard fields ('mkCIP68Datum'), UTF-8 encoded.
cip68StandardMetadataKeys :: [BuiltinByteString]
cip68StandardMetadataKeys =
  [ textToBuiltinByteString "name",
    textToBuiltinByteString "description",
    textToBuiltinByteString "image"
  ]

-- | Key-value pairs from the datum metadata map excluding standard CIP-68 fields, sorted by key.
cip68DatumOtherMetadataPairs :: CIP68Datum a -> [(T.Text, T.Text)]
cip68DatumOtherMetadataPairs datum =
  sortOn fst $
    map
      ( Data.Bifunctor.bimap
          fromBuiltinByteStringUtf8
          fromBuiltinByteStringUtf8
      )
      ( filter
          (not . (`elem` cip68StandardMetadataKeys) . fst)
          (AssocMap.toList (metadata datum))
      )

-- | Convert a CIP-68 achievement datum to a domain 'Achievement'.
onchainAchievementToAchievement :: (MonadError GYTxMonadException m) => CIP68Datum OnchainTypes.OnchainAchievement -> m Achievement
onchainAchievementToAchievement datum = do
  let OnchainTypes.OnchainAchievement {..} = extra datum
      ProfileData {..} = metadataFieldsToProfileData (getMetadataFields datum)
      otherMeta = cip68DatumOtherMetadataPairs datum
  gyAchievementId <- assetClassFromPlutus' achievementId
  gyAwardedTo <- assetClassFromPlutus' achievementAwardedTo
  gyAwardedBy <- assetClassFromPlutus' achievementAwardedBy
  return
    Achievement
      { achievementId = gyAchievementId,
        achievementAwardedToProfileId = gyAwardedTo,
        achievementAwardedByProfileId = gyAwardedBy,
        achievementAchievementDate = timeFromPlutus achievementDate,
        achievementAccepted = achievementIsAccepted,
        achievementName = profileDataName,
        achievementDescription = profileDataDescription,
        achievementImageURI = profileDataImageURI,
        achievementOtherMetadata = otherMeta
      }
