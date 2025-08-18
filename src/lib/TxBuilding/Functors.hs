module TxBuilding.Functors where

import Data.Text qualified as T
import DomainTypes.Core.Types
import DomainTypes.Core.Actions
import GeniusYield.TxBuilder
import GeniusYield.Types
import Onchain.BJJ (intToBelt)
import Onchain.CIP68 (MetadataFields (..), getMetadataFields, extra, CIP68Datum)
import Onchain.Protocol qualified as Onchain
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


profileTypeToOnChainProfileType :: ProfileType -> Onchain.OnChainProfileType
profileTypeToOnChainProfileType Practitioner = Onchain.Practitioner
profileTypeToOnChainProfileType Organization = Onchain.Organization

onChainProfileTypeToProfileType :: Onchain.OnChainProfileType -> ProfileType
onChainProfileTypeToProfileType Onchain.Practitioner = Practitioner
onChainProfileTypeToProfileType Onchain.Organization = Organization

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
  let profileType = onChainProfileTypeToProfileType $ Onchain.profileType onchainProfile
  gyProfileId <- assetClassFromPlutus' (Onchain.profileId onchainProfile)
  return $ Profile
    { profileId = gyProfileId,
      profileName = profileDataName,
      profileDescription = profileDataDescription,
      profileImageURI = profileDataImageURI,
      profileType = profileType
    }