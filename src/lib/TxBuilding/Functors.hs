module TxBuilding.Functors where

import Data.Text qualified as T
import DomainTypes.Profile.Types
import GeniusYield.TxBuilder
import GeniusYield.Types
import Onchain.BJJ (intToBelt)
import Onchain.CIP68 (MetadataFields (..))
import Onchain.Protocol qualified as Onchain
import PlutusLedgerApi.V3
import PlutusTx.Builtins (decodeUtf8)
import PlutusTx.Builtins.HasOpaque (stringToBuiltinByteStringUtf8)

profileDataToMetadataFields :: ProfileData -> MetadataFields
profileDataToMetadataFields ProfileData {profileName, profileDescription, profileImageURI} =
  Metadata222
    { metadataName = textToBuiltinByteString profileName,
      metadataDescription = textToBuiltinByteString profileDescription,
      metadataImageURI = textToBuiltinByteString profileImageURI
    }

metadataFieldsToProfileData :: MetadataFields -> ProfileData
metadataFieldsToProfileData Metadata222 {metadataName, metadataDescription, metadataImageURI} =
  ProfileData
    { profileName = fromBuiltinByteStringUtf8 metadataName,
      profileDescription = fromBuiltinByteStringUtf8 metadataDescription,
      profileImageURI = fromBuiltinByteStringUtf8 metadataImageURI
    }

textToBuiltinByteString :: T.Text -> BuiltinByteString
textToBuiltinByteString = stringToBuiltinByteStringUtf8 . T.unpack

fromBuiltinByteStringUtf8 :: BuiltinByteString -> T.Text
fromBuiltinByteStringUtf8 = T.pack . init . tail . show . decodeUtf8


profileTypeToOnChainProfileType :: ProfileType -> Onchain.OnChainProfileType
profileTypeToOnChainProfileType Practitioner = Onchain.Practitioner
profileTypeToOnChainProfileType Organization = Onchain.Organization

onchainRankToRankInformation :: (MonadError GYTxMonadException m) => Onchain.OnchainRank -> m (Maybe RankInformation)
onchainRankToRankInformation (Onchain.Rank {..}) = do
  gyRankId <- assetClassFromPlutus' rankId
  gyRankAchievedByProfileId <- assetClassFromPlutus' rankAchievedByProfileId
  gyRankAwardedByProfileId <- assetClassFromPlutus' rankAwardedByProfileId

  return $
    Just
      RankInformation
        { rankInfoId = gyRankId,
          rankInfoBelt = intToBelt rankNumber,
          rankInfoAchievedByProfileId = gyRankAchievedByProfileId,
          rankInfoAwardedByProfileId = gyRankAwardedByProfileId,
          rankInfoAchievementDate = timeFromPlutus rankAchievementDate
        }
onchainRankToRankInformation (Onchain.Promotion {}) = return Nothing

onchainPromotionToPromotionInformation :: (MonadError GYTxMonadException m) => Onchain.OnchainRank -> m (Maybe PromotionInformation)
onchainPromotionToPromotionInformation (Onchain.Promotion {..}) = do
  gyRankId <- assetClassFromPlutus' promotionId
  gyRankAchievedByProfileId <- assetClassFromPlutus' promotionAwardedTo
  gyRankAwardedByProfileId <- assetClassFromPlutus' promotionAwardedBy

  return $
    Just
      PromotionInformation
        { promotionInfoId = gyRankId,
          promotionInfoBelt = intToBelt promotionRankNumber,
          promotionInfoAchievedByProfileId = gyRankAchievedByProfileId,
          promotionInfoAwardedByProfileId = gyRankAwardedByProfileId,
          promotionInfoAchievementDate = timeFromPlutus promotionAchievementDate
        }
onchainPromotionToPromotionInformation (Onchain.Rank {}) = return Nothing


