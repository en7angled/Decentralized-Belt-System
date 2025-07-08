module TxBuilding.Functors where

import Data.Text qualified as T
import DomainTypes.Profile.Types
import Onchain.CIP68 (MetadataFields (..))
import Onchain.Protocol qualified as Onchain
import PlutusTx.Builtins.HasOpaque (stringToBuiltinByteString)
import PlutusLedgerApi.V3
import PlutusTx.Builtins (decodeUtf8)

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
    { profileName = T.pack $ show $ decodeUtf8 metadataName,
      profileDescription = T.pack $ show $ decodeUtf8 metadataDescription,
      profileImageURI = T.pack $ show metadataImageURI
    }


textToBuiltinByteString :: T.Text -> BuiltinByteString
textToBuiltinByteString = stringToBuiltinByteString . T.unpack

profileTypeToOnChainProfileType :: ProfileType -> Onchain.OnChainProfileType
profileTypeToOnChainProfileType Practitioner = Onchain.Practitioner
profileTypeToOnChainProfileType Organization = Onchain.Organization


