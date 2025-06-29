module TxBuilding.Functors where

import Data.Text qualified as T
import DomainTypes.Profile.Types
import Onchain.CIP68 (MetadataFields (..))
import Onchain.Types qualified as Onchain
import PlutusTx.Builtins.HasOpaque (stringToBuiltinByteString)

profileDataToMetadataFields :: ProfileData -> MetadataFields
profileDataToMetadataFields ProfileData {profileName, profileDescription, profileImageURI} =
  Metadata222
    { metadataName = stringToBuiltinByteString $ T.unpack profileName,
      metadataDescription = stringToBuiltinByteString $ T.unpack profileDescription,
      metadataImageURI = stringToBuiltinByteString $ T.unpack profileImageURI
    }

profileTypeToOnChainProfileType :: ProfileType -> Onchain.ProfileType
profileTypeToOnChainProfileType Practitioner = Onchain.Practitioner
profileTypeToOnChainProfileType Organization = Onchain.Organization
