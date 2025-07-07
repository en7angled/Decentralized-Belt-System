module TxBuilding.Functors where

import Data.Text qualified as T
import DomainTypes.Profile.Types
import Onchain.CIP68 (MetadataFields (..))
import Onchain.Protocol qualified as Onchain
import PlutusTx.Builtins.HasOpaque (stringToBuiltinByteString)
import PlutusLedgerApi.V1

profileDataToMetadataFields :: ProfileData -> MetadataFields
profileDataToMetadataFields ProfileData {profileName, profileDescription, profileImageURI} =
  Metadata222
    { metadataName = textToBuiltinByteString profileName,
      metadataDescription = textToBuiltinByteString profileDescription,
      metadataImageURI = textToBuiltinByteString profileImageURI
    }

textToBuiltinByteString :: T.Text -> BuiltinByteString
textToBuiltinByteString = stringToBuiltinByteString . T.unpack

profileTypeToOnChainProfileType :: ProfileType -> Onchain.OnChainProfileType
profileTypeToOnChainProfileType Practitioner = Onchain.Practitioner
profileTypeToOnChainProfileType Organization = Onchain.Organization

toPlutusPOSIXTime :: POSIXTimeInteger -> POSIXTime
toPlutusPOSIXTime = POSIXTime 


