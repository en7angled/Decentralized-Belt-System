module Onchain.CIP68 where

import GHC.Generics (Generic)
import PlutusTx
import PlutusTx.AssocMap qualified
import PlutusTx.Blueprint
import PlutusTx.Prelude

type Metadata = PlutusTx.AssocMap.Map BuiltinByteString BuiltinByteString

metadataVersion :: Integer
metadataVersion = 1

-- | The datum datatype which should be locked with the batch ref NFT.
-- | This datatype is following the CIP-68 Datum Metadata Standard.
data CIP68Datum = CIP68Datum
  { metadata :: Metadata, --- ^  Map k v (where k are  UTF-8 encoded @BuiltinByteString@s)
    version :: Integer, --- ^ version of CIP-68 Datum Metadata Standard.
    extra :: BuiltinData ---- ^ Plutus data
  }
  deriving stock (Generic, Show)

makeIsDataSchemaIndexed ''CIP68Datum [('CIP68Datum, 0)]

data MetadataFields
  = Metadata222
  { metadataName :: BuiltinByteString,
    metadataDescription :: BuiltinByteString,
    metadataImageURI :: BuiltinByteString
  }
  deriving stock (Generic, Show)

makeIsDataSchemaIndexed ''MetadataFields [('Metadata222, 0)]

-- All UTF-8 encoded keys and values need to be converted into their respective byte's representation when creating the datum on-chain.
mkCIP68Datum :: (ToData a) => a -> MetadataFields -> CIP68Datum
mkCIP68Datum extraData Metadata222 {..} =
  CIP68Datum
    { metadata =
        PlutusTx.AssocMap.safeFromList
          [ (encodeUtf8 "name", metadataName),
            (encodeUtf8 "description", metadataDescription),
            (encodeUtf8 "image", metadataImageURI)
          ],
      version = metadataVersion,
      extra = toBuiltinData extraData
    }
