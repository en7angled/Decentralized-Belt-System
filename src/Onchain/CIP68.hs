{-# LANGUAGE NoImplicitPrelude #-}

module Onchain.CIP68 where

import GHC.Generics (Generic)
import Onchain.Utils (integerToBs24)
import PlutusLedgerApi.V1.Value
import PlutusLedgerApi.V3
import PlutusTx
import PlutusTx.AssocMap qualified
import PlutusTx.Blueprint
import PlutusTx.Prelude
import Prelude qualified

type Metadata = PlutusTx.AssocMap.Map BuiltinByteString BuiltinByteString

metadataVersion :: Integer
metadataVersion = 1

-- | The datum datatype which should be locked with the batch ref NFT.
-- | This datatype is following the CIP-68 Datum Metadata Standard.
data CIP68Datum plutusData = CIP68Datum
  { metadata :: Metadata, --- ^  Map k v (where k are  UTF-8 encoded @BuiltinByteString@s)
    version :: Integer, --- ^ version of CIP-68 Datum Metadata Standard.
    extra :: plutusData ---- ^ Plutus data
  }
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''CIP68Datum [('CIP68Datum, 0)]

data MetadataFields
  = Metadata222
  { metadataName :: BuiltinByteString,
    metadataDescription :: BuiltinByteString,
    metadataImageURI :: BuiltinByteString
  }
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''MetadataFields [('Metadata222, 0)]

-- All UTF-8 encoded keys and values need to be converted into their respective byte's representation when creating the datum on-chain.
mkCIP68Datum :: a -> MetadataFields -> CIP68Datum a
mkCIP68Datum extraData Metadata222 {..} =
  CIP68Datum
    { metadata =
        PlutusTx.AssocMap.unsafeFromList -- Safe becase keys are unique
          [ (encodeUtf8 "name", metadataName),
            (encodeUtf8 "description", metadataDescription),
            (encodeUtf8 "image", metadataImageURI)
          ],
      version = metadataVersion,
      extra = extraData
    }

updateCIP68DatumImage :: forall a. BuiltinByteString -> CIP68Datum a -> CIP68Datum a
updateCIP68DatumImage newImageURI oldDatum =
  let newMetadata = PlutusTx.AssocMap.insert (encodeUtf8 "image") newImageURI (metadata oldDatum)
   in CIP68Datum
        { metadata = newMetadata,
          version = version oldDatum,
          extra = extra oldDatum
        }

refTokenPrefixBS :: BuiltinByteString
refTokenPrefixBS = integerToBs24 (0x000643b0 :: Integer) -- TODO update with new builtins
{-# INLINEABLE refTokenPrefixBS #-}

userTokenPrefixBS :: BuiltinByteString
userTokenPrefixBS = integerToBs24 (0x000de140 :: Integer) -- TODO update with new builtins
{-# INLINEABLE userTokenPrefixBS #-}

generateRefAndUserTN :: BuiltinByteString -> (TokenName, TokenName)
generateRefAndUserTN bs = (TokenName (refTokenPrefixBS <> bs), TokenName (userTokenPrefixBS <> bs))
{-# INLINEABLE generateRefAndUserTN #-}

deriveUserFromRefTN :: TokenName -> TokenName
deriveUserFromRefTN (TokenName bs) = TokenName (userTokenPrefixBS <> sliceByteString 4 (lengthOfByteString bs) bs)
{-# INLINEABLE deriveUserFromRefTN #-}

deriveRefFromUserTN :: TokenName -> TokenName
deriveRefFromUserTN (TokenName bs) = TokenName (refTokenPrefixBS <> sliceByteString 4 (lengthOfByteString bs) bs)
{-# INLINEABLE deriveRefFromUserTN #-}
