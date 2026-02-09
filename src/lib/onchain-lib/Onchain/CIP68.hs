{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use &&" #-}

module Onchain.CIP68 where

import GHC.Generics (Generic)
import PlutusTx.Builtins 
import PlutusLedgerApi.V1.Value
import PlutusTx
import PlutusTx.AssocMap qualified
import PlutusTx.Blueprint
import PlutusTx.Prelude
import Prelude qualified



type ImageURI = BuiltinByteString

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

-- | Maximum allowed sizes for metadata fields (in bytes)
maxNameLength :: Integer
maxNameLength = 128
{-# INLINEABLE maxNameLength #-}

maxDescriptionLength :: Integer
maxDescriptionLength = 1024
{-# INLINEABLE maxDescriptionLength #-}

maxImageURILength :: Integer
maxImageURILength = 256
{-# INLINEABLE maxImageURILength #-}

-- | Validate metadata field sizes to prevent oversized datums
{-# INLINEABLE validateMetadataFields #-}
validateMetadataFields :: MetadataFields -> Bool
validateMetadataFields Metadata222 {..} =
  and
    [ traceIfFalse "Name too long (max 128 bytes)"
        $ lengthOfByteString metadataName <= maxNameLength,
      traceIfFalse "Description too long (max 1024 bytes)"
        $ lengthOfByteString metadataDescription <= maxDescriptionLength,
      traceIfFalse "Image URI too long (max 256 bytes)"
        $ lengthOfByteString metadataImageURI <= maxImageURILength
    ]

-- | Validate image URI size only (for updates)
{-# INLINEABLE validateImageURI #-}
validateImageURI :: BuiltinByteString -> Bool
validateImageURI uri =
  traceIfFalse "Image URI too long (max 256 bytes)"
    $ lengthOfByteString uri <= maxImageURILength

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

getMetadataFields :: CIP68Datum a -> MetadataFields
getMetadataFields (CIP68Datum metadata _ _) =
  Metadata222
    { metadataName = lookupMetdata "name",
      metadataDescription = lookupMetdata "description",
      metadataImageURI = lookupMetdata "image"
    }
  where
    lookupMetdata label = fromMaybe @BuiltinByteString "" $ PlutusTx.AssocMap.lookup (encodeUtf8 label) metadata

updateCIP68DatumImage :: forall a. BuiltinByteString -> CIP68Datum a -> CIP68Datum a
updateCIP68DatumImage newImageURI oldDatum =
  let newMetadata = PlutusTx.AssocMap.insert (encodeUtf8 "image") newImageURI (metadata oldDatum)
   in CIP68Datum
        { metadata = newMetadata,
          version = version oldDatum,
          extra = extra oldDatum
        }

refTokenPrefixBS :: BuiltinByteString
refTokenPrefixBS = integerToByteString BigEndian 4 (0x000643b0 :: Integer)  -- 4 bytes for the prefix according to CIP-67
{-# INLINEABLE refTokenPrefixBS #-}


userTokenPrefixBS :: BuiltinByteString
userTokenPrefixBS = integerToByteString BigEndian 4 (0x000de140 :: Integer)  -- 4 bytes for the prefix according to CIP-67
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

deriveUserFromRefAC :: AssetClass -> AssetClass
deriveUserFromRefAC (AssetClass (cs, tn)) = AssetClass (cs, deriveUserFromRefTN tn)
{-# INLINEABLE deriveUserFromRefAC #-}

deriveRefFromUserAC :: AssetClass -> AssetClass
deriveRefFromUserAC (AssetClass (cs, tn)) = AssetClass (cs, deriveRefFromUserTN tn)
{-# INLINEABLE deriveRefFromUserAC #-}
