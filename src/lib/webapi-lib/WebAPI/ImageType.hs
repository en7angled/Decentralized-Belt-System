-- | Image type detection by magic bytes, for validating uploads before they
-- are forwarded to storage.
module WebAPI.ImageType
  ( ImageType (..),
    detectImageType,
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

-- | Supported upload image formats.
data ImageType = JPEG | PNG
  deriving (Eq, Show)

-- | Detect an image type from a byte prefix, or 'Nothing' if unrecognized.
-- JPEG starts with @FF D8 FF@; PNG with @89 50 4E 47@.
detectImageType :: ByteString -> Maybe ImageType
detectImageType bs
  | jpegMagic `BS.isPrefixOf` bs = Just JPEG
  | pngMagic `BS.isPrefixOf` bs = Just PNG
  | otherwise = Nothing
  where
    jpegMagic = BS.pack [0xFF, 0xD8, 0xFF]
    pngMagic = BS.pack [0x89, 0x50, 0x4E, 0x47]
