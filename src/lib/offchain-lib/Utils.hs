-- | ANSI-colored console output helpers and JSON configuration file/env loaders.
module Utils where

import Data.Aeson
import Data.Aeson qualified as Aeson
import qualified Data.Aeson.Types as AesonTypes
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.List.Extra (dropPrefix)
import Data.Swagger.SchemaOptions (SchemaOptions, fromAesonOptions)
import System.Directory.Extra
import System.Environment
import Prelude

-- | Wrap a string with an ANSI color code and surrounding newlines.
colorString :: String -> String -> String
colorString code s = "\n\ESC[" ++ code ++ "m" ++ s ++ "\ESC[0m\n"

greenColorString :: String -> String
greenColorString = colorString "1;32"

orangeColorString :: String -> String
orangeColorString = colorString "1;33"

yellowColorString :: String -> String
yellowColorString = colorString "1;93"

blueColorString :: String -> String
blueColorString = colorString "1;94"

purpleColorString :: String -> String
purpleColorString = colorString "1;95"

cyanColorString :: String -> String
cyanColorString = colorString "1;96"

-- | Decode a JSON configuration file, returning 'Nothing' if the file does not exist.
decodeConfigFile :: (FromJSON a) => FilePath -> IO (Maybe a)
decodeConfigFile path = do
  fileExist <- doesFileExist path
  if fileExist
    then decodeFileStrict path
    else do
      putStrLn $ yellowColorString $ "File " <> path <> " does not exist"
      return Nothing

-- | Try to read a JSON-encoded configuration from an environment variable.
-- If the variable is present, decode its contents; otherwise, fall back to the given file path.
decodeConfigEnvOrFile :: (FromJSON a) => String -> FilePath -> IO (Maybe a)
decodeConfigEnvOrFile envName filePath = do
  mVal <- lookupEnv envName
  case mVal of
    Just raw -> do
      putStrLn $ yellowColorString $ "Parsing config from env var " <> show envName
      case eitherDecodeStrict (BS8.pack raw) of
        Right a -> return (Just a)
        Left err -> error $ "Decoding env var " <> envName <> " failed: " <> err
    Nothing -> do
      putStrLn $ yellowColorString $ "Parsing config from file " <> filePath
      decodeConfigFile filePath

-- | Encode a value to its JSON string representation, stripping the outer quotes.
stringFromJSON :: (ToJSON a) => a -> String
stringFromJSON a =
  let raw = BL8.unpack (Aeson.encode a)
   in case raw of
        ('"' : rest) | not (null rest) -> Prelude.init rest
        other -> other

-- | Build 'SchemaOptions' that strip a given prefix and convert to snake_case.
-- Eliminates the per-type boilerplate in 'ToSchema' instances.
--
-- @
-- instance ToSchema MyRequest where
--   declareNamedSchema = genericDeclareNamedSchema (mkStripPrefixSchemaOptions "myReq")
-- @
mkStripPrefixSchemaOptions :: String -> SchemaOptions
mkStripPrefixSchemaOptions prefix =
  fromAesonOptions $
    AesonTypes.defaultOptions
      { AesonTypes.fieldLabelModifier = AesonTypes.camelTo2 '_' . dropPrefix prefix
      }
