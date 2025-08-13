module Utils where
import Data.Aeson
import System.Directory.Extra
import System.Environment
import qualified Data.ByteString.Char8 as BS8
import Prelude

greenColorString :: String -> String
greenColorString s =
  "\n"
    ++ "\ESC[1;32m"
    ++ s
    ++ "\ESC[0m"
    ++ "\n"

orangeColorString :: String -> String
orangeColorString s =
  "\n"
    ++ "\ESC[1;33m"
    ++ s
    ++ "\ESC[0m"
    ++ "\n"


yellowColorString :: String -> String
yellowColorString s =
  "\n"
    ++ "\ESC[1;93m"
    ++ s
    ++ "\ESC[0m"
    ++ "\n"

blueColorString :: String -> String
blueColorString s =
  "\n"
    ++ "\ESC[1;94m"
    ++ s
    ++ "\ESC[0m"
    ++ "\n"

purpleColorString :: String -> String
purpleColorString s =
  "\n"
    ++ "\ESC[1;95m"
    ++ s
    ++ "\ESC[0m"
    ++ "\n"




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

