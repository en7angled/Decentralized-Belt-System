{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Minimal IPFS HTTP API client for uploading images.
-- Uses raw http-client calls to POST to the IPFS node's @/api/v0/add@ endpoint.
module IPFS
  ( IPFSConfig (..)
  , initIPFSConfig
  , uploadToIPFS
  ) where

import Control.Exception (SomeException)
import Control.Exception qualified
import Data.Aeson (eitherDecode, (.:))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.List (dropWhileEnd)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Network.HTTP.Client
  ( Manager
  , httpLbs
  , method
  , newManager
  , parseRequest
  , responseBody
  , responseStatus
  )
import Network.HTTP.Client.MultipartFormData (formDataBody, partLBS)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusIsSuccessful)
import System.Environment (lookupEnv)

-- | Configuration for connecting to an IPFS node's HTTP API.
data IPFSConfig = IPFSConfig
  { ipfsApiUrl :: String
  -- ^ Base URL of the IPFS HTTP API (e.g. @http://localhost:5001@), without trailing slash
  , ipfsManager :: Manager
  -- ^ Shared HTTP manager (created once at startup for connection pooling)
  }

-- | Initialise IPFS configuration from environment variables.
-- Reads @IPFS_API_URL@ (default: @http://localhost:5001@) and creates a
-- shared 'Manager' for connection pooling across requests.
initIPFSConfig :: IO IPFSConfig
initIPFSConfig = do
  mUrl <- lookupEnv "IPFS_API_URL"
  let rawUrl = fromMaybe "http://localhost:5001" mUrl
  let url = dropWhileEnd (== '/') rawUrl
  mgr <- newManager tlsManagerSettings
  return IPFSConfig { ipfsApiUrl = url, ipfsManager = mgr }

-- | IPFS @/api/v0/add@ response (subset of fields we care about).
newtype AddResponse = AddResponse
  { addResponseHash :: Text
  }

instance Aeson.FromJSON AddResponse where
  parseJSON = Aeson.withObject "AddResponse" $ \o ->
    AddResponse <$> o .: "Hash"

-- | Upload raw bytes to IPFS via the HTTP API's @/api/v0/add@ endpoint.
-- Returns @Right "ipfs://<cid>"@ on success or @Left errorMessage@ on failure.
uploadToIPFS :: IPFSConfig -> LBS.ByteString -> IO (Either Text Text)
uploadToIPFS IPFSConfig{..} imageBytes = do
  let url = ipfsApiUrl <> "/api/v0/add"
  result <- Control.Exception.try $ do
    initialReq <- parseRequest url
    let req = initialReq { method = "POST" }
    multipartReq <- formDataBody [partLBS "file" imageBytes] req
    response <- httpLbs multipartReq ipfsManager
    if statusIsSuccessful (responseStatus response)
      then case eitherDecode (responseBody response) of
        Left err -> return $ Left $ "Failed to parse IPFS response: " <> pack err
        Right AddResponse{..} -> return $ Right $ "ipfs://" <> addResponseHash
      else return $ Left $ "IPFS API returned error status: " <> pack (show (responseStatus response))
  case result of
    Left (ex :: SomeException) -> return $ Left $ "IPFS service unavailable: " <> pack (show ex)
    Right r -> return r
