{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Off-chain instances and helpers for 'BJJBelt'.
-- Re-exports the on-chain type and adds JSON, Swagger, Servant, and
-- string-parsing support needed by the API layer.
module DomainTypes.Core.BJJ (module Onchain.BJJ, parseBelt) where

import Data.Aeson.Types (FromJSON, ToJSON)
import Data.String (IsString (..))
import Data.Swagger (ToParamSchema, ToSchema)
import Data.Text (Text)
import Data.Text qualified as T
import Onchain.BJJ
import Servant (FromHttpApiData (..))

deriving anyclass instance FromJSON BJJBelt

deriving anyclass instance ToJSON BJJBelt

deriving anyclass instance ToSchema BJJBelt

deriving anyclass instance ToParamSchema BJJBelt

instance IsString BJJBelt where
  fromString :: String -> BJJBelt
  fromString s = case parseBelt s of
    Just belt -> belt
    Nothing -> error "Invalid belt"

parseBelt :: String -> Maybe BJJBelt
parseBelt s = case s of
  "White" -> Just White
  "Blue" -> Just Blue
  "Purple" -> Just Purple
  "Brown" -> Just Brown
  "Black" -> Just Black
  "Black1" -> Just Black1
  "Black2" -> Just Black2
  "Black3" -> Just Black3
  "Black4" -> Just Black4
  "Black5" -> Just Black5
  "Black6" -> Just Black6
  "RedAndBlack" -> Just RedAndBlack
  "RedAndWhite" -> Just RedAndWhite
  "Red" -> Just Red
  "Red10" -> Just Red10
  _ -> Nothing

instance FromHttpApiData BJJBelt where
  parseQueryParam :: Text -> Either Text BJJBelt
  parseQueryParam = maybe (Left "Invalid belt") Right . parseBelt . T.unpack
