{-# OPTIONS_GHC -Wno-partial-fields #-}

module DomainTypes.Profile.Types where

import Data.Aeson
import Data.Swagger.Internal.Schema (ToSchema)
import Data.Text
import GHC.Generics
import GeniusYield.Types (GYAssetClass)

-------------------------------------------------------------------------------

-- * Profile

-------------------------------------------------------------------------------
type POSIXTimeInteger = Integer -- POSIXTime in milliseconds

data ProfileData
  = ProfileData
  { profileName :: Text
  , profileDescription :: Text
  , profileImageURI :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data ProfileType = Practitioner | Organization
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

type ProfileRefAC = GYAssetClass

data ProfileActionType
  = CreateProfileAction ProfileData ProfileType POSIXTimeInteger
  | UpdateProfileImageAction ProfileRefAC Text
  | DeleteProfileAction ProfileRefAC
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)
