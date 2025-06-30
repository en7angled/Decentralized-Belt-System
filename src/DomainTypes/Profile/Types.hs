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

data ProfileData
  = ProfileData
  { profileName :: Text,
    profileDescription :: Text,
    profileImageURI :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data Belt = White | Blue | Purple | Brown | Black | RedAndBlack | RedAndWhite | Red
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

newtype Stripe = Stripe Int
  deriving (Show, Generic)
  deriving newtype (FromJSON, ToJSON, ToSchema)

data ProfileType = Practitioner | Organization
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

type ProfileRefAC = GYAssetClass

data BJJRank = BJJRank Belt Stripe
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data BJJProfile = BJJProfile
  { profileData :: ProfileData,
    profileRank :: Maybe BJJRank,
    profileId :: ProfileRefAC
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

type POSIXTimeInteger = Integer -- POSIXTime in milliseconds

data ProfileActionType
  = CreateProfileAction ProfileData ProfileType POSIXTimeInteger
  | UpdateProfileAction ProfileRefAC ProfileData
  | DeleteProfileAction ProfileRefAC
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)
