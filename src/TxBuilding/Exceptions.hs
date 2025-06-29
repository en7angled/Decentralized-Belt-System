module TxBuilding.Exceptions where

import Control.Exception
import GHC.Generics (Generic)
import Prelude qualified
import GeniusYield.HTTP.Errors (IsGYApiError)

------------------------------------------------------------------------------------------------

-- * Custom Exceptions

------------------------------------------------------------------------------------------------

-- | Custom exception for profile operations
data ProfileException
  = ProfileNotFound
  | ProfileAlreadyExists
  | InvalidProfileData
  | InsufficientFunds
  | InvalidMetadata
  deriving stock (Generic, Prelude.Show, Prelude.Eq)

instance Exception ProfileException where
  displayException ProfileNotFound = "Profile not found"
  displayException ProfileAlreadyExists = "Profile already exists"
  displayException InvalidProfileData = "Invalid profile data"
  displayException InsufficientFunds = "Insufficient funds"
  displayException InvalidMetadata = "Invalid metadata"

instance IsGYApiError ProfileException 