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
  | InvalidAssetClass 
  | RankNotFound
  | WrongProfileType
  | WrongRankDataType
  deriving stock (Generic, Prelude.Show, Prelude.Eq)

instance Exception ProfileException where
  displayException :: ProfileException -> Prelude.String
  displayException ProfileNotFound = "Profile not found"
  displayException WrongProfileType = "Wrong profile type"  
  displayException ProfileAlreadyExists = "Profile already exists"
  displayException InvalidProfileData = "Invalid profile data"
  displayException InsufficientFunds = "Insufficient funds"
  displayException InvalidMetadata = "Invalid metadata"
  displayException InvalidAssetClass = "Invalid asset class"
  displayException RankNotFound = "Rank not found"
  displayException WrongRankDataType = "Wrong rank data type"
instance IsGYApiError ProfileException 