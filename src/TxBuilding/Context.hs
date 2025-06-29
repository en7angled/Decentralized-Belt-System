module TxBuilding.Context where

import GeniusYield.Types
import GHC.Generics (Generic)
import Prelude qualified

------------------------------------------------------------------------------------------------

-- * Profile Transaction Building Context

------------------------------------------------------------------------------------------------

-- | Context for profile transaction building operations
data ProfileTxBuildingContext = ProfileTxBuildingContext
  { profilesValidatorRef :: GYTxOutRef
  }
  deriving stock (Generic, Prelude.Show)

------------------------------------------------------------------------------------------------

-- * Default Context

------------------------------------------------------------------------------------------------

-- | Default context for testing and development
defaultProfileTxBuildingContext :: ProfileTxBuildingContext
defaultProfileTxBuildingContext = ProfileTxBuildingContext
  { profilesValidatorRef = Prelude.undefined -- TODO: Set proper default value when needed
  } 