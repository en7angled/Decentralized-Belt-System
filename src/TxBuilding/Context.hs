module TxBuilding.Context where

import GHC.Generics (Generic)
import GeniusYield.Types
import Prelude qualified

------------------------------------------------------------------------------------------------

-- * Profile Transaction Building Context

------------------------------------------------------------------------------------------------

-- | Context for profile transaction building operations
data ProfileTxBuildingContext = ProfileTxBuildingContext
  { mintingPolicyRef :: GYTxOutRef,
    profilesValidatorRef :: GYTxOutRef,
    prmotionsValidatorRef :: GYTxOutRef,
    ranksValidatorRef :: GYTxOutRef
  }
  deriving stock (Generic, Prelude.Show)

------------------------------------------------------------------------------------------------

-- * Default Context

------------------------------------------------------------------------------------------------

-- | Default context for testing and development
defaultProfileTxBuildingContext :: ProfileTxBuildingContext
defaultProfileTxBuildingContext =
  ProfileTxBuildingContext
    { mintingPolicyRef = Prelude.undefined, -- TODO: Set proper default value when needed
      profilesValidatorRef = Prelude.undefined, -- TODO: Set proper default value when needed
      prmotionsValidatorRef = Prelude.undefined, -- TODO: Set proper default value when needed
      ranksValidatorRef = Prelude.undefined -- TODO: Set proper default value when needed
    }