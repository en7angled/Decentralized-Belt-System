module TxBuilding.Interactions where

import Control.Monad.Reader.Class
import GeniusYield.Imports hiding (fromMaybe)
import GeniusYield.Types
import Onchain.CIP68 (MetadataFields)
import Onchain.Types (Profile)
import TxBuilding.Context
import TxBuilding.Transactions
import GeniusYield.TxBuilder.Query.Class (GYTxUserQueryMonad)
import GeniusYield.TxBuilder.Class (GYTxMonad)

------------------------------------------------------------------------------------------------

-- * High-Level Interaction Functions

------------------------------------------------------------------------------------------------

-- | Create a new profile with metadata
createProfile ::
  (GYTxUserQueryMonad m, MonadReader ProfileTxBuildingContext m, GYTxMonad m) =>
  GYAddress ->
  MetadataFields ->
  Profile ->
  m GYTxId
createProfile recipient metadata profile = do
  executeCreateProfile recipient metadata profile

-- | Update an existing profile's metadata
updateProfile ::
  (GYTxUserQueryMonad m, MonadReader ProfileTxBuildingContext m, GYTxMonad m) =>
  GYAddress ->
  MetadataFields ->
  GYAssetClass ->
  m GYTxId
updateProfile recipient newMetadata profileRefAC = do
  executeUpdateProfile recipient newMetadata profileRefAC

-- | Delete a profile and recover collateral
deleteProfile ::
  (GYTxUserQueryMonad m, MonadReader ProfileTxBuildingContext m, GYTxMonad m) =>
  GYAddress ->
  GYAssetClass ->
  m GYTxId
deleteProfile recipient profileRefAC = do
  executeDeleteProfile recipient profileRefAC

------------------------------------------------------------------------------------------------

-- * Batch Operations

------------------------------------------------------------------------------------------------

-- | Create multiple profiles in a single transaction
createMultipleProfiles ::
  (GYTxUserQueryMonad m, MonadReader ProfileTxBuildingContext m) =>
  GYAddress ->
  [(MetadataFields, Profile)] ->
  m GYTxId
createMultipleProfiles recipient profiles = do
  -- TODO: Implement batch creation
  -- This would require combining multiple createProfileTX skeletons
  error "Batch profile creation not yet implemented"

-- | Update multiple profiles in a single transaction
updateMultipleProfiles ::
  (GYTxUserQueryMonad m, MonadReader ProfileTxBuildingContext m) =>
  GYAddress ->
  [(GYAssetClass, MetadataFields)] ->
  m GYTxId
updateMultipleProfiles recipient profileUpdates = do
  -- TODO: Implement batch updates
  -- This would require combining multiple updateProfileTX skeletons
  error "Batch profile updates not yet implemented" 