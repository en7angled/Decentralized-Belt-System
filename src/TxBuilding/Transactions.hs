module TxBuilding.Transactions where

import GeniusYield.TxBuilder
import GeniusYield.Types

------------------------------------------------------------------------------------------------

-- * Transaction Execution Functions

------------------------------------------------------------------------------------------------

-- -- | Execute create profile transaction
-- executeCreateProfile ::
--   (GYTxUserQueryMonad m, MonadReader ProfileTxBuildingContext m, GYTxMonad m) =>
--   GYAddress ->
--   MetadataFields ->
--   OnChainProfileData ->
--   m GYTxId
-- executeCreateProfile recipient metadata profile = do
--   skeleton <- createProfileTX recipient metadata profile
--   buildTx skeleton

-- -- | Execute update profile transaction
-- executeUpdateProfile ::
--   (GYTxUserQueryMonad m, MonadReader ProfileTxBuildingContext m, GYTxMonad m) =>
--   MetadataFields ->
--   GYAssetClass ->
--   m GYTxId
-- executeUpdateProfile  newMetadata profileRefAC = do
--   skeleton <- updateProfileTX  newMetadata profileRefAC
--   buildTx skeleton

-- -- | Execute delete profile transaction
-- executeDeleteProfile ::
--   (GYTxUserQueryMonad m, MonadReader ProfileTxBuildingContext m, GYTxMonad m) =>
--   GYAddress ->
--   GYAssetClass ->
--   m GYTxId
-- executeDeleteProfile recipient profileRefAC = do
--   skeleton <- deleteProfileTX recipient profileRefAC
--   buildTx skeleton

------------------------------------------------------------------------------------------------

-- * Helper Functions

------------------------------------------------------------------------------------------------

-- | Build and submit transaction
buildTx :: (GYTxMonad m) => GYTxSkeleton 'PlutusV3 -> m GYTxId
buildTx skeleton = do
  txBody <- buildTxBody skeleton
  tx <- signTxBody txBody
  submitTx tx