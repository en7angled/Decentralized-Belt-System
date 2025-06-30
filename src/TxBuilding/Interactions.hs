{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module TxBuilding.Interactions where

import Control.Monad.Reader.Class (MonadReader)
import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.Swagger.Internal.Schema (ToSchema)
import DomainTypes.Profile.Types (ProfileActionType (..))
import GHC.Generics (Generic)
import GeniusYield.TxBuilder
import GeniusYield.Types
import TxBuilding.Context (ProfileTxBuildingContext)
import TxBuilding.Functors
import TxBuilding.Operations

------------------------------------------------------------------------------------------------

-- * Types

------------------------------------------------------------------------------------------------

data UserAddresses = UserAddresses
  { -- | User's used addresses.
    usedAddresses :: [GYAddress],
    -- | User's change address.
    changeAddress :: GYAddress,
    -- | Browser wallet's reserved collateral (if set).
    reservedCollateral :: Maybe GYTxOutRefCbor
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data ActionType = ProfileAction ProfileActionType
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data Interaction
  = Interaction
  { -- | The intented action to perfrom.
    action :: ActionType,
    -- | The user addresses to be used as input for transaction building.
    userAddresses :: UserAddresses,
    -- | If the interaction unlocks some funds, the funds will be sent to this address (if set, otherwise to the change address).
    recipient :: Maybe GYAddress
  }
  deriving (Show, Generic, FromJSON, ToJSON)

interactionToTxSkeleton ::
  (GYTxUserQueryMonad m, MonadReader ProfileTxBuildingContext m) =>
  Interaction ->
  m (GYTxSkeleton 'PlutusV3, GYAssetClass)
interactionToTxSkeleton Interaction {..} = do
  let changeAddr = changeAddress userAddresses
  let usedAddrs = usedAddresses userAddresses
  let receiveAddr = fromMaybe changeAddr recipient
  case action of
    ProfileAction actionType -> do
      case actionType of
        CreateProfileAction profileData profileType creationDate -> do
          createProfileTX
            receiveAddr
            (profileDataToMetadataFields profileData)
            (profileTypeToOnChainProfileType profileType)
            (toPlutusPOSIXTime creationDate)
        UpdateProfileImageAction profileRefAC imgURI -> do
          (,profileRefAC)
            <$> updateProfileTX
              profileRefAC
              (textToBuiltinByteString imgURI)
              usedAddrs
        DeleteProfileAction profileRefAC -> do
          (,profileRefAC)
            <$> deleteProfileTX profileRefAC receiveAddr

------------------------------------------------------------------------------------------------

-- * High-Level Interaction Functions

------------------------------------------------------------------------------------------------

-- -- | Create a new profile with metadata
-- createProfile ::
--   (GYTxUserQueryMonad m, MonadReader ProfileTxBuildingContext m, GYTxMonad m) =>
--   GYAddress ->
--   MetadataFields ->
--   Profile ->
--   m GYTxId
-- createProfile recipient metadata profile = do
--   executeCreateProfile recipient metadata profile

-- -- | Update an existing profile's metadata
-- updateProfile ::
--   (GYTxUserQueryMonad m, MonadReader ProfileTxBuildingContext m, GYTxMonad m) =>
--   MetadataFields ->
--   GYAssetClass ->
--   m GYTxId
-- updateProfile newMetadata profileRefAC = do
--   executeUpdateProfile newMetadata profileRefAC

-- -- | Delete a profile and recover collateral
-- deleteProfile ::
--   (GYTxUserQueryMonad m, MonadReader ProfileTxBuildingContext m, GYTxMonad m) =>
--   GYAddress ->
--   GYAssetClass ->
--   m GYTxId
-- deleteProfile recipient profileRefAC = do
--   executeDeleteProfile recipient profileRefAC

-- ------------------------------------------------------------------------------------------------

-- -- * Batch Operations

-- ------------------------------------------------------------------------------------------------

-- -- | Create multiple profiles in a single transaction
-- createMultipleProfiles ::
--   (GYTxUserQueryMonad m, MonadReader ProfileTxBuildingContext m) =>
--   GYAddress ->
--   [(MetadataFields, Profile)] ->
--   m GYTxId
-- createMultipleProfiles recipient profiles = do
--   -- TODO: Implement batch creation
--   -- This would require combining multiple createProfileTX skeletons
--   error "Batch profile creation not yet implemented"

-- -- | Update multiple profiles in a single transaction
-- updateMultipleProfiles ::
--   (GYTxUserQueryMonad m, MonadReader ProfileTxBuildingContext m) =>
--   GYAddress ->
--   [(GYAssetClass, MetadataFields)] ->
--   m GYTxId
-- updateMultipleProfiles recipient profileUpdates = do
--   -- TODO: Implement batch updates
--   -- This would require combining multiple updateProfileTX skeletons
--   error "Batch profile updates not yet implemented"