module TxBuilding.Interactions where

import Control.Monad.Reader.Class (MonadReader)
import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.Swagger.Internal.Schema (ToSchema)
import DomainTypes.Profile.Types
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
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

newtype ActionType = ProfileAction ProfileActionType
  deriving (Show, Generic)
  deriving newtype (FromJSON, ToJSON, ToSchema)

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
  (HasCallStack, GYTxUserQueryMonad m, MonadReader ProfileTxBuildingContext m) =>
  Interaction ->
  m (GYTxSkeleton 'PlutusV3, GYAssetClass)
interactionToTxSkeleton Interaction {..} = do
  let changeAddr = changeAddress userAddresses
  let usedAddrs = usedAddresses userAddresses
  let receiveAddr = fromMaybe changeAddr recipient
  case action of
    ProfileAction actionType -> do
      case actionType of
        CreateProfileWithRankAction profileData profileType creationDate belt -> do
          createProfileWithRankTX
            receiveAddr
            (profileDataToMetadataFields profileData)
            (profileTypeToOnChainProfileType profileType)
            (timeToPlutus creationDate)
            belt
        InitProfileAction profileData profileType creationDate -> do
          createProfileTX
            receiveAddr
            (profileDataToMetadataFields profileData)
            (profileTypeToOnChainProfileType profileType)
            (timeToPlutus creationDate)
        UpdateProfileImageAction profileRefAC imgURI -> do
          (,profileRefAC)
            <$> updateProfileTX
              profileRefAC
              (textToBuiltinByteString imgURI)
              usedAddrs
        DeleteProfileAction profileRefAC -> do
          (,profileRefAC)
            <$> deleteProfileTX profileRefAC receiveAddr usedAddrs
        PromoteProfileAction promotedProfileId promotedByProfileId achievementDate belt ->
          promoteProfileTX promotedProfileId promotedByProfileId (timeToPlutus achievementDate) belt usedAddrs
        AcceptPromotionAction promotionId ->
          acceptPromotionTX promotionId usedAddrs
