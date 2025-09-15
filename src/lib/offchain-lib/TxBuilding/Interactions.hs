module TxBuilding.Interactions where

import Control.Monad.Reader.Class (MonadReader)
import Data.Aeson
import Data.Aeson.Types qualified as AesonTypes
import Data.List.Extra
import Data.Maybe (fromMaybe)
import Data.Swagger (ToSchema (..), genericDeclareNamedSchema)
import Data.Swagger.Schema (SchemaOptions, fromAesonOptions)
import Deriving.Aeson
import DomainTypes.Core.Actions
import GHC.Stack (HasCallStack)
import GeniusYield.TxBuilder
import GeniusYield.Types
import TxBuilding.Context (DeployedScriptsContext)
import TxBuilding.Functors
import TxBuilding.Operations

------------------------------------------------------------------------------------------------

-- * Types

------------------------------------------------------------------------------------------------

-- | Input parameters to add for reference script.
data AddWitAndSubmitParams = AddWitAndSubmitParams
  { awasTxUnsigned :: !GYTx,
    awasTxWit :: !GYTxWitness
  }
  deriving (Generic)
  deriving (FromJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "awas", CamelToSnake]] AddWitAndSubmitParams

instance ToSchema AddWitAndSubmitParams where
  declareNamedSchema = genericDeclareNamedSchema addWitAndSubmitParamsSchemaOptions
    where
      addWitAndSubmitParamsSchemaOptions :: SchemaOptions
      addWitAndSubmitParamsSchemaOptions =
        fromAesonOptions $
          AesonTypes.defaultOptions
            { AesonTypes.fieldLabelModifier = camelTo2 '_' . dropPrefix "awas"
            }

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
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

interactionToTxSkeleton ::
  (HasCallStack, GYTxUserQueryMonad m, MonadReader DeployedScriptsContext m) =>
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
