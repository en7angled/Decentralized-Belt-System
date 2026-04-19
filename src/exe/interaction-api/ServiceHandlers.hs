


-- | Service-layer handlers for the interaction API's dedicated endpoints.
-- Each handler resolves a service request into an 'Interaction' and delegates
-- to the existing tx-building pipeline via 'buildInteractionApp'.
module ServiceHandlers where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.List (find)
import Data.Text (Text, unpack)
import DomainTypes.Core.Actions
import DomainTypes.Core.Types (MembershipHistory (..))
import GeniusYield.Types (GYAddress)
import IPFS (IPFSConfig, uploadToIPFS)
import InteractionAppMonad
import Servant (ServerError (..), err502)
import Servant.Multipart (FileData (..))
import ServiceRequests
import TxBuilding.Context (TxBuildingContext (..), getNetworkId, runQuery)
import TxBuilding.Interactions (ActionType (..), Interaction (..), UserAddresses)
import TxBuilding.Lookups (getMembershipHistoriesForOrganization)

-------------------------------------------------------------------------------

-- * Helpers

-------------------------------------------------------------------------------

-- | Upload image bytes to IPFS or throw a 502 (Bad Gateway) error.
uploadOrThrow :: IPFSConfig -> LBS.ByteString -> InteractionAppMonad Text
uploadOrThrow cfg bytes = do
  result <- liftIO $ uploadToIPFS cfg bytes
  case result of
    Left err -> InteractionAppMonad $ throwError err502 { errBody = BL8.pack (unpack err) }
    Right uri -> return uri

-- | Build an 'Interaction' from a profile action, user addresses, and optional recipient.
mkInteraction :: ProfileActionType -> UserAddresses -> Maybe GYAddress -> Interaction
mkInteraction actionType = Interaction (ProfileAction actionType)

-------------------------------------------------------------------------------

-- * Smart handlers (IPFS upload / chain queries)

-------------------------------------------------------------------------------

-- | Create profile: upload image to IPFS, then build InitProfileAction or
-- CreateProfileWithRankAction depending on whether a belt was provided.
handleCreateProfile :: WithImage CreateProfileRequest -> InteractionAppMonad String
handleCreateProfile (WithImage imageFile req) = do
  ipfsCfg <- asks ipfsConfig
  ipfsUri <- uploadOrThrow ipfsCfg (fdPayload imageFile)
  let pd = ProfileData (cprName req) (cprDescription req) ipfsUri
  let action = case cprBelt req of
        Nothing -> InitProfileAction pd (cprProfileType req) (cprCreationDate req)
        Just belt -> CreateProfileWithRankAction pd (cprProfileType req) (cprCreationDate req) belt
  buildInteractionApp $ mkInteraction action (cprUserAddresses req) (cprRecipient req)

-- | New membership: query the chain for an existing membership history between
-- the org and practitioner. If none exists, create one; otherwise add an interval.
handleNewMembership :: NewMembershipRequest -> InteractionAppMonad String
handleNewMembership req = do
  txCtx <- asks txBuildingContext
  let pCtx = providerCtx txCtx
      nid = getNetworkId pCtx
  existingHistory <- runWithTxErrorHandling $
    runQuery pCtx $ do
      histories <- getMembershipHistoriesForOrganization nid (nmrOrganizationProfileId req)
      return $ find (\h -> membershipHistoryPractitionerId h == nmrPractitionerProfileId req) histories
  let action = case existingHistory of
        Nothing ->
          CreateMembershipHistoryAction
            (nmrOrganizationProfileId req)
            (nmrPractitionerProfileId req)
            (nmrStartDate req)
            (nmrEndDate req)
        Just history ->
          AddMembershipIntervalAction
            (nmrOrganizationProfileId req)
            (membershipHistoryId history)
            (nmrStartDate req)
            (nmrEndDate req)
  buildInteractionApp $ mkInteraction action (nmrUserAddresses req) (nmrRecipient req)

-- | Update profile: upload new image to IPFS, then build UpdateProfileAction.
handleUpdateProfile :: WithImage UpdateProfileRequest -> InteractionAppMonad String
handleUpdateProfile (WithImage imageFile req) = do
  ipfsCfg <- asks ipfsConfig
  ipfsUri <- uploadOrThrow ipfsCfg (fdPayload imageFile)
  let action = UpdateProfileAction (uprProfileId req) (uprDescription req) ipfsUri
  buildInteractionApp $ mkInteraction action (uprUserAddresses req) (uprRecipient req)

-- | Award achievement: upload image to IPFS, then build AwardAchievementAction.
handleAwardAchievement :: WithImage AwardAchievementRequest -> InteractionAppMonad String
handleAwardAchievement (WithImage imageFile req) = do
  ipfsCfg <- asks ipfsConfig
  ipfsUri <- uploadOrThrow ipfsCfg (fdPayload imageFile)
  let pd = ProfileData (aarName req) (aarDescription req) ipfsUri
  let action = AwardAchievementAction
        (aarAwardedToProfileId req)
        (aarAwardedByProfileId req)
        pd
        (aarOtherMetadata req)
        (aarAchievementDate req)
  buildInteractionApp $ mkInteraction action (aarUserAddresses req) (aarRecipient req)

-------------------------------------------------------------------------------

-- * Pass-through handlers (thin wrappers)

-------------------------------------------------------------------------------

handlePromoteProfile :: PromoteProfileRequest -> InteractionAppMonad String
handlePromoteProfile req = buildInteractionApp $ mkInteraction
  (PromoteProfileAction
    (pprPromotedProfileId req)
    (pprPromotedByProfileId req)
    (pprAchievementDate req)
    (pprPromotedBelt req))
  (pprUserAddresses req)
  (pprRecipient req)

handleAcceptPromotion :: AcceptPromotionRequest -> InteractionAppMonad String
handleAcceptPromotion req = buildInteractionApp $ mkInteraction
  (AcceptPromotionAction (aprPromotionId req))
  (aprUserAddresses req)
  (aprRecipient req)

handleAcceptMembership :: AcceptMembershipRequest -> InteractionAppMonad String
handleAcceptMembership req = buildInteractionApp $ mkInteraction
  (AcceptMembershipIntervalAction (amrIntervalId req))
  (amrUserAddresses req)
  (amrRecipient req)

handleUpdateEndDate :: UpdateEndDateRequest -> InteractionAppMonad String
handleUpdateEndDate req = buildInteractionApp $ mkInteraction
  (UpdateEndDateAction
    (uerIntervalId req)
    (uerHistoryNodeId req)
    (uerNewEndDate req))
  (uerUserAddresses req)
  (uerRecipient req)

handleAcceptAchievement :: AcceptAchievementRequest -> InteractionAppMonad String
handleAcceptAchievement req = buildInteractionApp $ mkInteraction
  (AcceptAchievementAction (acrAchievementId req))
  (acrUserAddresses req)
  (acrRecipient req)
