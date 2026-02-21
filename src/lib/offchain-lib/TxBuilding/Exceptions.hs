module TxBuilding.Exceptions where

import Control.Exception
import GHC.Generics (Generic)
import GeniusYield.HTTP.Errors (IsGYApiError)
import Prelude

------------------------------------------------------------------------------------------------

-- * Add membership interval validation reasons

------------------------------------------------------------------------------------------------

-- | Reason for rejecting an add-membership-interval request (mirrors on-chain validation).
data AddMembershipIntervalReason
  = LastIntervalNotAccepted -- practitioner must accept current interval first
  | LastIntervalNotClosed -- current interval has no end date or new start < its end
  | InvalidNewIntervalEndDate -- end date not after start date (TC)
  | HeadNumberMismatch -- history head number != last interval number
  deriving stock (Generic, Show, Eq)

------------------------------------------------------------------------------------------------

-- * Offchain Transaction Building Exceptions

------------------------------------------------------------------------------------------------

-- | Exceptions raised during offchain transaction building and lookups.
data TxBuildingException
  = -- * Profile Lookup Errors
    ProfileNotFound
  | WrongProfileType
    -- * Rank Lookup Errors
  | RankNotFound
  | RankListEmpty
  | WrongRankDataType
    -- * Promotion Errors
  | PromotionNotFound
    -- * Membership Errors
  | MembershipHistoryNotFound
  | MembershipIntervalNotFound
  | MembershipListNodeNotFound
  | MembershipRootNodeHasNoHistory
  | CannotAddMembershipInterval AddMembershipIntervalReason
    -- * Achievement Errors
  | AchievementNotFound
    -- * Oracle Errors
  | OracleNotFound
  | OracleDatumInvalid
  | ProtocolPaused
    -- * Script / Infrastructure Errors
  | ScriptNotFound
  | DeployedScriptsNotReady
    -- * UTxO / Asset Errors
  | InvalidAssetClass
  | NFTNotFound
  | MultipleUtxosFound
  | DatumParseError
    -- * Cleanup Errors
  | NoDustFound
  deriving stock (Generic, Show, Eq)

instance Exception TxBuildingException where
  displayException :: TxBuildingException -> String
  displayException ProfileNotFound = "Profile not found"
  displayException WrongProfileType = "Wrong profile type"
  displayException RankNotFound = "Rank not found"
  displayException RankListEmpty = "Rank list is empty for this practitioner"
  displayException WrongRankDataType = "Wrong rank data type"
  displayException PromotionNotFound = "Promotion not found"
  displayException MembershipHistoryNotFound = "Membership history not found"
  displayException MembershipIntervalNotFound = "Membership interval not found"
  displayException MembershipListNodeNotFound = "Membership list node not found"
  displayException MembershipRootNodeHasNoHistory = "Membership list node is the root; it has no history or first interval"
  displayException (CannotAddMembershipInterval reason) = addMembershipIntervalReasonMessage reason
  displayException AchievementNotFound = "Achievement not found"
  displayException OracleNotFound = "Oracle UTxO not found"
  displayException OracleDatumInvalid = "Oracle datum invalid or unparseable"
  displayException ProtocolPaused = "Protocol is paused"
  displayException ScriptNotFound = "Script not found"
  displayException DeployedScriptsNotReady = "Deployed scripts are not ready"
  displayException InvalidAssetClass = "Invalid asset class"
  displayException NFTNotFound = "UTxO with this NFT not found"
  displayException MultipleUtxosFound = "Multiple UTxOs found for this asset class"
  displayException DatumParseError = "UTxO datum is missing or unparseable"
  displayException NoDustFound = "No dust UTxOs found at validator addresses"

addMembershipIntervalReasonMessage :: AddMembershipIntervalReason -> String
addMembershipIntervalReasonMessage LastIntervalNotAccepted =
  "Cannot add membership interval: the current last interval must be accepted by the practitioner first"
addMembershipIntervalReasonMessage LastIntervalNotClosed =
  "Cannot add membership interval: the current last interval must be closed (have an end date) and the new start date must be after it"
addMembershipIntervalReasonMessage InvalidNewIntervalEndDate =
  "Cannot add membership interval: end date must be after start date"
addMembershipIntervalReasonMessage HeadNumberMismatch =
  "Cannot add membership interval: history head number does not match last interval number"

instance IsGYApiError TxBuildingException

------------------------------------------------------------------------------------------------

-- * HTTP Status Mapping

------------------------------------------------------------------------------------------------

-- | Map a 'TxBuildingException' to the appropriate HTTP status code.
-- Used by the interaction API to return structured error responses.
txBuildingExceptionToHttpStatus :: TxBuildingException -> Int
txBuildingExceptionToHttpStatus ProfileNotFound = 404
txBuildingExceptionToHttpStatus RankNotFound = 404
txBuildingExceptionToHttpStatus RankListEmpty = 404
txBuildingExceptionToHttpStatus PromotionNotFound = 404
txBuildingExceptionToHttpStatus MembershipHistoryNotFound = 404
txBuildingExceptionToHttpStatus MembershipIntervalNotFound = 404
txBuildingExceptionToHttpStatus MembershipListNodeNotFound = 404
txBuildingExceptionToHttpStatus MembershipRootNodeHasNoHistory = 404
txBuildingExceptionToHttpStatus (CannotAddMembershipInterval _) = 400
txBuildingExceptionToHttpStatus AchievementNotFound = 404
txBuildingExceptionToHttpStatus OracleNotFound = 404
txBuildingExceptionToHttpStatus DeployedScriptsNotReady = 503
txBuildingExceptionToHttpStatus ProtocolPaused = 503
txBuildingExceptionToHttpStatus NoDustFound = 404
txBuildingExceptionToHttpStatus NFTNotFound = 404
txBuildingExceptionToHttpStatus _ = 400
