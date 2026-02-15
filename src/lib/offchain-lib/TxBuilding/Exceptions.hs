module TxBuilding.Exceptions where

import Control.Exception
import GHC.Generics (Generic)
import GeniusYield.HTTP.Errors (IsGYApiError)
import Prelude qualified

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
    -- * Oracle Errors
  | OracleNotFound
  | OracleDatumInvalid
  | ProtocolPaused
    -- * Script / Infrastructure Errors
  | ScriptNotFound
  | DeployedScriptsNotReady
    -- * UTxO / Asset Errors
  | InvalidAssetClass
  | MultipleUtxosFound
  | DatumParseError
  deriving stock (Generic, Prelude.Show, Prelude.Eq)

instance Exception TxBuildingException where
  displayException :: TxBuildingException -> Prelude.String
  displayException ProfileNotFound = "Profile not found"
  displayException WrongProfileType = "Wrong profile type"
  displayException RankNotFound = "Rank not found"
  displayException RankListEmpty = "Rank list is empty for this practitioner"
  displayException WrongRankDataType = "Wrong rank data type"
  displayException PromotionNotFound = "Promotion not found"
  displayException OracleNotFound = "Oracle UTxO not found"
  displayException OracleDatumInvalid = "Oracle datum invalid or unparseable"
  displayException ProtocolPaused = "Protocol is paused"
  displayException ScriptNotFound = "Script not found"
  displayException DeployedScriptsNotReady = "Deployed scripts are not ready"
  displayException InvalidAssetClass = "Invalid asset class"
  displayException MultipleUtxosFound = "Multiple UTxOs found for this asset class"
  displayException DatumParseError = "UTxO datum is missing or unparseable"

instance IsGYApiError TxBuildingException

------------------------------------------------------------------------------------------------

-- * HTTP Status Mapping

------------------------------------------------------------------------------------------------

-- | Map a 'TxBuildingException' to the appropriate HTTP status code.
-- Used by the interaction API to return structured error responses.
txBuildingExceptionToHttpStatus :: TxBuildingException -> Prelude.Int
txBuildingExceptionToHttpStatus ProfileNotFound = 404
txBuildingExceptionToHttpStatus RankNotFound = 404
txBuildingExceptionToHttpStatus RankListEmpty = 404
txBuildingExceptionToHttpStatus PromotionNotFound = 404
txBuildingExceptionToHttpStatus OracleNotFound = 404
txBuildingExceptionToHttpStatus DeployedScriptsNotReady = 503
txBuildingExceptionToHttpStatus ProtocolPaused = 503
txBuildingExceptionToHttpStatus _ = 400
