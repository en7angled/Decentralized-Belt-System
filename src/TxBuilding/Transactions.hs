module TxBuilding.Transactions where

import GeniusYield.TxBuilder
import GeniusYield.Types

------------------------------------------------------------------------------------------------

-- * Transaction Execution Functions

------------------------------------------------------------------------------------------------



------------------------------------------------------------------------------------------------

-- * Helper Functions

------------------------------------------------------------------------------------------------

-- | Build and submit transaction
buildTx :: (GYTxMonad m) => GYTxSkeleton 'PlutusV3 -> m GYTxId
buildTx skeleton = do
  txBody <- buildTxBody skeleton
  tx <- signTxBody txBody
  submitTx tx