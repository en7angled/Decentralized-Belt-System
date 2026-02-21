module Test.Helpers
  ( assert,
    queryOracle,
  )
where

import Control.Monad.Reader (runReaderT)
import GeniusYield.TxBuilder.Query.Class (GYTxQueryMonad)
import Onchain.Protocol.Types (OracleParams (..))
import TxBuilding.Context (DeployedScriptsContext)
import TxBuilding.Lookups (queryOracleParams)

-- | Assert a boolean in the test monad; calls 'error' if False.
assert :: (GYTxQueryMonad m) => Bool -> m ()
assert True = return ()
assert False = error "Assertion failed"

-- | Query oracle params from the deployed scripts context.
queryOracle :: (GYTxQueryMonad m) => DeployedScriptsContext -> m OracleParams
queryOracle ctx = do
  (params, _, _) <- runReaderT queryOracleParams ctx
  return params
