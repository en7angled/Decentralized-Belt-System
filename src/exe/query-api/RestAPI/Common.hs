module RestAPI.Common (withBackend) where

import Control.Monad.Reader (asks)
import QueryAppMonad

-- | Dispatch to live or projected backend based on the context setting.
withBackend :: QueryAppMonad a -> QueryAppMonad a -> QueryAppMonad a
withBackend liveAct projectedAct = do
  live <- asks liveProjection
  if live then liveAct else projectedAct
