{-# LANGUAGE OverloadedStrings #-}

-- | Shared HTTP error construction and client-facing sanitization for the API
-- servers. Full internal detail is logged by callers; clients receive only the
-- generic messages produced here.
module WebAPI.Errors
  ( mkServantErr,
    genericErrorMessage,
  ) where

import qualified Data.ByteString.Lazy.Char8 as BL8
import Servant

-- | Build a 'ServerError' for an HTTP status code with the given body.
-- Unlike a naive mapping, non-4xx statuses are not collapsed to 400: a 5xx
-- stays a 5xx (fixes probe failures being reported as 400).
mkServantErr :: Int -> String -> ServerError
mkServantErr status msg = base {errBody = BL8.pack msg}
  where
    base = case status of
      400 -> err400
      404 -> err404
      500 -> err500
      502 -> err502
      503 -> err503
      _ -> err500

-- | Generic, non-sensitive client-facing message for an HTTP status code.
genericErrorMessage :: Int -> String
genericErrorMessage status = case status of
  400 -> "Bad request"
  404 -> "Not found"
  502 -> "Upstream service error"
  503 -> "Service temporarily unavailable"
  _ -> "Internal server error"
