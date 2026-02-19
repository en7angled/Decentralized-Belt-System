{-# LANGUAGE NoImplicitPrelude #-}

-- | Re-exports 'Onchain.Protocol.Types', 'Onchain.Protocol.Id', 'Onchain.Protocol.Lookup',
-- and 'Onchain.Protocol.Core' so that existing @import Onchain.Protocol@
-- statements continue to work unchanged.
module Onchain.Protocol
  ( module Onchain.Protocol.Types,
    module Onchain.Protocol.Id,
    module Onchain.Protocol.Lookup,
    module Onchain.Protocol.Core,
  )
where

import Onchain.Protocol.Core
import Onchain.Protocol.Id
import Onchain.Protocol.Lookup
import Onchain.Protocol.Types
