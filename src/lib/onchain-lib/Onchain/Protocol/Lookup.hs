{-# LANGUAGE NoImplicitPrelude #-}

-- | Typed datum lookup helpers for extracting protocol data from transaction inputs.
-- Each function locates the UTxO carrying a specific state NFT and deserializes
-- the inline datum into the expected on-chain type.
module Onchain.Protocol.Lookup
  ( unsafeGetRank,
    unsafeGetRankDatumAndValue,
    unsafeGetProfile,
    unsafeGetProfileDatumAndValue,
    unsafeGetListNodeDatumAndValue,
    unsafeGetMembershipInterval,
  )
where

import Onchain.CIP68 (CIP68Datum (..))
import Onchain.Protocol.Types
import Onchain.Utils (checkAndGetCurrentStateDatumAndValue)
import PlutusLedgerApi.V3 (Address, TxInInfo, Value)
import PlutusTx (unsafeFromBuiltinData)
import PlutusTx.Prelude (traceError)

-- | Look up a rank datum from transaction inputs. Fails if not found.
{-# INLINEABLE unsafeGetRank #-}
unsafeGetRank :: RankId -> Address -> [TxInInfo] -> OnchainRank
unsafeGetRank ac addr txins =
  let (_, b) = checkAndGetCurrentStateDatumAndValue ac addr txins
   in unsafeFromBuiltinData b

-- | Look up a rank datum and its UTxO value from transaction inputs. Fails if not found.
{-# INLINEABLE unsafeGetRankDatumAndValue #-}
unsafeGetRankDatumAndValue :: RankId -> Address -> [TxInInfo] -> (Value, OnchainRank)
unsafeGetRankDatumAndValue ac addr txins =
  let (v, b) = checkAndGetCurrentStateDatumAndValue ac addr txins
   in (v, unsafeFromBuiltinData b)

-- | Look up a profile datum (unwrapped from CIP68) from transaction inputs. Fails if not found.
{-# INLINEABLE unsafeGetProfile #-}
unsafeGetProfile :: ProfileId -> Address -> [TxInInfo] -> OnchainProfile
unsafeGetProfile ac addr txins =
  let (_, b) = checkAndGetCurrentStateDatumAndValue ac addr txins
   in extra (unsafeFromBuiltinData b :: CIP68Datum OnchainProfile)

-- | Look up a CIP68 profile datum and its UTxO value from transaction inputs. Fails if not found.
{-# INLINEABLE unsafeGetProfileDatumAndValue #-}
unsafeGetProfileDatumAndValue :: ProfileId -> Address -> [TxInInfo] -> (Value, CIP68Datum OnchainProfile)
unsafeGetProfileDatumAndValue ac addr txins =
  let (v, b) = checkAndGetCurrentStateDatumAndValue ac addr txins
   in (v, unsafeFromBuiltinData b :: CIP68Datum OnchainProfile)

-- | Look up a membership histories list node and its UTxO value from transaction inputs. Fails if not found.
{-# INLINEABLE unsafeGetListNodeDatumAndValue #-}
unsafeGetListNodeDatumAndValue :: MembershipHistoriesListNodeId -> Address -> [TxInInfo] -> (Value, MembershipHistoriesListNode)
unsafeGetListNodeDatumAndValue listNodeId addr txins =
  let (v, b) = checkAndGetCurrentStateDatumAndValue listNodeId addr txins
   in case unsafeFromBuiltinData b :: MembershipDatum of
        ListNodeDatum node -> (v, node)
        _ -> traceError "0"

-- | Look up a membership interval datum and its UTxO value from transaction inputs. Fails if not found.
{-# INLINEABLE unsafeGetMembershipInterval #-}
unsafeGetMembershipInterval :: MembershipIntervalId -> Address -> [TxInInfo] -> (Value, OnchainMembershipInterval)
unsafeGetMembershipInterval intervalId addr txins =
  let (v, b) = checkAndGetCurrentStateDatumAndValue intervalId addr txins
   in case unsafeFromBuiltinData b :: MembershipDatum of
        IntervalDatum interval -> (v, interval)
        _ -> traceError "1"
