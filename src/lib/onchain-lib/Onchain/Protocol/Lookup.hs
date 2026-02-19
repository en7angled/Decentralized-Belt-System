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
    readOracleParams,
    checkFee,
  )
where

import Onchain.CIP68 (CIP68Datum (..))
import Onchain.Protocol.Types
import Onchain.Utils (checkAndGetCurrentStateDatumAndValue)
import PlutusLedgerApi.V3 
import PlutusTx.Prelude  
import qualified PlutusLedgerApi.V1 as V1
import qualified PlutusTx.List
import PlutusLedgerApi.V1.Value

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
        _ -> traceError "K0"

-- | Look up a membership interval datum and its UTxO value from transaction inputs. Fails if not found.
{-# INLINEABLE unsafeGetMembershipInterval #-}
unsafeGetMembershipInterval :: MembershipIntervalId -> Address -> [TxInInfo] -> (Value, OnchainMembershipInterval)
unsafeGetMembershipInterval intervalId addr txins =
  let (v, b) = checkAndGetCurrentStateDatumAndValue intervalId addr txins
   in case unsafeFromBuiltinData b :: MembershipDatum of
        IntervalDatum interval -> (v, interval)
        _ -> traceError "K0"

-------------------------------------------------------------------------------

-- * Oracle Helpers

-------------------------------------------------------------------------------

-- | Read 'OracleParams' from the oracle UTxO identified by its NFT 'AssetClass'
-- in the transaction's reference inputs. Fails if the oracle UTxO is not found
-- or the datum cannot be decoded.
{-# INLINEABLE readOracleParams #-}
readOracleParams :: AssetClass -> [TxInInfo] -> OracleParams
readOracleParams oracleAC refInputs =
  case PlutusTx.List.find hasOracleNFT refInputs of
    Just (TxInInfo _ TxOut {txOutDatum}) -> case txOutDatum of
      OutputDatum (Datum bd) -> case fromBuiltinData @OracleParams bd of
        Just params -> params
        Nothing -> traceError "K1" -- Oracle read failed: cannot decode datum (K1)
      _ -> traceError "K1" -- Oracle read failed: must have inline datum (K1)
    Nothing -> traceError "K1" -- Oracle read failed: UTxO not found (K1)
  where
    hasOracleNFT (TxInInfo _ TxOut {txOutValue}) =
      V1.assetClassValueOf txOutValue oracleAC == 1

-- | Check that a fee payment is included in the transaction outputs.
-- If 'opFeeConfig' is 'Nothing', no fee is required and the check passes.
-- If 'opFeeConfig' is 'Just', validates that at least one output pays
-- >= the required fee (extracted via the selector) to 'fcFeeAddress'.
{-# INLINEABLE checkFee #-}
checkFee :: OracleParams -> (FeeConfig -> Integer) -> [TxOut] -> Bool
checkFee oracle feeSelector outputs = case opFeeConfig oracle of
  Nothing -> True
  Just feeConfig ->
    let requiredFee = feeSelector feeConfig
        feeAddr = fcFeeAddress feeConfig
        feeValue = V1.lovelaceValue (V1.Lovelace requiredFee)
     in traceIfFalse "K2" -- Must pay required fee (K2)
          $ any
            ( \TxOut {txOutValue = v, txOutAddress = a} ->
                a == feeAddr && v `geq` feeValue
            )
            outputs