{-# LANGUAGE NoImplicitPrelude #-}

-- | Deterministic ID generation for on-chain protocol entities.
-- Each function derives a unique 'AssetClass' by hashing relevant inputs.
module Onchain.Protocol.Id
  ( deriveRankId,
    derivePromotionRankId,
    deriveMembershipHistoryId,
    deriveMembershipIntervalId,
    deriveMembershipHistoriesListId,
  )
where

import Onchain.Protocol.Types
import Onchain.Utils (nameFromTxOutRef)
import PlutusLedgerApi.V1.Value (AssetClass (..))
import PlutusLedgerApi.V3 (CurrencySymbol, TokenName (TokenName), TxOutRef)
import PlutusTx.Builtins (ByteOrder (BigEndian), appendByteString, blake2b_224, integerToByteString)
import PlutusTx.Prelude (Integer)


-- | Generate a deterministic rank ID from a profile ID and rank number.
{-# INLINEABLE deriveRankId #-}
deriveRankId :: ProfileId -> Integer -> RankId
deriveRankId (AssetClass (cs, TokenName bs)) i = AssetClass (cs, TokenName (blake2b_224 (bs `appendByteString` integerToByteString BigEndian 0 i)))

-- | Generate a unique promotion rank ID from a seed TxOutRef.
-- The seed ensures uniqueness since each TxOutRef can only be spent once.
{-# INLINEABLE derivePromotionRankId #-}
derivePromotionRankId :: TxOutRef -> CurrencySymbol -> RankId
derivePromotionRankId seed cs = AssetClass (cs, TokenName (nameFromTxOutRef seed))

-- | Derive a unique membership history ID from two profile IDs.
{-# INLINEABLE deriveMembershipHistoryId #-}
deriveMembershipHistoryId :: ProfileId -> ProfileId -> MembershipHistoryId
deriveMembershipHistoryId (AssetClass (cs, TokenName orgId)) (AssetClass (_cs, TokenName prId)) =
  AssetClass (cs, TokenName (blake2b_224 (orgId `appendByteString` prId)))

-- | Derive a unique membership interval ID from a membership history ID and a number.
{-# INLINEABLE deriveMembershipIntervalId #-}
deriveMembershipIntervalId :: MembershipHistoryId -> Integer -> MembershipIntervalId
deriveMembershipIntervalId (AssetClass (cs, TokenName bs)) i =
  AssetClass (cs, TokenName (blake2b_224 (bs `appendByteString` integerToByteString BigEndian 0 i)))

-- | Derive a unique membership histories list ID from a profile ID as the hash of the profile ID.
{-# INLINEABLE deriveMembershipHistoriesListId #-}
deriveMembershipHistoriesListId :: ProfileId -> MembershipHistoriesListNodeId
deriveMembershipHistoriesListId (AssetClass (cs, TokenName bs)) = AssetClass (cs, TokenName (blake2b_224 bs))
