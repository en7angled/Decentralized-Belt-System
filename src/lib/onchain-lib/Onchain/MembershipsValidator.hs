{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use &&" #-}

-- | Memberships validator managing membership histories and intervals
-- in a sorted linked list.
module Onchain.MembershipsValidator
  ( -- * Memberships Redeemer
    MembershipsRedeemer (..),

    -- * Memberships Validator
    membershipsLambda,

    -- * Compilation
    membershipsCompile,
  )
where

-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:remove-trace #-}
-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:optimize #-}
-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

import GHC.Generics
import Onchain.CIP68 (deriveUserFromRefAC)
import Onchain.LinkedList (NodeDatum (nodeKey))
import Onchain.Protocol
  ( MembershipDatum (..),
    MembershipHistoriesListNode (..),
    MembershipHistoriesListNodeId,
    MembershipIntervalId,
    OnchainMembershipHistory (..),
    OnchainMembershipInterval (..),
    acceptMembershipInterval,
    addMembershipIntervalToHistory,
    appendMembershipHistory,
    insertMembershipHistoryInBetween,
    mkMembershipHistoriesListNode,
    unsafeGetListNodeDatumAndValue,
    unsafeGetMembershipHistory,
    unsafeGetMembershipInterval,
    updateNodeMembershipHistory,
  )
import Onchain.Utils (mkUntypedLambda)
import Onchain.Utils qualified as Utils
import PlutusLedgerApi.V1 qualified as V1
import PlutusLedgerApi.V3
import PlutusLedgerApi.V3.Contexts
import PlutusTx
import PlutusTx.Blueprint
import PlutusTx.Prelude
import Prelude qualified

-------------------------------------------------------------------------------

-- * Memberships Redeemer

-------------------------------------------------------------------------------

-- | Custom redeemer :
data MembershipsRedeemer
  = -- | Insert a new node to the membership histories list when a new member is added to the organization
    InsertNodeToMHList
      { maybeRightNodeId :: Maybe MembershipHistoriesListNodeId, -- ˆ Nothing if the new node is the last node
        insertedMembershipHistory :: OnchainMembershipHistory,
        updatedLeftNodeTxOutIdx :: Integer, -- ˆ The output index of the updated left node
        insertedNodeTxOutIdx :: Integer -- ˆ The output index of the inserted node
      }
  | -- | Update a node in the membership histories list when an existing member has a new interval added to his membership history
    UpdateNodeInMHList
      { lastIntervalId :: MembershipIntervalId,
        startDate :: POSIXTime,
        endDate :: Maybe POSIXTime,
        updatedNodeTxOutIdx :: Integer -- ˆ The output index of the updated node
      }
  | -- | Accept an interval when a practitioner accepts a membership interval
    AcceptInterval
      { updatedIntervalTxOutIdx :: Integer -- ˆ The output index of the updated interval
      }
  | -- | Permissionless dust/griefing cleanup. Anyone can spend a UTxO at the
    -- validator address if its datum is absent or does not parse as a valid
    -- protocol datum. Legitimate protocol UTxOs (with valid datums) are rejected.
    Cleanup
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''MembershipsRedeemer [('InsertNodeToMHList, 0), ('UpdateNodeInMHList, 1), ('AcceptInterval, 2), ('Cleanup, 3)]

-------------------------------------------------------------------------------

-- * Memberships Validator

-------------------------------------------------------------------------------

{-# INLINEABLE membershipsLambda #-}
membershipsLambda :: ScriptContext -> Bool
membershipsLambda (ScriptContext txInfo@TxInfo {..} (Redeemer bsredeemer) scriptInfo) =
  let redeemer = unsafeFromBuiltinData @MembershipsRedeemer bsredeemer
   in case scriptInfo of
        (SpendingScript spendingTxOutRef mdatum) -> case redeemer of
          -- Permissionless cleanup: allow spending if datum is absent or unparseable.
          Cleanup -> case mdatum of
            Nothing -> True
            Just (Datum bd) -> case fromBuiltinData @MembershipDatum bd of
              Nothing -> True
              Just _ -> traceError "k" -- Cannot cleanup UTxO with valid protocol datum
          _ ->
            let ownInput = Utils.unsafeFindOwnInputByTxOutRef spendingTxOutRef txInfoInputs
                ownValue = txOutValue ownInput
                ownAddress = txOutAddress ownInput
             in case mdatum of
                  Nothing -> traceError "l" -- No datum
                  Just (Datum bdatum) -> case fromBuiltinData bdatum of
                    Nothing -> traceError "m" -- Invalid datum
                    Just (datum :: MembershipDatum) -> case datum of
                      ListNodeDatum spendingNode -> case redeemer of
                        InsertNodeToMHList {maybeRightNodeId, insertedMembershipHistory, updatedLeftNodeTxOutIdx, insertedNodeTxOutIdx} ->
                          handleInsertNode txInfo ownValue ownAddress spendingNode maybeRightNodeId insertedMembershipHistory updatedLeftNodeTxOutIdx insertedNodeTxOutIdx
                        UpdateNodeInMHList {lastIntervalId, startDate, endDate, updatedNodeTxOutIdx} ->
                          handleUpdateNode txInfo ownValue ownAddress spendingNode lastIntervalId startDate endDate updatedNodeTxOutIdx
                        _otherRedeemer -> traceError "n" -- Invalid redeemer for ListNodeDatum
                      IntervalDatum unacceptedInterval -> case redeemer of
                        AcceptInterval {updatedIntervalTxOutIdx} ->
                          handleAcceptInterval txInfo ownValue ownAddress unacceptedInterval updatedIntervalTxOutIdx
                        _otherRedeemer -> traceError "o" -- Invalid redeemer for IntervalDatum
        _ -> traceError "p" -- Invalid purpose

-------------------------------------------------------------------------------

-- * Per-Redeemer Handlers

-------------------------------------------------------------------------------

{-# INLINEABLE handleInsertNode #-}
handleInsertNode ::
  TxInfo ->
  Value ->
  Address ->
  MembershipHistoriesListNode ->
  Maybe MembershipHistoriesListNodeId ->
  OnchainMembershipHistory ->
  Integer ->
  Integer ->
  Bool
handleInsertNode txInfo@TxInfo {..} ownValue ownAddress oldLeftNode maybeRightNodeId insertedMembershipHistory updatedLeftNodeTxOutIdx insertedNodeTxOutIdx =
  let -- Derive org User NFT from the EXISTING on-chain datum (not redeemer) to prevent cross-org attacks
      orgUserAC = deriveUserFromRefAC (organizationId oldLeftNode)
      insertedNodeNFT = V1.assetClassValue (membershipHistoryId insertedMembershipHistory) 1

      newIntervalId = membershipHistoryIntervalsHeadId $ unsafeGetMembershipHistory insertedNodeDatum
      newIntervalNFT = V1.assetClassValue newIntervalId 1

      -- nextNodeKey must be the right node's KEY (practitioner id), not its NFT (which is the node id not the node key) .
      (insertedNodeDatum, updatedLeftNode) = case maybeRightNodeId of
        Just rightNodeId ->
          let (_rightNodeValue, rightNode) = unsafeGetListNodeDatumAndValue rightNodeId ownAddress txInfoReferenceInputs
              nextKey = nodeKey (nodeInfo rightNode)
              insertedNodeDatum' = mkMembershipHistoriesListNode insertedMembershipHistory nextKey
              updatedLeftNode' = insertMembershipHistoryInBetween (oldLeftNode, rightNode, insertedNodeDatum')
           in (insertedNodeDatum', updatedLeftNode')
        Nothing ->
          let insertedNodeDatum' = mkMembershipHistoriesListNode insertedMembershipHistory Nothing
              updatedLeftNode' = appendMembershipHistory (oldLeftNode, insertedNodeDatum')
           in (insertedNodeDatum', updatedLeftNode')
   in and
        [ traceIfFalse "q" $ V1.assetClassValueOf (valueSpent txInfo) orgUserAC == 1, -- Must spend org User NFT
          traceIfFalse "r" $ Utils.checkTxOutAtIndexWithDatumValueAndAddress updatedLeftNodeTxOutIdx (ListNodeDatum updatedLeftNode) ownValue ownAddress txInfoOutputs, -- Lock updated left node
          traceIfFalse "s" $ mintValueMinted txInfoMint == (insertedNodeNFT + newIntervalNFT), -- Exact mint check
          traceIfFalse "t" $ Utils.checkTxOutAtIndexWithDatumValueAndAddress insertedNodeTxOutIdx (ListNodeDatum insertedNodeDatum) (insertedNodeNFT + Utils.minLovelaceValue) ownAddress txInfoOutputs -- Lock inserted node
        ]

{-# INLINEABLE handleUpdateNode #-}
handleUpdateNode ::
  TxInfo ->
  Value ->
  Address ->
  MembershipHistoriesListNode ->
  MembershipIntervalId ->
  POSIXTime ->
  Maybe POSIXTime ->
  Integer ->
  Bool
handleUpdateNode txInfo@TxInfo {..} ownValue ownAddress spendingNode lastIntervalId startDate endDate updatedNodeTxOutIdx =
  let -- Derive org User NFT from the EXISTING on-chain datum (not redeemer) to prevent cross-org attacks
      orgUserAC = deriveUserFromRefAC (organizationId spendingNode)
      oldHistory = unsafeGetMembershipHistory spendingNode
      (_lastIntervalValue, lastInterval) = unsafeGetMembershipInterval lastIntervalId ownAddress txInfoReferenceInputs
      (newHistory, newInterval) = addMembershipIntervalToHistory oldHistory lastInterval startDate endDate
      newIntervalId = membershipIntervalId newInterval
      newIntervalNFT = V1.assetClassValue newIntervalId 1
      updatedNode = updateNodeMembershipHistory spendingNode newHistory
   in and
        [ traceIfFalse "u" $ V1.assetClassValueOf (valueSpent txInfo) orgUserAC == 1, -- Must spend org User NFT
          traceIfFalse "v" $ Utils.checkTxOutAtIndexWithDatumValueAndAddress updatedNodeTxOutIdx (ListNodeDatum updatedNode) ownValue ownAddress txInfoOutputs, -- Lock updated node
          traceIfFalse "w" $ mintValueMinted txInfoMint == newIntervalNFT -- Exact mint check
        ]

{-# INLINEABLE handleAcceptInterval #-}
handleAcceptInterval ::
  TxInfo ->
  Value ->
  Address ->
  OnchainMembershipInterval ->
  Integer ->
  Bool
handleAcceptInterval txInfo@TxInfo {..} ownValue ownAddress unacceptedInterval updatedIntervalTxOutIdx =
  let acceptedInterval = acceptMembershipInterval unacceptedInterval
      profileUserAssetClass = deriveUserFromRefAC (membershipIntervalPractitionerId acceptedInterval)
   in and
        [ traceIfFalse "x" $ Utils.checkTxOutAtIndexWithDatumValueAndAddress updatedIntervalTxOutIdx (IntervalDatum acceptedInterval) ownValue ownAddress txInfoOutputs, -- Lock updated interval
          traceIfFalse "y" $ V1.assetClassValueOf (valueSpent txInfo) profileUserAssetClass == 1 -- Must spend practitioner User NFT
        ]

-------------------------------------------------------------------------------

-- * Compilation

-------------------------------------------------------------------------------

-- | Lose the types
membershipsUntyped :: BuiltinData -> BuiltinUnit
membershipsUntyped = mkUntypedLambda membershipsLambda

-- | Compile the untyped lambda to a UPLC script and splice back to Haskell.
membershipsCompile :: CompiledCode (BuiltinData -> BuiltinUnit)
membershipsCompile = $$(compile [||membershipsUntyped||])
