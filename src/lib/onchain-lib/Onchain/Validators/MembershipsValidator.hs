{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use &&" #-}

-- | Memberships validator managing membership histories and intervals
-- in a sorted linked list.
module Onchain.Validators.MembershipsValidator
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
    deriveIntervalsHeadId,
    deriveMembershipHistoryIdFromHistory,
    deriveMembershipIntervalId,
    insertMembershipHistoryInBetween,
    mkMembershipHistoriesListNode,
    opMinUTxOValue,
    oracleToken,
    readOracleParams,
    unsafeGetListNodeDatumAndValue,
    unsafeGetMembershipHistory,
    unsafeGetMembershipInterval,
    unsafeGetProtocolParamsFromProfileRefInput,
    updateMembershipIntervalEndDate,
    updateNodeMembershipHistory,
  )
import Onchain.Utils (mkUntypedLambda)
import Onchain.Utils qualified as Utils
  ( checkTxOutAtIndexWithDatumMinValueAndAddress,
    unsafeFindOwnInputByTxOutRef,
  )
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
  | -- | Update the end date of a membership interval (org: any future; practitioner: shorten/close accepted only)
    UpdateEndDate
      { membershipHistoryNodeId :: MembershipHistoriesListNodeId,
        newEndDate :: POSIXTime,
        updatedIntervalTxOutIdx :: Integer
      }
  | -- | Permissionless dust/griefing cleanup. Anyone can spend a UTxO at the
    -- validator address if its datum is absent or does not parse as a valid
    -- protocol datum. Legitimate protocol UTxOs (with valid datums) are rejected.
    Cleanup
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''MembershipsRedeemer [('InsertNodeToMHList, 0), ('UpdateNodeInMHList, 1), ('AcceptInterval, 2), ('Cleanup, 3), ('UpdateEndDate, 4)]

-------------------------------------------------------------------------------

-- * Memberships Validator

-------------------------------------------------------------------------------

{-# INLINEABLE membershipsLambda #-}
membershipsLambda :: ScriptContext -> Bool
membershipsLambda (ScriptContext txInfo@TxInfo {..} (Redeemer bsredeemer) scriptInfo) =
  let !redeemer = unsafeFromBuiltinData @MembershipsRedeemer bsredeemer
   in case scriptInfo of
        (SpendingScript spendingTxOutRef mdatum) -> case redeemer of
          -- Permissionless cleanup: allow spending if datum is absent or unparseable.
          Cleanup -> case mdatum of
            Nothing -> True
            Just (Datum bd) -> case fromBuiltinData @MembershipDatum bd of
              Nothing -> True
              Just _ -> traceError "V0" -- Cannot cleanup UTxO with valid protocol datum (V0)
          _ ->
            let !ownInput = Utils.unsafeFindOwnInputByTxOutRef spendingTxOutRef txInfoInputs
                !ownValue = txOutValue ownInput
                !ownAddress = txOutAddress ownInput
             in case mdatum of
                  Nothing -> traceError "V1" -- No datum (V1)
                  Just (Datum bdatum) -> case fromBuiltinData bdatum of
                    Nothing -> traceError "V1" -- Invalid datum (V1)
                    Just (datum :: MembershipDatum) -> case datum of
                      ListNodeDatum spendingNode -> case redeemer of
                        InsertNodeToMHList {maybeRightNodeId, insertedMembershipHistory, updatedLeftNodeTxOutIdx, insertedNodeTxOutIdx} ->
                          handleInsertNode txInfo ownValue ownAddress spendingNode maybeRightNodeId insertedMembershipHistory updatedLeftNodeTxOutIdx insertedNodeTxOutIdx
                        UpdateNodeInMHList {lastIntervalId, startDate, endDate, updatedNodeTxOutIdx} ->
                          handleUpdateNode txInfo ownValue ownAddress spendingNode lastIntervalId startDate endDate updatedNodeTxOutIdx
                        _otherRedeemer -> traceError "V2" -- Invalid redeemer for ListNodeDatum (V2)
                      IntervalDatum interval -> case redeemer of
                        AcceptInterval {updatedIntervalTxOutIdx} ->
                          handleAcceptInterval txInfo ownValue ownAddress interval updatedIntervalTxOutIdx
                        UpdateEndDate {membershipHistoryNodeId, newEndDate, updatedIntervalTxOutIdx} ->
                          handleUpdateEndDate txInfo ownValue ownAddress interval membershipHistoryNodeId newEndDate updatedIntervalTxOutIdx
                        _otherRedeemer -> traceError "V3" -- Invalid redeemer for IntervalDatum (V3)
        _ -> traceError "V4" -- Invalid purpose (V4)

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
      historyId = deriveMembershipHistoryIdFromHistory insertedMembershipHistory
      insertedNodeNFT = V1.assetClassValue historyId 1

      insertedHistory = unsafeGetMembershipHistory insertedNodeDatum
      newIntervalId = deriveIntervalsHeadId insertedHistory
      newIntervalNFT = V1.assetClassValue newIntervalId 1

      -- Min lovelace from oracle (org profile must be ref input so we can get ProtocolParams and read oracle)
      protocolParams' = unsafeGetProtocolParamsFromProfileRefInput (organizationId oldLeftNode) txInfoReferenceInputs
      oracle = readOracleParams (oracleToken protocolParams') txInfoReferenceInputs
      minLv = V1.lovelaceValue (V1.Lovelace (opMinUTxOValue oracle))

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
   in traceIfFalse "V5" -- Insert node handler failed (V5)
        $ and
          [ V1.assetClassValueOf (valueSpent txInfo) orgUserAC == 1, -- Must spend org User NFT
            Utils.checkTxOutAtIndexWithDatumMinValueAndAddress updatedLeftNodeTxOutIdx (ListNodeDatum updatedLeftNode) ownValue ownAddress txInfoOutputs, -- Lock updated left node (>= ownValue; balancer may add min-ADA)
            mintValueMinted txInfoMint == (insertedNodeNFT + newIntervalNFT), -- Exact mint check
            Utils.checkTxOutAtIndexWithDatumMinValueAndAddress insertedNodeTxOutIdx (ListNodeDatum insertedNodeDatum) (insertedNodeNFT + minLv) ownAddress txInfoOutputs -- Lock inserted node (>= minLv + NFT)
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
      -- Verify redeemer-provided lastIntervalId matches the derived head
      expectedHeadId = deriveIntervalsHeadId oldHistory
      validRedeemerId = expectedHeadId == lastIntervalId
      (_lastIntervalValue, lastInterval) = unsafeGetMembershipInterval lastIntervalId ownAddress txInfoReferenceInputs
      (newHistory, newInterval) = addMembershipIntervalToHistory oldHistory lastInterval startDate endDate
      historyId = deriveMembershipHistoryIdFromHistory oldHistory
      newIntervalId = deriveMembershipIntervalId historyId (membershipIntervalNumber newInterval)
      newIntervalNFT = V1.assetClassValue newIntervalId 1
      updatedNode = updateNodeMembershipHistory spendingNode newHistory
   in traceIfFalse "V6" -- Update node handler failed (V6)
        $ and
          [ validRedeemerId,
            V1.assetClassValueOf (valueSpent txInfo) orgUserAC == 1,
            Utils.checkTxOutAtIndexWithDatumMinValueAndAddress updatedNodeTxOutIdx (ListNodeDatum updatedNode) ownValue ownAddress txInfoOutputs, -- >= ownValue; balancer may add min-ADA
            mintValueMinted txInfoMint == newIntervalNFT
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
   in traceIfFalse "V7" -- Accept interval handler failed (V7)
        $ and
          [ Utils.checkTxOutAtIndexWithDatumMinValueAndAddress updatedIntervalTxOutIdx (IntervalDatum acceptedInterval) ownValue ownAddress txInfoOutputs, -- >= ownValue; balancer may add min-ADA
            V1.assetClassValueOf (valueSpent txInfo) profileUserAssetClass == 1 -- Must spend practitioner User NFT
          ]

{-# INLINEABLE handleUpdateEndDate #-}
handleUpdateEndDate ::
  TxInfo ->
  Value ->
  Address ->
  OnchainMembershipInterval ->
  MembershipHistoriesListNodeId ->
  POSIXTime ->
  Integer ->
  Bool
handleUpdateEndDate txInfo@TxInfo {..} ownValue ownAddress interval membershipHistoryNodeId newEndDate updatedIntervalTxOutIdx =
  let (_historyNodeValue, historyNode) = unsafeGetListNodeDatumAndValue membershipHistoryNodeId ownAddress txInfoReferenceInputs
      history = unsafeGetMembershipHistory historyNode
      -- Interval must belong to this history: practitioner match + NFT in ownValue
      -- matches the derivation from history fields. Verifying against ownValue is
      -- stronger than comparing stored datum fields — it proves the actual token
      -- locked in this UTxO matches the expected derivation.
      historyId = deriveMembershipHistoryIdFromHistory history
      expectedIntervalId = deriveMembershipIntervalId historyId (membershipIntervalNumber interval)
      intervalBelongsToHistory =
        membershipHistoryPractitionerId history
          == membershipIntervalPractitionerId interval -- interval datum and history agree on who the practitioner is.
          && V1.assetClassValueOf ownValue expectedIntervalId
          == 1 -- the token we’re spending is the one that belongs to this history.
      orgUserAC = deriveUserFromRefAC (membershipHistoryOrganizationId history)
      practitionerUserAC = deriveUserFromRefAC (membershipIntervalPractitionerId interval)
      spentOrg = V1.assetClassValueOf (valueSpent txInfo) orgUserAC == 1
      spentPractitioner = V1.assetClassValueOf (valueSpent txInfo) practitionerUserAC == 1
      isOrganization = spentOrg && not spentPractitioner
      mustSpendOneUserNFT = spentOrg /= spentPractitioner -- exactly one of org or practitioner
      updatedInterval = updateMembershipIntervalEndDate isOrganization interval newEndDate txInfoValidRange
   in traceIfFalse "V8" -- UpdateEndDate handler failed (V8)
        $ and
          [ intervalBelongsToHistory,
            mustSpendOneUserNFT,
            Utils.checkTxOutAtIndexWithDatumMinValueAndAddress updatedIntervalTxOutIdx (IntervalDatum updatedInterval) ownValue ownAddress txInfoOutputs -- >= ownValue; balancer may add min-ADA
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
