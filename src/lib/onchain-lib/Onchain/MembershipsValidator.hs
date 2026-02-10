{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use &&" #-}

module Onchain.MembershipsValidator where

-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:remove-trace #-}
-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:optimize #-}
-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

import GHC.Generics
import Onchain.CIP68 (deriveUserFromRefAC)
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

-----------------------------------------------------------------------------
-- Memberships Validator
-----------------------------------------------------------------------------

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
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''MembershipsRedeemer [('InsertNodeToMHList, 0), ('UpdateNodeInMHList, 1), ('AcceptInterval, 2)]

{-# INLINEABLE membershipsLambda #-}
membershipsLambda :: ScriptContext -> Bool
membershipsLambda (ScriptContext txInfo@TxInfo {..} (Redeemer bsredeemer) scriptInfo) =
  let redeemer = unsafeFromBuiltinData @MembershipsRedeemer bsredeemer
      minValue = V1.lovelaceValue 3_500_000 
   in case scriptInfo of
        (SpendingScript spendingTxOutRef mdatum) ->
          let ownInput = Utils.unsafeFindOwnInputByTxOutRef spendingTxOutRef txInfoInputs
              ownValue = txOutValue ownInput
              ownAddress = txOutAddress ownInput
           in case mdatum of
                Nothing -> traceError "No datum"
                Just (Datum bdatum) -> case fromBuiltinData bdatum of
                  Nothing -> traceError "Invalid datum"
                  Just (datum :: MembershipDatum) -> case datum of
                    ListNodeDatum spendingNode -> case redeemer of
                      InsertNodeToMHList {maybeRightNodeId, insertedMembershipHistory, updatedLeftNodeTxOutIdx, insertedNodeTxOutIdx} ->
                        let oldLeftNode = spendingNode
                            -- Derive org User NFT from the EXISTING on-chain datum (not redeemer) to prevent cross-org attacks
                            orgUserAC = deriveUserFromRefAC (organizationId oldLeftNode)

                            insertedNodeDatum = mkMembershipHistoriesListNode insertedMembershipHistory maybeRightNodeId
                            insertedNodeNFT = V1.assetClassValue (membershipHistoryId insertedMembershipHistory) 1
                            newIntervalId = membershipHistoryIntervalsHeadId $ unsafeGetMembershipHistory insertedNodeDatum
                            newIntervalNFT = V1.assetClassValue newIntervalId 1

                            -- Validate insert rules and get the updated left node
                            updatedLeftNode = case maybeRightNodeId of
                              Just rightNodeId ->
                                let (_rightNodeValue, rightNode) = unsafeGetListNodeDatumAndValue rightNodeId ownAddress txInfoReferenceInputs
                                 in insertMembershipHistoryInBetween (oldLeftNode, rightNode, insertedNodeDatum)
                              Nothing ->
                                appendMembershipHistory (oldLeftNode, insertedNodeDatum)
                         in and
                              [ traceIfFalse "Must spend organization User NFT to modify membership list"
                                  $ V1.assetClassValueOf (valueSpent txInfo) orgUserAC
                                  == 1,
                                traceIfFalse "Must lock updated left node with inline datum at membershipsValidator address (output idx)"
                                  $ Utils.checkTxOutAtIndexWithDatumValueAndAddress updatedLeftNodeTxOutIdx (ListNodeDatum updatedLeftNode) ownValue ownAddress txInfoOutputs, -- Guarantees that tokens never leaves the validator.
                                traceIfFalse "Tx must mint JUST inserted node NFT and interval NFT"
                                  $ mintValueMinted txInfoMint -- protection against other-token-name attack vector
                                  == (insertedNodeNFT + newIntervalNFT),
                                traceIfFalse "Must lock inserted node at membershipsValidator address (output idx)"
                                  $ Utils.checkTxOutAtIndexWithDatumValueAndAddress insertedNodeTxOutIdx (ListNodeDatum insertedNodeDatum) (insertedNodeNFT + minValue) ownAddress txInfoOutputs
                              ]
                      UpdateNodeInMHList {lastIntervalId, startDate, endDate, updatedNodeTxOutIdx} ->
                        let -- Derive org User NFT from the EXISTING on-chain datum (not redeemer) to prevent cross-org attacks
                            orgUserAC = deriveUserFromRefAC (organizationId spendingNode)
                            oldHistory = unsafeGetMembershipHistory spendingNode
                            (_lastIntervalValue, lastInterval) = unsafeGetMembershipInterval lastIntervalId ownAddress txInfoReferenceInputs
                            (newHistory, newInterval) = addMembershipIntervalToHistory oldHistory lastInterval startDate endDate
                            newIntervalId = membershipIntervalId newInterval
                            newIntervalNFT = V1.assetClassValue newIntervalId 1
                            updatedNode = updateNodeMembershipHistory spendingNode newHistory
                         in and
                              [ traceIfFalse "Must spend organization User NFT to modify membership history"
                                  $ V1.assetClassValueOf (valueSpent txInfo) orgUserAC
                                  == 1,
                                traceIfFalse "Must lock updated node with inline datum at membershipsValidator address (output idx)"
                                  $ Utils.checkTxOutAtIndexWithDatumValueAndAddress updatedNodeTxOutIdx (ListNodeDatum updatedNode) ownValue ownAddress txInfoOutputs, -- Guarantees that tokens never leaves the validator.
                                traceIfFalse "Tx must mint JUST interval NFT"
                                  $ mintValueMinted txInfoMint -- protection against other-token-name attack vector
                                  == newIntervalNFT
                              ]
                      _otherRedeemer -> traceError "Invalid redeemer for ListNodeDatum"
                    IntervalDatum unacceptedInterval -> case redeemer of
                      AcceptInterval {updatedIntervalTxOutIdx} ->
                        let acceptedInterval = acceptMembershipInterval unacceptedInterval
                            profileUserAssetClass = deriveUserFromRefAC (membershipIntervalPractitionerId acceptedInterval)
                         in and
                              [ traceIfFalse "Must lock updated interval with inline datum at membershipsValidator address (output idx)"
                                  $ Utils.checkTxOutAtIndexWithDatumValueAndAddress updatedIntervalTxOutIdx (IntervalDatum acceptedInterval) ownValue ownAddress txInfoOutputs, -- Guarantees that tokens never leaves the validator.
                                traceIfFalse "Must spend profile User NFT to accept membership interval"
                                  $ V1.assetClassValueOf (valueSpent txInfo) profileUserAssetClass
                                  == 1
                              ]
                      _otherRedeemer -> traceError "Invalid redeemer for IntervalDatum"
        _ -> traceError "Invalid purpose"

-- | Lose the types
membershipsUntyped :: BuiltinData -> BuiltinUnit
membershipsUntyped = mkUntypedLambda membershipsLambda

-- | Compile the untyped lambda to a UPLC script and splice back to Haskell.
membershipsCompile :: CompiledCode (BuiltinData -> BuiltinUnit)
membershipsCompile = $$(compile [||membershipsUntyped||])
