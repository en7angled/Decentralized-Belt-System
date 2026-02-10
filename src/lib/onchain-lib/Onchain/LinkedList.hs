{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use &&" #-}
{-# HLINT ignore "Use isNothing" #-}

module Onchain.LinkedList where

import GHC.Generics (Generic)
import PlutusLedgerApi.V1 (AssetClass (..))
import PlutusTx
import PlutusTx.Blueprint
import PlutusTx.Prelude
import Prelude qualified (Show (..))

-- | A generic linked list node datum that can hold any data type and use any key type.
--
--   Note: This type does NOT derive HasBlueprintDefinition or HasBlueprintSchema because
--   the PlutusTx TH macros don't support polymorphic types with blueprint constraints.
--   When you instantiate this type with concrete types (e.g., NodeDatum MyData BuiltinByteString),
--   you can create type aliases and use makeIsDataSchemaIndexed on those concrete types for blueprints.
data NodeDatum plutusData = NodeDatum
  { -- | Nothing for root node
    nodeKey :: Maybe AssetClass,
    -- | "pointer" to the next node (by key)
    nextNodeKey :: Maybe AssetClass,
    -- | the application-specific data
    nodeData :: plutusData
  }
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (HasBlueprintDefinition)

instance (Eq AssetClass, Eq plutusData) => Eq (NodeDatum plutusData) where
  (==) left right =
    and
      [ nodeKey left == nodeKey right,
        nextNodeKey left == nextNodeKey right,
        nodeData left == nodeData right
      ]

-- Generate ToData/FromData instances for serialization
makeIsDataIndexed ''NodeDatum [('NodeDatum, 0)]

-- * Operations Functions

-- | If not root or last it should have same currency symbol for the key and next key
{-# INLINEABLE checkIfValidNodeDatum #-}
checkIfValidNodeDatum :: NodeDatum plutusData -> Bool
checkIfValidNodeDatum node = case nodeKey node of
  Nothing -> True -- Root node
  Just key -> case nextNodeKey node of
    Nothing -> True -- Last node
    Just nextKey ->
      traceIfFalse "currency symbol mismatch for node key and next key"
        $ assetClassCurrencySymbol key
        == assetClassCurrencySymbol nextKey
  where
    assetClassCurrencySymbol (AssetClass (cs, _)) = cs

-- | This function validates the inputs and outputs of the insert operation
-- when inserting a node between two existing nodes
{-# INLINEABLE checkInputsAndInsertInBetweenNodes #-}
checkInputsAndInsertInBetweenNodes ::
  (Ord AssetClass) =>
  (NodeDatum plutusData, NodeDatum plutusData, NodeDatum plutusData) ->
  NodeDatum plutusData
checkInputsAndInsertInBetweenNodes (oldLeftNode, rightNode, insertedNode) =
  if validInputs
    then
      updatedLeftNode
    else
      traceError "Invalid insert inputs"
  where
    updatedLeftNode = oldLeftNode {nextNodeKey = nodeKey insertedNode}
    validInputs =
      and
        [ traceIfFalse "input nodes where adiacent" $ nextNodeKey oldLeftNode == nodeKey rightNode,
          traceIfFalse "left must be less than inserted" $ nodeKey oldLeftNode < nodeKey insertedNode,
          traceIfFalse "right must be greater than inserted" $ nodeKey rightNode > nodeKey insertedNode,
          traceIfFalse "inserted must point to the right node" $ nextNodeKey insertedNode == nodeKey rightNode,
          all checkIfValidNodeDatum [oldLeftNode, rightNode, insertedNode]
        ]

-- | This function validates the inputs and outputs of the append operation
-- when appending a node to the end of the list
{-# INLINEABLE checkInputsAndAppendNode #-}
checkInputsAndAppendNode ::
  (NodeDatum plutusData, NodeDatum plutusData) ->
  NodeDatum plutusData
checkInputsAndAppendNode (lastNode, appendedNode) =
  if validInputs
    then
      updatedLastNode
    else
      traceError "Invalid append inputs"
  where
    updatedLastNode = lastNode {nextNodeKey = nodeKey appendedNode}
    validInputs =
      and
        [ traceIfFalse "last node must not have a next node" $ isNothing (nextNodeKey lastNode),
          traceIfFalse "appended node must have a key" $ isJust (nodeKey appendedNode),
          traceIfFalse "appended node must not have a next node" $ isNothing (nextNodeKey appendedNode),
          traceIfFalse "appended node must be greater than last" $ nodeKey appendedNode > nodeKey lastNode,
          all checkIfValidNodeDatum [lastNode, appendedNode]
        ]
