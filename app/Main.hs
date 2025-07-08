module Main where

import Data.Word (Word8)
import Onchain.Blueprint (contractBlueprint, mintingPolicyBlueprint)
import Onchain.Protocol (ProtocolParams (..))
import PlutusTx.Blueprint (writeBlueprint)
import TxBuilding.Validators (defaultProtocolParams)

-- Example protocol parameters (you would get these from your actual validator hashes)
exampleProtocolParams :: ProtocolParams
exampleProtocolParams = defaultProtocolParams

main :: IO ()
main = do
  putStrLn "BJJ Belt System Blueprint Generator"
  putStrLn "=================================="

  -- Generate the complete contract blueprint
  let completeBlueprint = contractBlueprint exampleProtocolParams
  putStrLn "✓ Generated complete contract blueprint"

  -- Write the complete blueprint to a file
  writeBlueprint "bjj-belt-system-blueprint.json" completeBlueprint
  putStrLn "✓ Wrote complete blueprint to 'bjj-belt-system-blueprint.json'"

  putStrLn "\nBlueprint file generated successfully!"

