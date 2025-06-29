module TxBuildingExample where

import GeniusYield.Imports
import GeniusYield.Types
import Onchain.CIP68 (MetadataFields (..))
import Onchain.Types (Profile (..), ProfileId (..), ProfileType (..), RankId (..))
import TxBuilding.Context
import TxBuilding.Interactions

------------------------------------------------------------------------------------------------

-- * Example Usage

------------------------------------------------------------------------------------------------

-- | Example of creating a practitioner profile
exampleCreatePractitionerProfile :: IO ()
exampleCreatePractitionerProfile = do
  -- Initialize context (in a real application, this would come from configuration)
  let context = defaultProfileTxBuildingContext
  
  -- Create sample profile data
  let profileId = ProfileId "practitioner_001"
  let profileType = Practitioner
  let currentRank = Just (RankId "white_belt_001")
  let profile = Profile { profileId, profileType, currentRank }
  
  -- Create sample metadata
  let metadata = Metadata222
        { metadataName = "John Doe"
        , metadataDescription = "BJJ Practitioner"
        , metadataImageURI = "https://example.com/profile.jpg"
        }
  
  -- Example recipient address (replace with actual address)
  let recipient = addressFromText "addr_test1..."
  
  -- In a real application, you would run this in a monad that provides
  -- the necessary context for transaction building
  putStrLn "Example profile creation setup complete"
  putStrLn $ "Profile ID: " ++ show profileId
  putStrLn $ "Profile Type: " ++ show profileType
  putStrLn $ "Metadata Name: " ++ show metadata

-- | Example of creating an organization profile
exampleCreateOrganizationProfile :: IO ()
exampleCreateOrganizationProfile = do
  -- Initialize context
  let context = defaultProfileTxBuildingContext
  
  -- Create sample organization profile
  let profileId = ProfileId "org_001"
  let profileType = Organization
  let currentRank = Nothing -- Organizations don't have ranks
  let profile = Profile { profileId, profileType, currentRank }
  
  -- Create sample metadata
  let metadata = Metadata222
        { metadataName = "BJJ Academy"
        , metadataDescription = "Brazilian Jiu-Jitsu Training Academy"
        , metadataImageURI = "https://example.com/academy.jpg"
        }
  
  -- Example recipient address
  let recipient = addressFromText "addr_test1..."
  
  putStrLn "Example organization profile creation setup complete"
  putStrLn $ "Organization ID: " ++ show profileId
  putStrLn $ "Organization Type: " ++ show profileType
  putStrLn $ "Metadata Name: " ++ show metadata

-- | Example of updating a profile
exampleUpdateProfile :: IO ()
exampleUpdateProfile = do
  -- Initialize context
  let context = defaultProfileTxBuildingContext
  
  -- Example profile reference token (in a real application, this would be obtained from a previous transaction)
  let profileRefAC = AssetClass (CurrencySymbol "mock_cs", TokenName "mock_tn")
  
  -- Create updated metadata
  let updatedMetadata = Metadata222
        { metadataName = "John Doe - Updated"
        , metadataDescription = "BJJ Practitioner - Updated Description"
        , metadataImageURI = "https://example.com/profile_updated.jpg"
        }
  
  -- Example recipient address
  let recipient = addressFromText "addr_test1..."
  
  putStrLn "Example profile update setup complete"
  putStrLn $ "Profile Reference Token: " ++ show profileRefAC
  putStrLn $ "Updated Metadata Name: " ++ show updatedMetadata

-- | Example of deleting a profile
exampleDeleteProfile :: IO ()
exampleDeleteProfile = do
  -- Initialize context
  let context = defaultProfileTxBuildingContext
  
  -- Example profile reference token
  let profileRefAC = AssetClass (CurrencySymbol "mock_cs", TokenName "mock_tn")
  
  -- Example recipient address (where to send the recovered collateral)
  let recipient = addressFromText "addr_test1..."
  
  putStrLn "Example profile deletion setup complete"
  putStrLn $ "Profile Reference Token: " ++ show profileRefAC
  putStrLn "Collateral will be sent to: " ++ show recipient

------------------------------------------------------------------------------------------------

-- * Helper Functions

------------------------------------------------------------------------------------------------

-- | Helper function to create address from text (placeholder)
addressFromText :: Text -> GYAddress
addressFromText _ = error "Replace with actual address parsing logic"

-- | Main function to run all examples
main :: IO ()
main = do
  putStrLn "=== Decentralized Belt System TxBuilding Examples ==="
  putStrLn ""
  
  putStrLn "1. Creating Practitioner Profile:"
  exampleCreatePractitionerProfile
  putStrLn ""
  
  putStrLn "2. Creating Organization Profile:"
  exampleCreateOrganizationProfile
  putStrLn ""
  
  putStrLn "3. Updating Profile:"
  exampleUpdateProfile
  putStrLn ""
  
  putStrLn "4. Deleting Profile:"
  exampleDeleteProfile
  putStrLn ""
  
  putStrLn "=== Examples Complete ==="
  putStrLn ""
  putStrLn "Note: These are examples showing the structure and setup."
  putStrLn "In a real application, you would need to:"
  putStrLn "  - Configure proper network settings"
  putStrLn "  - Set up proper signing keys"
  putStrLn "  - Handle transaction submission and monitoring"
  putStrLn "  - Implement proper error handling"
  putStrLn "  - Add minting policy for profile tokens" 