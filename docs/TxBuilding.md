# TxBuilding Module Documentation

## Overview

The TxBuilding module provides a comprehensive framework for building and executing transactions for the Decentralized Belt System's Profile validator. It follows the same patterns and structure as the raffleize project, using the Atlas Cardano library for transaction building.

## Module Structure

```
src/TxBuilding/
├── Context.hs          -- Transaction building context
├── Exceptions.hs       -- Custom exceptions
├── Interactions.hs     -- High-level interaction functions
├── Lookups.hs          -- UTxO lookup functions
├── Operations.hs       -- Transaction building operations
├── Skeletons.hs        -- Transaction skeleton builders
├── Transactions.hs     -- Transaction execution functions
├── Utils.hs           -- Utility functions
└── Validators.hs      -- Validator definitions
```

## Key Components

### 1. Validators (`TxBuilding.Validators`)

Defines the Profile validator and related scripts:

```haskell
-- Profile validator
profilesValidatorGY :: GYScript 'PlutusV3
profilesValidatorHashGY :: GYScriptHash

-- Export functions
exportProfilesScript :: IO ()
```

### 2. Context (`TxBuilding.Context`)

Defines the context for transaction building operations:

```haskell
data ProfileTxBuildingContext = ProfileTxBuildingContext
  { profilesValidatorRef :: GYTxOutRef
  }
```

### 3. Operations (`TxBuilding.Operations`)

Provides high-level transaction building operations:

```haskell
-- Create a new profile
createProfileTX :: GYAddress -> MetadataFields -> Profile -> m (GYTxSkeleton 'PlutusV3)

-- Update an existing profile
updateProfileTX :: GYAddress -> MetadataFields -> AssetClass -> m (GYTxSkeleton 'PlutusV3)

-- Delete a profile
deleteProfileTX :: GYAddress -> AssetClass -> m (GYTxSkeleton 'PlutusV3)
```

### 4. Interactions (`TxBuilding.Interactions`)

Provides high-level interaction functions:

```haskell
-- Create a new profile
createProfile :: GYAddress -> MetadataFields -> Profile -> m GYTxId

-- Update a profile
updateProfile :: GYAddress -> MetadataFields -> AssetClass -> m GYTxId

-- Delete a profile
deleteProfile :: GYAddress -> AssetClass -> m GYTxId
```

## Usage Examples

### Creating a Profile

```haskell
import TxBuilding.Interactions
import TxBuilding.Context
import Onchain.CIP68 (MetadataFields(..))
import Onchain.Types (Profile(..), ProfileId(..), ProfileType(..))

-- Create profile data
let profile = Profile 
  { profileId = ProfileId "practitioner_001"
  , profileType = Practitioner
  , currentRank = Just (RankId "white_belt_001")
  }

-- Create metadata
let metadata = Metadata222
  { metadataName = "John Doe"
  , metadataDescription = "BJJ Practitioner"
  , metadataImageURI = "https://example.com/profile.jpg"
  }

-- Create profile transaction
txId <- createProfile recipient metadata profile
```

### Updating a Profile

```haskell
-- Update metadata
let updatedMetadata = Metadata222
  { metadataName = "John Doe - Updated"
  , metadataDescription = "BJJ Practitioner - Updated"
  , metadataImageURI = "https://example.com/profile_updated.jpg"
  }

-- Update profile
txId <- updateProfile recipient updatedMetadata profileRefAC
```

### Deleting a Profile

```haskell
-- Delete profile and recover collateral
txId <- deleteProfile recipient profileRefAC
```

## Transaction Flow

1. **Create Profile**: 
   - Spends a seed UTxO
   - Locks profile data with CIP68 datum at validator address
   - Requires 2 ADA collateral

2. **Update Profile**:
   - Spends existing profile UTxO
   - Updates metadata while preserving profile data
   - Re-locks at validator address

3. **Delete Profile**:
   - Spends existing profile UTxO
   - Recovers collateral
   - Destroys profile state

## Dependencies

The TxBuilding module requires:

- `atlas-cardano` - For transaction building functionality
- `plutus-ledger-api` - For Plutus types
- `plutus-tx` - For Plutus compilation

## Configuration

To use the TxBuilding module, you need to:

1. Set up the Atlas Cardano dependency in your `cabal.project`
2. Configure the Profile validator reference script
3. Set up proper network configuration
4. Configure signing keys and addresses

## Error Handling

The module provides custom exceptions for various error conditions:

```haskell
data ProfileException
  = ProfileNotFound
  | ProfileAlreadyExists
  | InvalidProfileData
  | InsufficientFunds
  | InvalidMetadata
```

## Future Enhancements

- [ ] Add minting policy for profile tokens
- [ ] Implement batch operations
- [ ] Add support for profile tokens (reference and user tokens)
- [ ] Add validation for profile data
- [ ] Implement profile querying functions

## Notes

- The current implementation focuses on the Profile validator use cases
- Minting policy functionality is planned for future implementation
- Token-based operations (reference/user tokens) are currently placeholders
- The module follows the same patterns as the raffleize project for consistency 