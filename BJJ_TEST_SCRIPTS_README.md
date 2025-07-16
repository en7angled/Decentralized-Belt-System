# BJJ Belt System Test Scripts

This directory contains bash scripts that reproduce the `blackPromotesWhiteToBlue` test from the UnitTests using the BJJ Belt System CLI.

## Scripts Overview

### 1. `black_promotes_white_to_blue.sh` (Complete Test)
This script reproduces the full `blackPromotesWhiteToBlue` test from UnitTests.hs, including:
- Creating a master profile with Black belt
- Creating a student profile with White belt
- Promoting student from White to Blue belt
- Accepting the Blue belt promotion
- Promoting student from Blue to Purple belt
- Accepting the Purple belt promotion

### 2. `simple_black_promotes_white_to_blue.sh` (Core Test)
This script focuses on the core promotion scenario:
- Creating a master profile with Black belt
- Creating a student profile with White belt
- Promoting student from White to Blue belt
- Accepting the Blue belt promotion

## Prerequisites

1. **BJJ CLI Built**: Make sure the BJJ Belt System CLI is built and available as `admin` command
2. **Configuration Files**: Ensure you have the required configuration files:
   - `config_atlas.json` - Atlas configuration
   - `operation.prv` - Private key file
   - `config_bjj_validators.json` - Will be created by the script if not present

## Usage

### Running the Complete Test
```bash
./black_promotes_white_to_blue.sh
```

### Running the Simple Test
```bash
./simple_black_promotes_white_to_blue.sh
```

## What the Scripts Do

### Step-by-Step Process

1. **Deploy Reference Scripts**: If not already deployed, the script will deploy the necessary reference scripts for the BJJ system.

2. **Create Master Profile**: Creates a master profile with Black belt using the same data as the UnitTest:
   - Name: "Master"
   - Description: "Master is a master"
   - Image URI: "ipfs://Qmb3JXJHQxuReSUaH6rXAoP5oX9NRs6JmnRFGTj2RVhGwe"
   - Belt: Black

3. **Create Student Profile**: Creates a student profile with White belt (initially):
   - Name: "John Doe"
   - Description: "John Doe is a student"
   - Image URI: "ipfs://QmReBRNMe7tBr6WbA89uwnHHW7f7Zoe8wY2mzVpA8STdAk"

4. **Promote to Blue Belt**: The master promotes the student from White to Blue belt.

5. **Accept Promotion**: The student accepts the Blue belt promotion.

6. **Promote to Purple Belt** (Complete test only): The master promotes the student from Blue to Purple belt with appropriate time spacing.

7. **Accept Purple Promotion** (Complete test only): The student accepts the Purple belt promotion.

### Asset ID Capture

The scripts use the `--output-id` flag to capture asset IDs from CLI commands:
- **Profile IDs**: Captured when creating profiles
- **Promotion IDs**: Captured when creating promotions
- **Rank IDs**: Captured when accepting promotions

This allows the scripts to use the returned asset IDs in subsequent commands, just like the UnitTests do.

### Time Handling

The scripts use POSIX timestamps in **milliseconds** (not seconds) to match the Cardano blockchain requirements:
- Creation times are set to current time in milliseconds
- Promotion times are calculated with proper spacing
- Time calculations for belt promotions follow the minimum time requirements (e.g., 18 months for Blue to Purple)

## Expected Output

The scripts will provide colored output showing:
- `[INFO]` - General information about the process
- `[SUCCESS]` - Successful operations
- `[WARNING]` - Warnings (if any)
- `[ERROR]` - Errors (if any)

At the end, you'll see a summary with all the created asset IDs.

## Troubleshooting

### Common Issues

1. **CLI Not Found**: Make sure the `admin` command is available in your PATH
2. **Configuration Files Missing**: Ensure `config_atlas.json` and `operation.prv` exist
3. **Network Issues**: The scripts include delays for blockchain confirmation, but you may need to adjust these based on your network

### Error Handling

The scripts use `set -e` to exit on any error. If a step fails, the script will stop and show an error message.

## Customization

You can modify the scripts to:
- Change profile names and descriptions
- Adjust timing between operations
- Add additional promotion steps
- Modify the belt progression

## Comparison with UnitTest

The scripts reproduce the same logic as the `blackPromotesWhiteToBlue` test in `test/UnitTests.hs`:

```haskell
blackPromotesWhiteToBlue :: (HasCallStack) => TestInfo -> GYTxMonadClb ()
blackPromotesWhiteToBlue TestInfo {..} = do
  -- Create master with Black belt
  -- Create student with White belt
  -- Promote White to Blue
  -- Accept Blue promotion
  -- Promote Blue to Purple
  -- Accept Purple promotion
```

The bash scripts use the CLI commands to achieve the same result on a real Cardano network. 