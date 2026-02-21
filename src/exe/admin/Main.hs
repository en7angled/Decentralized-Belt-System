{-# LANGUAGE RecordWildCards #-}

module Main where

import Constants qualified
import Control.Monad.Reader (runReaderT)
import Data.Aeson (encode)
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as LSB8
import Data.ByteString.Lazy qualified as B
import Data.Char (toLower, toUpper)
import Data.Text qualified as T
import DomainTypes.Core.Actions (AdminActionType (..), ProfileActionType (..), ProfileData (..), ProtocolActionType (..))
import DomainTypes.Core.BJJ (BJJBelt (..), parseBelt)
import DomainTypes.Core.Types (ProfileType (..))
import GeniusYield.GYConfig
import GeniusYield.Imports
import GeniusYield.Types
import Onchain.Blueprint (contractBlueprint)
import Onchain.Protocol.Types (FeeConfig (..), OracleParams (..))
import Options.Applicative
import PlutusLedgerApi.V3
import System.Exit (die)
import Text.Read qualified as Text
import TxBuilding.Context
import TxBuilding.Interactions
import TxBuilding.Lookups (getFirstIntervalIdForMembershipNode, queryOracleParams)
import TxBuilding.Transactions
import TxBuilding.Utils
import TxBuilding.Validators (blueprintProtocolParams)
import Utils

mnemonicFilePath :: FilePath
mnemonicFilePath = "operation.prv"

printYellow :: String -> IO ()
printYellow = putStrLn . yellowColorString

printGreen :: String -> IO ()
printGreen = putStrLn . greenColorString

-- Command data types
-- NOTE: DeleteProfile command was intentionally removed to preserve lineage integrity.
-- BJJ belt records are permanent historical facts that should not be erasable.
data Command
  = DeployReferenceScripts
  | WriteBlueprint FilePath
  | PauseProtocol
  | UnpauseProtocol
  | SetFees SetFeesArgs
  | SetMinUTxOValue Integer
  | QueryOracle
  | InitProfile InitProfileArgs
  | UpdateProfileImage UpdateProfileImageArgs
  | PromoteProfile PromoteProfileArgs
  | AcceptPromotion AcceptPromotionArgs
  | CreateProfileWithRank CreateProfileWithRankArgs
  | CreateMembershipHistory CreateMembershipHistoryArgs
  | GetFirstMembershipIntervalId GetFirstMembershipIntervalIdArgs
  | AddMembershipInterval AddMembershipIntervalArgs
  | AcceptMembershipInterval AcceptMembershipIntervalArgs
  | UpdateEndDate UpdateEndDateArgs
  | AwardAchievement AwardAchievementArgs
  | AcceptAchievement AcceptAchievementArgs
  | CleanupDust
  deriving (Show)

data SetFeesArgs
  = ClearFees
  | UpdateFees
      { sfaFeeAddress :: String,
        sfaProfileCreationFee :: Integer,
        sfaPromotionFee :: Integer,
        sfaMembershipHistoryFee :: Integer,
        sfaMembershipIntervalFee :: Integer,
        sfaAchievementFee :: Integer
      }
  deriving (Show)

data InitProfileArgs = InitProfileArgs
  { ipaProfileData :: ProfileData,
    ipaProfileType :: ProfileType,
    ipaCreationDate :: GYTime,
    ipaOutputId :: Bool
  }
  deriving (Show)

data UpdateProfileImageArgs = UpdateProfileImageArgs
  { upiaProfileId :: GYAssetClass,
    upiaImageURI :: T.Text,
    upiaOutputId :: Bool
  }
  deriving (Show)

data PromoteProfileArgs = PromoteProfileArgs
  { ppaPromotedProfileId :: GYAssetClass,
    ppaPromotedByProfileId :: GYAssetClass,
    ppaAchievementDate :: GYTime,
    ppaPromotedBelt :: BJJBelt,
    ppaOutputId :: Bool
  }
  deriving (Show)

newtype AcceptPromotionArgs = AcceptPromotionArgs
  { apaPromotionId :: GYAssetClass
  }
  deriving (Show)

data CreateProfileWithRankArgs = CreateProfileWithRankArgs
  { cpwraProfileData :: ProfileData,
    cpwraProfileType :: ProfileType,
    cpwraCreationDate :: GYTime,
    cpwraBelt :: BJJBelt,
    cpwraOutputId :: Bool
  }
  deriving (Show)

data CreateMembershipHistoryArgs = CreateMembershipHistoryArgs
  { cmhaOrgProfileId :: GYAssetClass,
    cmhaPractitionerProfileId :: GYAssetClass,
    cmhaStartDate :: GYTime,
    cmhaEndDate :: Maybe GYTime,
    cmhaOutputId :: Bool
  }
  deriving (Show)

newtype GetFirstMembershipIntervalIdArgs = GetFirstMembershipIntervalIdArgs
  { gfmiiMembershipNodeId :: GYAssetClass
  }
  deriving (Show)

data AddMembershipIntervalArgs = AddMembershipIntervalArgs
  { amiaOrgProfileId :: GYAssetClass,
    amiaMembershipNodeId :: GYAssetClass,
    amiaStartDate :: GYTime,
    amiaEndDate :: Maybe GYTime,
    amiaOutputId :: Bool
  }
  deriving (Show)

data AcceptMembershipIntervalArgs = AcceptMembershipIntervalArgs
  { amiaIntervalId :: GYAssetClass,
    amiaAcceptOutputId :: Bool
  }
  deriving (Show)

data UpdateEndDateArgs = UpdateEndDateArgs
  { uedaIntervalId :: GYAssetClass,
    uedaHistoryNodeId :: GYAssetClass,
    uedaNewEndDate :: GYTime,
    uedaOutputId :: Bool
  }
  deriving (Show)

data AwardAchievementArgs = AwardAchievementArgs
  { awaAwardedToProfileId :: GYAssetClass,
    awaAwardedByProfileId :: GYAssetClass,
    awaProfileData :: ProfileData,
    awaAchievementDate :: GYTime,
    awaOutputId :: Bool
  }
  deriving (Show)

newtype AcceptAchievementArgs = AcceptAchievementArgs
  { acaAchievementId :: GYAssetClass
  }
  deriving (Show)

-- Profile data parser
profileDataParser :: Parser ProfileData
profileDataParser =
  ProfileData
    <$> fmap
      T.pack
      ( strOption
          ( long "name"
              <> metavar "NAME"
              <> help "Profile name"
          )
      )
    <*> fmap
      T.pack
      ( strOption
          ( long "description"
              <> metavar "DESCRIPTION"
              <> help "Profile description"
          )
      )
    <*> fmap
      T.pack
      ( strOption
          ( long "image-uri"
              <> metavar "IMAGE_URI"
              <> help "Profile image URI"
          )
      )

-- Profile type parser
profileTypeParser :: Parser ProfileType
profileTypeParser =
  flag'
    Practitioner
    ( long "practitioner"
        <> short 'p'
        <> help "Profile type (Practitioner or Organization)"
    )
    <|> flag'
      Organization
      ( long "organization"
          <> short 'o'
          <> help "Profile type (Practitioner or Organization)"
      )

-- POSIX time parser
posixTimeParser :: Parser GYTime
posixTimeParser =
  timeFromPlutus . POSIXTime
    <$> option
      auto
      ( long "posix"
          <> short 't'
          <> metavar "POSIX_TIME"
          <> help "POSIX timestamp"
      )

-- Optional POSIX time parser
optionalPosixTimeParser :: Parser (Maybe GYTime)
optionalPosixTimeParser =
  optional $
    timeFromPlutus . POSIXTime
      <$> option
        auto
        ( long "end-posix"
            <> metavar "END_POSIX_TIME"
            <> help "Optional end POSIX timestamp"
        )

-- Asset class parser helper function
parseAssetClass :: String -> Maybe GYAssetClass
parseAssetClass s = do
  -- Try to decode as JSON directly
  Just =<< Aeson.decode (B.fromStrict (LSB8.pack s))

-- Asset class parser
assetClassParser :: Parser GYAssetClass
assetClassParser =
  option
    (maybeReader parseAssetClass)
    ( long "asset-class"
        <> short 'a'
        <> metavar "ASSET_CLASS"
        <> help "Asset class identifier"
    )

-- BJJ Belt parser
bjjBeltParser :: Parser BJJBelt
bjjBeltParser =
  option
    (maybeReader parseBeltFromCLI)
    ( long "belt"
        <> short 'b'
        <> metavar "BELT"
        <> help "BJJ belt type (white, blue, purple, brown, black, black1, black2, black3, black4, black5, black6, red-and-black, red-and-white, red, red10)"
    )

-- Accept case-insensitive and hyphenated values: e.g., "black", "Red-And-Black" -> "Black", "RedAndBlack"
parseBeltFromCLI :: String -> Maybe BJJBelt
parseBeltFromCLI = parseBelt . concatMap capitalize . splitOnDash
  where
    splitOnDash :: String -> [String]
    splitOnDash s = case break (== '-') s of
      (chunk, []) -> [chunk]
      (chunk, _ : rest) -> chunk : splitOnDash rest

    capitalize :: String -> String
    capitalize [] = []
    capitalize (x : xs) = toUpper x : map toLower xs

-- Output ID flag parser
outputIdParser :: Parser Bool
outputIdParser =
  flag
    False
    True
    ( long "output-id"
        <> short 'o'
        <> help "Output the asset ID in a parseable format"
    )

-- Set fees parser (--clear-fees flag or individual fee fields)
setFeesParser :: Parser Command
setFeesParser =
  fmap SetFees $
    flag' ClearFees (long "clear-fees" <> help "Clear fee configuration (no fees)")
      <|> ( UpdateFees
              <$> strOption (long "fee-address" <> metavar "ADDRESS" <> help "Bech32 address to receive fees")
              <*> option auto (long "profile-fee" <> metavar "LOVELACE" <> help "Profile creation fee in lovelace")
              <*> option auto (long "promotion-fee" <> metavar "LOVELACE" <> help "Promotion fee in lovelace")
              <*> option auto (long "membership-history-fee" <> metavar "LOVELACE" <> help "Membership history fee in lovelace")
              <*> option auto (long "membership-interval-fee" <> metavar "LOVELACE" <> help "Membership interval fee in lovelace")
              <*> option auto (long "achievement-fee" <> metavar "LOVELACE" <> help "Achievement fee in lovelace")
          )

-- Main command parser
commandParser :: Parser Command
commandParser =
  hsubparser
    ( command
        "deploy-reference-scripts"
        ( info
            (pure DeployReferenceScripts)
            (progDesc "Deploy reference scripts for the BJJ belt system (includes oracle deployment)")
        )
        <> command
          "write-blueprint"
          ( info
              ( WriteBlueprint
                  <$> strOption
                    ( long "output"
                        <> short 'o'
                        <> metavar "FILE"
                        <> value Constants.defaultBlueprintFile
                        <> showDefault
                        <> help "Output file path for the CIP-57 contract blueprint JSON"
                    )
              )
              (progDesc "Write the CIP-57 contract blueprint JSON to a file")
          )
        <> command
          "pause-protocol"
          ( info
              (pure PauseProtocol)
              (progDesc "Pause the protocol (set opPaused = True in oracle)")
          )
        <> command
          "unpause-protocol"
          ( info
              (pure UnpauseProtocol)
              (progDesc "Unpause the protocol (set opPaused = False in oracle)")
          )
        <> command
          "set-fees"
          ( info
              setFeesParser
              (progDesc "Set or clear fee configuration in the oracle")
          )
        <> command
          "set-min-utxo-value"
          ( info
              (SetMinUTxOValue <$> option auto (long "lovelace" <> short 'n' <> metavar "LOVELACE" <> help "Minimum lovelace for protocol state outputs (oracle opMinUTxOValue)"))
              (progDesc "Set minimum UTxO value (lovelace) for protocol state outputs in the oracle")
          )
        <> command
          "query-oracle"
          ( info
              (pure QueryOracle)
              (progDesc "Display current oracle parameters (read-only)")
          )
        <> command
          "init-profile"
          ( info
              ( InitProfile
                  <$> ( InitProfileArgs
                          <$> profileDataParser
                          <*> profileTypeParser
                          <*> posixTimeParser
                          <*> outputIdParser
                      )
              )
              (progDesc "Initialize a new profile")
          )
        <> command
          "update-profile-image"
          ( info
              ( UpdateProfileImage
                  <$> ( UpdateProfileImageArgs
                          <$> assetClassParser
                          <*> fmap T.pack (strOption (long "image-uri" <> metavar "IMAGE_URI" <> help "New image URI"))
                          <*> outputIdParser
                      )
              )
              (progDesc "Update profile image")
          )
        <> command
          "promote-profile"
          ( info
              ( PromoteProfile
                  <$> ( PromoteProfileArgs
                          <$> option (maybeReader parseAssetClass) (long "promoted-profile-id" <> short 'p' <> metavar "PROMOTED_PROFILE_ID" <> help "ID of the profile being promoted")
                          <*> option (maybeReader parseAssetClass) (long "promoted-by-profile-id" <> short 'b' <> metavar "PROMOTED_BY_PROFILE_ID" <> help "ID of the profile doing the promotion")
                          <*> posixTimeParser
                          <*> bjjBeltParser
                          <*> outputIdParser
                      )
              )
              (progDesc "Promote a profile to a new belt")
          )
        <> command
          "accept-promotion"
          ( info
              (AcceptPromotion . AcceptPromotionArgs <$> assetClassParser)
              (progDesc "Accept a promotion")
          )
        <> command
          "create-profile-with-rank"
          ( info
              ( CreateProfileWithRank
                  <$> ( CreateProfileWithRankArgs
                          <$> profileDataParser
                          <*> profileTypeParser
                          <*> posixTimeParser
                          <*> bjjBeltParser
                          <*> outputIdParser
                      )
              )
              (progDesc "Create a profile with initial rank")
          )
        <> command
          "create-membership-history"
          ( info
              ( CreateMembershipHistory
                  <$> ( CreateMembershipHistoryArgs
                          <$> option (maybeReader parseAssetClass) (long "org-profile-id" <> metavar "ORG_PROFILE_ID" <> help "Organization profile asset class")
                          <*> option (maybeReader parseAssetClass) (long "practitioner-profile-id" <> metavar "PRACTITIONER_PROFILE_ID" <> help "Practitioner profile asset class")
                          <*> posixTimeParser
                          <*> optionalPosixTimeParser
                          <*> outputIdParser
                      )
              )
              (progDesc "Create a new membership history for a practitioner at an organization")
          )
        <> command
          "get-first-interval-id"
          ( info
              (GetFirstMembershipIntervalId . GetFirstMembershipIntervalIdArgs <$> option (maybeReader parseAssetClass) (long "membership-node-id" <> metavar "MEMBERSHIP_NODE_ID" <> help "Membership history node asset class"))
              (progDesc "Get the first (interval 0) membership interval ID for a membership history node (for accepting before adding next interval)")
          )
        <> command
          "add-membership-interval"
          ( info
              ( AddMembershipInterval
                  <$> ( AddMembershipIntervalArgs
                          <$> option (maybeReader parseAssetClass) (long "org-profile-id" <> metavar "ORG_PROFILE_ID" <> help "Organization profile asset class")
                          <*> option (maybeReader parseAssetClass) (long "membership-node-id" <> metavar "MEMBERSHIP_NODE_ID" <> help "Membership history node asset class")
                          <*> posixTimeParser
                          <*> optionalPosixTimeParser
                          <*> outputIdParser
                      )
              )
              (progDesc "Add a new membership interval to an existing membership history")
          )
        <> command
          "accept-membership-interval"
          ( info
              ( AcceptMembershipInterval
                  <$> ( AcceptMembershipIntervalArgs
                          <$> option (maybeReader parseAssetClass) (long "interval-id" <> short 'i' <> metavar "INTERVAL_ID" <> help "Membership interval asset class")
                          <*> outputIdParser
                      )
              )
              (progDesc "Accept a membership interval (practitioner acknowledges membership)")
          )
        <> command
          "update-end-date"
          ( info
              ( UpdateEndDate
                  <$> ( UpdateEndDateArgs
                          <$> option (maybeReader parseAssetClass) (long "interval-id" <> short 'i' <> metavar "INTERVAL_ID" <> help "Membership interval asset class")
                          <*> option (maybeReader parseAssetClass) (long "history-node-id" <> metavar "HISTORY_NODE_ID" <> help "Membership history node asset class (reference input)")
                          <*> posixTimeParser
                          <*> outputIdParser
                      )
              )
              (progDesc "Update membership interval end date (org or practitioner; new end date must be in the future)")
          )
        <> command
          "award-achievement"
          ( info
              ( AwardAchievement
                  <$> ( AwardAchievementArgs
                          <$> option (maybeReader parseAssetClass) (long "awarded-to-profile-id" <> metavar "AWARDED_TO_PROFILE_ID" <> help "Profile ID of the practitioner receiving the achievement")
                          <*> option (maybeReader parseAssetClass) (long "awarded-by-profile-id" <> metavar "AWARDED_BY_PROFILE_ID" <> help "Profile ID of the awarder (org or practitioner)")
                          <*> profileDataParser
                          <*> posixTimeParser
                          <*> outputIdParser
                      )
              )
              (progDesc "Award an achievement to a practitioner")
          )
        <> command
          "accept-achievement"
          ( info
              (AcceptAchievement . AcceptAchievementArgs <$> option (maybeReader parseAssetClass) (long "achievement-id" <> short 'i' <> metavar "ACHIEVEMENT_ID" <> help "Achievement asset class"))
              (progDesc "Accept an achievement (practitioner acknowledges)")
          )
        <> command
          "cleanup-dust"
          ( info
              (pure CleanupDust)
              (progDesc "Sweep dust/griefing UTxOs from validator addresses (permissionless — anyone can run this)")
          )
    )

-- Action type conversion functions
initProfileToActionType :: InitProfileArgs -> ActionType
initProfileToActionType InitProfileArgs {ipaProfileData, ipaProfileType, ipaCreationDate} =
  ProfileAction $ InitProfileAction ipaProfileData ipaProfileType ipaCreationDate

updateProfileImageToActionType :: UpdateProfileImageArgs -> ActionType
updateProfileImageToActionType UpdateProfileImageArgs {upiaProfileId, upiaImageURI} =
  ProfileAction $ UpdateProfileImageAction upiaProfileId upiaImageURI

promoteProfileToActionType :: PromoteProfileArgs -> ActionType
promoteProfileToActionType PromoteProfileArgs {ppaPromotedProfileId, ppaPromotedByProfileId, ppaAchievementDate, ppaPromotedBelt} =
  ProfileAction $ PromoteProfileAction ppaPromotedProfileId ppaPromotedByProfileId ppaAchievementDate ppaPromotedBelt

acceptPromotionToActionType :: AcceptPromotionArgs -> ActionType
acceptPromotionToActionType AcceptPromotionArgs {apaPromotionId} =
  ProfileAction $ AcceptPromotionAction apaPromotionId

createProfileWithRankToActionType :: CreateProfileWithRankArgs -> ActionType
createProfileWithRankToActionType CreateProfileWithRankArgs {cpwraProfileData, cpwraProfileType, cpwraCreationDate, cpwraBelt} =
  ProfileAction $ CreateProfileWithRankAction cpwraProfileData cpwraProfileType cpwraCreationDate cpwraBelt

createMembershipHistoryToActionType :: CreateMembershipHistoryArgs -> ActionType
createMembershipHistoryToActionType CreateMembershipHistoryArgs {cmhaOrgProfileId, cmhaPractitionerProfileId, cmhaStartDate, cmhaEndDate} =
  ProfileAction $ CreateMembershipHistoryAction cmhaOrgProfileId cmhaPractitionerProfileId cmhaStartDate cmhaEndDate

addMembershipIntervalToActionType :: AddMembershipIntervalArgs -> ActionType
addMembershipIntervalToActionType AddMembershipIntervalArgs {amiaOrgProfileId, amiaMembershipNodeId, amiaStartDate, amiaEndDate} =
  ProfileAction $ AddMembershipIntervalAction amiaOrgProfileId amiaMembershipNodeId amiaStartDate amiaEndDate

acceptMembershipIntervalToActionType :: AcceptMembershipIntervalArgs -> ActionType
acceptMembershipIntervalToActionType AcceptMembershipIntervalArgs {amiaIntervalId} =
  ProfileAction $ AcceptMembershipIntervalAction amiaIntervalId

updateEndDateToActionType :: UpdateEndDateArgs -> ActionType
updateEndDateToActionType UpdateEndDateArgs {uedaIntervalId, uedaHistoryNodeId, uedaNewEndDate} =
  ProfileAction $ UpdateEndDateAction uedaIntervalId uedaHistoryNodeId uedaNewEndDate

awardAchievementToActionType :: AwardAchievementArgs -> ActionType
awardAchievementToActionType AwardAchievementArgs {awaAwardedToProfileId, awaAwardedByProfileId, awaProfileData, awaAchievementDate} =
  ProfileAction $ AwardAchievementAction awaAwardedToProfileId awaAwardedByProfileId awaProfileData [] awaAchievementDate

acceptAchievementToActionType :: AcceptAchievementArgs -> ActionType
acceptAchievementToActionType AcceptAchievementArgs {acaAchievementId} =
  ProfileAction $ AcceptAchievementAction acaAchievementId

-- | Convert SetFeesArgs to the AdminActionType for the Interaction pipeline
setFeesToAdminAction :: SetFeesArgs -> AdminActionType
setFeesToAdminAction ClearFees = SetFeesAction Nothing
setFeesToAdminAction UpdateFees {..} =
  let gyAddr = unsafeAddressFromText (T.pack sfaFeeAddress)
      plutusAddr = addressToPlutus gyAddr
   in SetFeesAction $
        Just
          FeeConfig
            { fcFeeAddress = plutusAddr,
              fcProfileCreationFee = sfaProfileCreationFee,
              fcPromotionFee = sfaPromotionFee,
              fcMembershipHistoryFee = sfaMembershipHistoryFee,
              fcMembershipIntervalFee = sfaMembershipIntervalFee,
              fcAchievementFee = sfaAchievementFee
            }

-- Execute command function
executeCommand :: Either ProviderCtx TxBuildingContext -> GYExtendedPaymentSigningKey -> Command -> IO ()
executeCommand (Left pCtx) signKey cmd = case cmd of
  WriteBlueprint _ -> pure () -- Handled in main before provider loading
  QueryOracle -> do
    printYellow "No transaction building context found."
    printYellow "Please run 'deploy-reference-scripts' first to set up the system."
  DeployReferenceScripts -> do
    printYellow "Deploying reference scripts..."
    deployedScriptsCtx <- deployReferenceScripts pCtx signKey
    B.writeFile Constants.defaultTxBuildingContextFile (encode . toJSON $ deployedScriptsCtx)
    printGreen $ "Reference scripts deployed successfully! \n\t" <> "File: " <> Constants.defaultTxBuildingContextFile
  _ -> do
    printYellow "No transaction building context found."
    printYellow "Please run 'deploy-reference-scripts' first to set up the system."
executeCommand (Right txBuildingCtx) signKey cmd = case cmd of
  WriteBlueprint _ -> pure () -- Handled in main before provider loading
  DeployReferenceScripts -> do
    printYellow "Deploying reference scripts..."
    deployedScriptsCtx <- deployReferenceScripts (providerCtx txBuildingCtx) signKey
    B.writeFile Constants.defaultTxBuildingContextFile (encode . toJSON $ deployedScriptsCtx)
    printGreen $ "Reference scripts deployed successfully! \n\t" <> "File: " <> Constants.defaultTxBuildingContextFile
  -- Oracle admin commands — routed through the Interaction pipeline
  PauseProtocol -> do
    printYellow "Pausing protocol..."
    (txId, _) <- runBJJActionWithPK txBuildingCtx signKey (AdminAction PauseProtocolAction) Nothing
    printGreen $ "Protocol paused successfully! TxId: " <> show txId
  UnpauseProtocol -> do
    printYellow "Unpausing protocol..."
    (txId, _) <- runBJJActionWithPK txBuildingCtx signKey (AdminAction UnpauseProtocolAction) Nothing
    printGreen $ "Protocol unpaused successfully! TxId: " <> show txId
  SetFees args -> do
    printYellow "Updating fee configuration..."
    let adminAction = setFeesToAdminAction args
    (txId, _) <- runBJJActionWithPK txBuildingCtx signKey (AdminAction adminAction) Nothing
    printGreen $ "Fee configuration updated successfully! TxId: " <> show txId
  SetMinUTxOValue n -> do
    printYellow $ "Setting min UTxO value to " <> show n <> " lovelace..."
    (txId, _) <- runBJJActionWithPK txBuildingCtx signKey (AdminAction (SetMinUTxOValueAction n)) Nothing
    printGreen $ "Min UTxO value updated successfully! TxId: " <> show txId
  -- Query oracle — read-only, does not go through the Interaction pipeline
  QueryOracle -> do
    printYellow "Querying oracle parameters..."
    let pCtx = providerCtx txBuildingCtx
        dCtx = deployedScriptsCtx txBuildingCtx
    (oracleParams, oracleRef, oracleVal) <- runQuery pCtx $ runReaderT queryOracleParams dCtx
    printGreen "=== Oracle Parameters ==="
    printGreen $ "  UTxO Ref:           " <> show oracleRef
    printGreen $ "  Value:              " <> show oracleVal
    printGreen $ "  Admin PKH:          " <> show (opAdminPkh oracleParams)
    printGreen $ "  Paused:             " <> show (opPaused oracleParams)
    printGreen $ "  Min UTxO Value:     " <> show (opMinUTxOValue oracleParams) <> " lovelace"
    case opFeeConfig oracleParams of
      Nothing -> printGreen "  Fee Config:          None"
      Just fc -> do
        printGreen "  Fee Config:"
        printGreen $ "    Fee Address:         " <> show (fcFeeAddress fc)
        printGreen $ "    Profile Creation Fee: " <> show (fcProfileCreationFee fc) <> " lovelace"
        printGreen $ "    Promotion Fee:        " <> show (fcPromotionFee fc) <> " lovelace"
        printGreen $ "    Membership History Fee: " <> show (fcMembershipHistoryFee fc) <> " lovelace"
        printGreen $ "    Membership Interval Fee: " <> show (fcMembershipIntervalFee fc) <> " lovelace"
        printGreen $ "    Achievement Fee:       " <> show (fcAchievementFee fc) <> " lovelace"
  -- Profile commands — routed through the Interaction pipeline
  InitProfile args -> do
    printYellow "Initializing profile..."
    let actionType = initProfileToActionType args
    (_txId, mAssetClass) <- runBJJActionWithPK txBuildingCtx signKey actionType Nothing
    printGreen "Profile initialized successfully!"
    if ipaOutputId args
      then putStrLn $ LSB8.unpack $ LSB8.toStrict $ Aeson.encode mAssetClass
      else printGreen $ "Profile ID: " <> LSB8.unpack (LSB8.toStrict (Aeson.encode mAssetClass))
  UpdateProfileImage args -> do
    printYellow "Updating profile image..."
    let actionType = updateProfileImageToActionType args
    (_txId, mAssetClass) <- runBJJActionWithPK txBuildingCtx signKey actionType Nothing
    printGreen "Profile image updated successfully!"
    if upiaOutputId args
      then putStrLn $ LSB8.unpack $ LSB8.toStrict $ Aeson.encode mAssetClass
      else printGreen $ "Profile ID: " <> LSB8.unpack (LSB8.toStrict (Aeson.encode mAssetClass))
  PromoteProfile args -> do
    printYellow "Promoting profile..."
    let actionType = promoteProfileToActionType args
    (_txId, mAssetClass) <- runBJJActionWithPK txBuildingCtx signKey actionType Nothing
    printGreen "Profile promoted successfully!"
    if ppaOutputId args
      then putStrLn $ LSB8.unpack $ LSB8.toStrict $ Aeson.encode mAssetClass
      else printGreen $ "Promotion ID: " <> LSB8.unpack (LSB8.toStrict (Aeson.encode mAssetClass))
  AcceptPromotion args -> do
    printYellow "Accepting promotion..."
    let actionType = acceptPromotionToActionType args
    (_txId, mAssetClass) <- runBJJActionWithPK txBuildingCtx signKey actionType Nothing
    printGreen "Promotion accepted successfully!"
    printGreen $ "Rank ID: " <> LSB8.unpack (LSB8.toStrict (Aeson.encode mAssetClass))
  CreateProfileWithRank args -> do
    printYellow "Creating profile with rank..."
    let actionType = createProfileWithRankToActionType args
    (_txId, mAssetClass) <- runBJJActionWithPK txBuildingCtx signKey actionType Nothing
    printGreen "Profile with rank created successfully!"
    if cpwraOutputId args
      then putStrLn $ LSB8.unpack $ LSB8.toStrict $ Aeson.encode mAssetClass
      else printGreen $ "Profile ID: " <> LSB8.unpack (LSB8.toStrict (Aeson.encode mAssetClass))
  -- Membership commands — routed through the Interaction pipeline
  CreateMembershipHistory args -> do
    printYellow "Creating membership history..."
    let actionType = createMembershipHistoryToActionType args
    (_txId, mAssetClass) <- runBJJActionWithPK txBuildingCtx signKey actionType Nothing
    printGreen "Membership history created successfully!"
    if cmhaOutputId args
      then putStrLn $ LSB8.unpack $ LSB8.toStrict $ Aeson.encode mAssetClass
      else printGreen $ "Membership History ID: " <> LSB8.unpack (LSB8.toStrict (Aeson.encode mAssetClass))
  GetFirstMembershipIntervalId args -> do
    printYellow "Looking up first interval ID for membership node..."
    let pCtx = providerCtx txBuildingCtx
        dCtx = deployedScriptsCtx txBuildingCtx
    firstIntervalId <- runQuery pCtx $ runReaderT (getFirstIntervalIdForMembershipNode (gfmiiMembershipNodeId args)) dCtx
    printGreen "First interval ID:"
    putStrLn $ LSB8.unpack $ LSB8.toStrict $ Aeson.encode firstIntervalId
  AddMembershipInterval args -> do
    printYellow "Adding membership interval..."
    let actionType = addMembershipIntervalToActionType args
    (_txId, mAssetClass) <- runBJJActionWithPK txBuildingCtx signKey actionType Nothing
    printGreen "Membership interval added successfully!"
    if amiaOutputId args
      then putStrLn $ LSB8.unpack $ LSB8.toStrict $ Aeson.encode mAssetClass
      else printGreen $ "Interval ID: " <> LSB8.unpack (LSB8.toStrict (Aeson.encode mAssetClass))
  AcceptMembershipInterval args -> do
    printYellow "Accepting membership interval..."
    let actionType = acceptMembershipIntervalToActionType args
    (_txId, mAssetClass) <- runBJJActionWithPK txBuildingCtx signKey actionType Nothing
    printGreen "Membership interval accepted successfully!"
    if amiaAcceptOutputId args
      then putStrLn $ LSB8.unpack $ LSB8.toStrict $ Aeson.encode mAssetClass
      else printGreen $ "Interval ID: " <> LSB8.unpack (LSB8.toStrict (Aeson.encode mAssetClass))
  UpdateEndDate args -> do
    printYellow "Updating membership interval end date..."
    let actionType = updateEndDateToActionType args
    (_txId, mAssetClass) <- runBJJActionWithPK txBuildingCtx signKey actionType Nothing
    printGreen "Membership interval end date updated successfully!"
    if uedaOutputId args
      then putStrLn $ LSB8.unpack $ LSB8.toStrict $ Aeson.encode mAssetClass
      else printGreen $ "Interval ID: " <> LSB8.unpack (LSB8.toStrict (Aeson.encode mAssetClass))
  -- Achievement commands — routed through the Interaction pipeline
  AwardAchievement args -> do
    printYellow "Awarding achievement..."
    let actionType = awardAchievementToActionType args
    (_txId, mAssetClass) <- runBJJActionWithPK txBuildingCtx signKey actionType Nothing
    printGreen "Achievement awarded successfully!"
    if awaOutputId args
      then putStrLn $ LSB8.unpack $ LSB8.toStrict $ Aeson.encode mAssetClass
      else printGreen $ "Achievement ID: " <> LSB8.unpack (LSB8.toStrict (Aeson.encode mAssetClass))
  AcceptAchievement args -> do
    printYellow "Accepting achievement..."
    let actionType = acceptAchievementToActionType args
    (_txId, mAssetClass) <- runBJJActionWithPK txBuildingCtx signKey actionType Nothing
    printGreen "Achievement accepted successfully!"
    printGreen $ "Achievement ID: " <> LSB8.unpack (LSB8.toStrict (Aeson.encode mAssetClass))
  CleanupDust -> do
    printYellow "Sweeping dust UTxOs from validator addresses..."
    let actionType = ProtocolAction CleanupDustAction
    (txId, _) <- runBJJActionWithPK txBuildingCtx signKey actionType Nothing
    printGreen $ "Dust cleanup successful! TxId: " <> show txId

main :: IO ()
main = do
  printGreen "BJJ Belt System - Decentralized Belt Management"

  -- Parse command line arguments
  cmd <-
    execParser $
      info
        (commandParser <**> helper)
        ( fullDesc
            <> progDesc "A command-line tool for managing Brazilian Jiu Jitsu profiles, belt promotions, and achievements on the Cardano blockchain. Supports deploying reference scripts, writing CIP-57 blueprints, initializing and updating profiles, handling promotions, and more."
            <> header "BJJ Belt System - Decentralized Belt Management"
        )

  -- Handle commands that don't require blockchain providers
  case cmd of
    WriteBlueprint outputPath -> do
      printYellow $ "Writing CIP-57 contract blueprint to " <> outputPath <> " ..."
      B.writeFile outputPath (encode $ contractBlueprint blueprintProtocolParams)
      printGreen $ "Blueprint written successfully to " <> outputPath
    _ -> executeOnchainCommand cmd

-- | Execute commands that require blockchain providers and signing keys
executeOnchainCommand :: Command -> IO ()
executeOnchainCommand cmd = do
  mTxBuildingContext <- decodeConfigEnvOrFile "DEPLOYED_VALIDATORS_CONFIG" Constants.defaultTxBuildingContextFile
  case mTxBuildingContext of
    Nothing -> do
      printYellow "No transaction building context found, please run deploy-reference-scripts first"
    Just _txBuildingContext -> do
      printYellow "Transaction building context found, executing command"

  printYellow $ "Reading signing key file from " <> mnemonicFilePath
  signKey <- readMnemonicFile mnemonicFilePath

  printYellow "Reading atlas configuration file ..."
  atlasConfig <- maybe (die "Atlas configuration file not found") return =<< decodeConfigEnvOrFile "ATLAS_CORE_CONFIG" Constants.defaultAtlasCoreConfig

  printYellow "Loading Providers ..."
  withCfgProviders atlasConfig (Text.read @GYLogNamespace "bjj-belt-system") $ \providers -> do
    let pCtx = ProviderCtx atlasConfig providers

    case mTxBuildingContext of
      Nothing -> do
        printYellow "No deployed scripts context found, please run deploy-reference-scripts first"
        executeCommand (Left pCtx) signKey cmd
      Just validatorsCtx -> do
        let txBuildingContext = TxBuildingContext {deployedScriptsCtx = validatorsCtx, providerCtx = pCtx}
        executeCommand (Right txBuildingContext) signKey cmd
