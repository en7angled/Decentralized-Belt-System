module Main where

import Data.Char (toLower)
import Data.Text qualified as T
import Data.Word (Word8)
import DomainTypes.Profile.Types (ProfileActionType (..), ProfileData (..), ProfileType (..))
import GeniusYield.GYConfig
import GeniusYield.Types
import Onchain.BJJ (BJJBelt(..))
import Onchain.Blueprint (contractBlueprint, mintingPolicyBlueprint)
import Onchain.Protocol (ProtocolParams (..))
import Options.Applicative
import PlutusLedgerApi.V3
import PlutusTx.Blueprint (writeBlueprint)
import System.Environment (getArgs)
import Text.Read qualified as Text
import TxBuilding.Context
import TxBuilding.Interactions
import TxBuilding.Transactions
import TxBuilding.Utils
import TxBuilding.Validators (defaultProtocolParams)
import Utils
import Data.ByteString.Lazy qualified as B
import GeniusYield.Imports
import Data.Aeson (encode, decodeFileStrict)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as LSB8



atlasCoreConfig :: FilePath
atlasCoreConfig = "config_atlas.json"

txBuldingContextFile :: FilePath
txBuldingContextFile = "config_bjj_validators.json"

mnemonicFilePath :: FilePath
mnemonicFilePath = "operation.prv"

printYellow :: String -> IO ()
printYellow = putStrLn . yellowColorString

printGreen :: String -> IO ()
printGreen = putStrLn . greenColorString

-- Command data types
data Command
  = DeployReferenceScripts
  | InitProfile InitProfileArgs
  | UpdateProfileImage UpdateProfileImageArgs
  | DeleteProfile DeleteProfileArgs
  | PromoteProfile PromoteProfileArgs
  | AcceptPromotion AcceptPromotionArgs
  | CreateProfileWithRank CreateProfileWithRankArgs
  deriving (Show)

data InitProfileArgs = InitProfileArgs
  { ipaProfileData :: ProfileData
  , ipaProfileType :: ProfileType
  , ipaCreationDate :: GYTime
  , ipaOutputId :: Bool
  } deriving (Show)

data UpdateProfileImageArgs = UpdateProfileImageArgs
  { upiaProfileId :: GYAssetClass
  , upiaImageURI :: T.Text
  , upiaOutputId :: Bool
  } deriving (Show)

newtype DeleteProfileArgs = DeleteProfileArgs
  { dpaProfileId :: GYAssetClass
  } deriving (Show)

data PromoteProfileArgs = PromoteProfileArgs
  { ppaPromotedProfileId :: GYAssetClass
  , ppaPromotedByProfileId :: GYAssetClass
  , ppaAchievementDate :: GYTime
  , ppaPromotedBelt :: BJJBelt
  , ppaOutputId :: Bool
  } deriving (Show)

newtype AcceptPromotionArgs = AcceptPromotionArgs
  { apaPromotionId :: GYAssetClass
  } deriving (Show)

data CreateProfileWithRankArgs = CreateProfileWithRankArgs
  { cpwraProfileData :: ProfileData
  , cpwraProfileType :: ProfileType
  , cpwraCreationDate :: GYTime
  , cpwraBelt :: BJJBelt
  , cpwraOutputId :: Bool
  } deriving (Show)

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

-- Asset class parser helper function
parseAssetClass :: String -> Maybe GYAssetClass
parseAssetClass s = do
  -- Try to decode as JSON directly
  case Aeson.decode (B.fromStrict (LSB8.pack s)) of
    Just assetClass -> Just assetClass
    Nothing -> Nothing

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
    (maybeReader parseBelt)
    ( long "belt"
        <> short 'b'
        <> metavar "BELT"
        <> help "BJJ belt type (white, blue, purple, brown, black, black1, black2, black3, black4, black5, black6, red-and-black, red-and-white, red, red10)"
    )
  where
    parseBelt :: String -> Maybe BJJBelt
    parseBelt s = case map toLower s of
      "white" -> Just White
      "blue" -> Just Blue
      "purple" -> Just Purple
      "brown" -> Just Brown
      "black" -> Just Black
      "black1" -> Just Black1
      "black2" -> Just Black2
      "black3" -> Just Black3
      "black4" -> Just Black4
      "black5" -> Just Black5
      "black6" -> Just Black6
      "red-and-black" -> Just RedAndBlack
      "red-and-white" -> Just RedAndWhite
      "red" -> Just Red
      "red10" -> Just Red10
      _ -> Nothing

-- Output ID flag parser
outputIdParser :: Parser Bool
outputIdParser = flag False True
  ( long "output-id"
      <> short 'o'
      <> help "Output the asset ID in a parseable format"
  )

-- Main command parser
commandParser :: Parser Command
commandParser =
  hsubparser
    ( command "deploy-reference-scripts"
        ( info (pure DeployReferenceScripts)
            ( progDesc "Deploy reference scripts for the BJJ belt system"
            )
        )
        <> command "init-profile"
        ( info ( InitProfile
                  <$> ( InitProfileArgs
                          <$> profileDataParser
                          <*> profileTypeParser
                          <*> posixTimeParser
                          <*> outputIdParser
                      )
              )
              ( progDesc "Initialize a new profile"
              )
        )
        <> command "update-profile-image"
        ( info ( UpdateProfileImage
                  <$> ( UpdateProfileImageArgs
                          <$> assetClassParser
                          <*> fmap T.pack (strOption (long "image-uri" <> metavar "IMAGE_URI" <> help "New image URI"))
                          <*> outputIdParser
                      )
              )
              ( progDesc "Update profile image"
              )
        )
        <> command "delete-profile"
        ( info ( DeleteProfile . DeleteProfileArgs <$> assetClassParser
              )
              ( progDesc "Delete a profile"
              )
        )
        <> command "promote-profile"
        ( info ( PromoteProfile
                  <$> ( PromoteProfileArgs
                          <$> option (maybeReader parseAssetClass) (long "promoted-profile-id" <> short 'p' <> metavar "PROMOTED_PROFILE_ID" <> help "ID of the profile being promoted")
                          <*> option (maybeReader parseAssetClass) (long "promoted-by-profile-id" <> short 'b' <> metavar "PROMOTED_BY_PROFILE_ID" <> help "ID of the profile doing the promotion")
                          <*> posixTimeParser
                          <*> bjjBeltParser
                          <*> outputIdParser
                      )
              )
              ( progDesc "Promote a profile to a new belt"
              )
        )
        <> command "accept-promotion"
        ( info ( AcceptPromotion . AcceptPromotionArgs <$> assetClassParser
              )
              ( progDesc "Accept a promotion"
              )
        )
        <> command "create-profile-with-rank"
        ( info ( CreateProfileWithRank
                  <$> ( CreateProfileWithRankArgs
                          <$> profileDataParser
                          <*> profileTypeParser
                          <*> posixTimeParser
                          <*> bjjBeltParser
                          <*> outputIdParser
                      )
              )
              ( progDesc "Create a profile with initial rank"
              )
        )
    )

-- Action type conversion functions
initProfileToActionType :: InitProfileArgs -> ActionType
initProfileToActionType InitProfileArgs{ipaProfileData, ipaProfileType, ipaCreationDate} =
  ProfileAction $ InitProfileAction ipaProfileData ipaProfileType ipaCreationDate

updateProfileImageToActionType :: UpdateProfileImageArgs -> ActionType
updateProfileImageToActionType UpdateProfileImageArgs{upiaProfileId, upiaImageURI} =
  ProfileAction $ UpdateProfileImageAction upiaProfileId upiaImageURI

deleteProfileToActionType :: DeleteProfileArgs -> ActionType
deleteProfileToActionType DeleteProfileArgs{dpaProfileId} =
  ProfileAction $ DeleteProfileAction dpaProfileId

promoteProfileToActionType :: PromoteProfileArgs -> ActionType
promoteProfileToActionType PromoteProfileArgs{ppaPromotedProfileId, ppaPromotedByProfileId, ppaAchievementDate, ppaPromotedBelt} =
  ProfileAction $ PromoteProfileAction ppaPromotedProfileId ppaPromotedByProfileId ppaAchievementDate ppaPromotedBelt

acceptPromotionToActionType :: AcceptPromotionArgs -> ActionType
acceptPromotionToActionType AcceptPromotionArgs{apaPromotionId} =
  ProfileAction $ AcceptPromotionAction apaPromotionId

createProfileWithRankToActionType :: CreateProfileWithRankArgs -> ActionType
createProfileWithRankToActionType CreateProfileWithRankArgs{cpwraProfileData, cpwraProfileType, cpwraCreationDate, cpwraBelt} =
  ProfileAction $ CreateProfileWithRankAction cpwraProfileData cpwraProfileType cpwraCreationDate cpwraBelt

-- Execute command function
executeCommand :: Either ProviderCtx TxBuildingContext -> GYExtendedPaymentSigningKey -> Command -> IO ()
executeCommand (Left pCtx) signKey cmd = case cmd of
  DeployReferenceScripts -> do
    printYellow "Deploying reference scripts..."
    txBuildingCtx <- deployReferenceScripts pCtx signKey
    B.writeFile txBuldingContextFile (encode . toJSON $ txBuildingCtx)
    printGreen $ "Reference scripts deployed successfully! \n\t" <>  "File: " <> txBuldingContextFile

executeCommand (Right txBuildingCtx) signKey cmd = case cmd of
  InitProfile args -> do
    printYellow "Initializing profile..."
    let actionType = initProfileToActionType args
    (txId, assetClass) <- runBJJActionWithPK txBuildingCtx signKey actionType Nothing
    printGreen "Profile initialized successfully!"
    if ipaOutputId args
      then putStrLn $ LSB8.unpack $ LSB8.toStrict  $ Aeson.encode assetClass
      else printGreen $ "Profile ID: " <> LSB8.unpack (LSB8.toStrict (Aeson.encode assetClass))

  UpdateProfileImage args -> do
    printYellow "Updating profile image..."
    let actionType = updateProfileImageToActionType args
    (txId, assetClass) <- runBJJActionWithPK txBuildingCtx signKey actionType Nothing
    printGreen "Profile image updated successfully!"
    if upiaOutputId args
      then putStrLn $ LSB8.unpack $ LSB8.toStrict  $ Aeson.encode assetClass
      else printGreen $ "Profile ID: " <> LSB8.unpack (LSB8.toStrict (Aeson.encode assetClass))

  DeleteProfile args -> do
    printYellow "Deleting profile..."
    let actionType = deleteProfileToActionType args
    (txId, assetClass) <- runBJJActionWithPK txBuildingCtx signKey actionType Nothing
    printGreen "Profile deleted successfully!"
    printGreen $ "Profile ID: " <> LSB8.unpack (LSB8.toStrict (Aeson.encode assetClass))

  PromoteProfile args -> do
    printYellow "Promoting profile..."
    let actionType = promoteProfileToActionType args
    (txId, assetClass) <- runBJJActionWithPK txBuildingCtx signKey actionType Nothing
    printGreen "Profile promoted successfully!"
    if ppaOutputId args
      then putStrLn $ LSB8.unpack $ LSB8.toStrict  $ Aeson.encode assetClass
      else printGreen $ "Promotion ID: " <> LSB8.unpack (LSB8.toStrict (Aeson.encode assetClass))

  AcceptPromotion args -> do
    printYellow "Accepting promotion..."
    let actionType = acceptPromotionToActionType args
    (txId, assetClass) <- runBJJActionWithPK txBuildingCtx signKey actionType Nothing
    printGreen "Promotion accepted successfully!"
    printGreen $ "Rank ID: " <> LSB8.unpack (LSB8.toStrict (Aeson.encode assetClass))

  CreateProfileWithRank args -> do
    printYellow "Creating profile with rank..."
    let actionType = createProfileWithRankToActionType args
    (txId, assetClass) <- runBJJActionWithPK txBuildingCtx signKey actionType Nothing
    printGreen "Profile with rank created successfully!"
    if cpwraOutputId args
      then putStrLn $ LSB8.unpack $ LSB8.toStrict  $ Aeson.encode assetClass
      else printGreen $ "Profile ID: " <> LSB8.unpack (LSB8.toStrict (Aeson.encode assetClass))

main :: IO ()
main = do
  printGreen "BJJ Belt System - Decentralized Belt Management"

  mTxBuildingContext <- decodeConfigFile @ProfileTxBuildingContext txBuldingContextFile
  case mTxBuildingContext of
    Nothing -> do
      printYellow "No transaction building context found, please run deploy-reference-scripts first"
      printYellow "Please run deploy-reference-scripts first to set up the system"
    Just txBuildingContext -> do
      printYellow "Transaction building context found, executing command"

  -- Parse command line arguments
  cmd <- execParser $ info (commandParser <**> helper)
    ( fullDesc
        <> progDesc "A command-line tool for managing Brazilian Jiu Jitsu profiles, belt promotions, and achievements on the Cardano blockchain. Supports deploying reference scripts, initializing and updating profiles, handling promotions, and more."
        <> header "BJJ Belt System - Decentralized Belt Management"
    )


  printYellow $ "Reading signing key file from " <> mnemonicFilePath
  signKey <- readMnemonicFile mnemonicFilePath

  printYellow "Reading atlas configuration file ..."
  atlasConfig <- maybe (error "Atlas configuration file not found") return =<< decodeConfigFile @GYCoreConfig atlasCoreConfig

  printYellow "Loading Providers ..."
  withCfgProviders atlasConfig (Text.read @GYLogNamespace "bjj-belt-system") $ \providers -> do
    let pCtx = ProviderCtx atlasConfig providers

    case mTxBuildingContext of
      Nothing -> do

        executeCommand (Left pCtx) signKey cmd
      Just validatorsCtx -> do
        let txBuildingContext = TxBuildingContext {validatorsCtx = validatorsCtx, providerCtx = pCtx}
        executeCommand (Right txBuildingContext) signKey cmd

