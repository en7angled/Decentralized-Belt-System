{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Wire response types for the query API that include @thumbnail_uri@. Domain
-- types in offchain-lib store image URIs only; this module maps to JSON and
-- derives thumbnails via a query-local helper (today @image_uri@; may later
-- resolve cached or CDN URLs).
--
-- JSON uses @StripPrefix@ + @CamelToSnake@ (see 'ProfileResponse', 'LineageGraphData').
module DomainTypes.Transfer.QueryResponses
  ( ProfileResponse (..),
    AchievementResponse (..),
    PractitionerProfileResponse (..),
    OrganizationProfileResponse (..),
    profileToResponse,
    achievementToResponse,
    practitionerProfileToResponse,
    organizationProfileToResponse,
    SearchResults (..),
    SearchGroup (..),
    SearchHit (..),
    SearchTarget (..),
    LineageGraphData (..),
    LineageNode (..),
    LineageEdge (..),
    AchievementInformationResponse (..),
    achievementInformationToResponse,
    PromotionInformationResponse (..),
    promotionInformationToResponse,
    PractitionerDetailResponse (..),
    OrganizationDetailResponse (..),
    MonthlyPromotionsDataResponse (..),
    AchievementsMonthlyStatsResponse (..),
    PromotionsPageResponse (..),
    AchievementsPageResponse (..),
    ProfilesPageResponse (..),
    PractitionerExplorerRowResponse (..),
    PractitionerExplorerPageResponse (..),
    HomeExplorerHubPageResponse (..),
    DashboardPageResponse (..),
    TopOrganizationResponse (..),
    topOrganizationResponse,
    ActivityEventDetails (..),
    ActivityEventResponse (..),
    PendingActionsPromotionResponse (..),
    pendingPromotionToResponse,
    PendingActionsResponse (..),
    PromotionEligibilityResponse (..),
  )
where

import Data.Map.Strict (Map)
import Data.Swagger (ToSchema (..), genericDeclareNamedSchema)
import Data.Swagger.Internal.Schema ()
import Data.Text (Text)
import Deriving.Aeson
import DomainTypes.Core.BJJ (BJJBelt)
import DomainTypes.Core.Types qualified as Core
import DomainTypes.Rules.Promotion (PromotionViolation)
import DomainTypes.Transfer.OrderBy (ActivityEventType (..))
import DomainTypes.Transfer.Types qualified as T
import GeniusYield.Types (GYTime)
import Utils (mkStripPrefixSchemaOptions)

-- * Profile

data ProfileResponse = ProfileResponse
  { profileId :: Core.ProfileRefAC,
    profileName :: Text,
    profileDescription :: Text,
    profileImageURI :: Text,
    profileThumbnailURI :: Text,
    profileType :: Core.ProfileType
  }
  deriving stock (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "profile", CamelToSnake]] ProfileResponse

instance ToSchema ProfileResponse where
  declareNamedSchema = genericDeclareNamedSchema (mkStripPrefixSchemaOptions "profile")

profileToResponse :: Core.Profile -> ProfileResponse
profileToResponse p =
  ProfileResponse
    { profileId = Core.profileId p,
      profileName = Core.profileName p,
      profileDescription = Core.profileDescription p,
      profileImageURI = Core.profileImageURI p,
      profileThumbnailURI = deriveThumbnailUri (Core.profileImageURI p),
      profileType = Core.profileType p
    }

-- * Achievement

data AchievementResponse = AchievementResponse
  { achievementId :: Core.AchievementAC,
    achievementAwardedToProfileId :: Core.ProfileRefAC,
    achievementAwardedByProfileId :: Core.ProfileRefAC,
    achievementAchievementDate :: GYTime,
    achievementAccepted :: Bool,
    achievementName :: Text,
    achievementDescription :: Text,
    achievementImageURI :: Text,
    achievementThumbnailURI :: Text,
    achievementOtherMetadata :: [(Text, Text)]
  }
  deriving stock (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "achievement", CamelToSnake]] AchievementResponse

instance ToSchema AchievementResponse where
  declareNamedSchema = genericDeclareNamedSchema (mkStripPrefixSchemaOptions "achievement")

achievementToResponse :: Core.Achievement -> AchievementResponse
achievementToResponse a =
  AchievementResponse
    { achievementId = Core.achievementId a,
      achievementAwardedToProfileId = Core.achievementAwardedToProfileId a,
      achievementAwardedByProfileId = Core.achievementAwardedByProfileId a,
      achievementAchievementDate = Core.achievementAchievementDate a,
      achievementAccepted = Core.achievementAccepted a,
      achievementName = Core.achievementName a,
      achievementDescription = Core.achievementDescription a,
      achievementImageURI = Core.achievementImageURI a,
      achievementThumbnailURI = deriveThumbnailUri (Core.achievementImageURI a),
      achievementOtherMetadata = Core.achievementOtherMetadata a
    }

-- | Enriched achievement with resolved profile information for awarded-to and awarded-by.
-- Both profiles use 'ProfileResponse' (which carries 'profileType') because either
-- practitioners or organizations can award/receive achievements.
data AchievementInformationResponse = AchievementInformationResponse
  { achievementInfoId :: Core.AchievementAC,
    achievementInfoAchievementDate :: GYTime,
    achievementInfoAccepted :: Bool,
    achievementInfoName :: Text,
    achievementInfoDescription :: Text,
    achievementInfoImageURI :: Text,
    achievementInfoThumbnailURI :: Text,
    achievementInfoOtherMetadata :: [(Text, Text)],
    achievementInfoAwardedTo :: ProfileResponse,
    achievementInfoAwardedBy :: ProfileResponse
  }
  deriving stock (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "achievementInfo", CamelToSnake]] AchievementInformationResponse

instance ToSchema AchievementInformationResponse where
  declareNamedSchema = genericDeclareNamedSchema (mkStripPrefixSchemaOptions "achievementInfo")

achievementInformationToResponse ::
  Core.Profile ->
  Core.Profile ->
  Core.Achievement ->
  AchievementInformationResponse
achievementInformationToResponse awardedTo awardedBy a =
  AchievementInformationResponse
    { achievementInfoId = Core.achievementId a,
      achievementInfoAchievementDate = Core.achievementAchievementDate a,
      achievementInfoAccepted = Core.achievementAccepted a,
      achievementInfoName = Core.achievementName a,
      achievementInfoDescription = Core.achievementDescription a,
      achievementInfoImageURI = Core.achievementImageURI a,
      achievementInfoThumbnailURI = deriveThumbnailUri (Core.achievementImageURI a),
      achievementInfoOtherMetadata = Core.achievementOtherMetadata a,
      achievementInfoAwardedTo = profileToResponse awardedTo,
      achievementInfoAwardedBy = profileToResponse awardedBy
    }

-- * Practitioner / organization profile (detail)

data PractitionerProfileResponse = PractitionerProfileResponse
  { practitionerId :: Core.ProfileRefAC,
    practitionerName :: Text,
    practitionerDescription :: Text,
    practitionerImageURI :: Text,
    practitionerThumbnailURI :: Text,
    practitionerCurrentRank :: Core.Rank,
    practitionerPreviousRanks :: [Core.Rank]
  }
  deriving stock (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "practitioner", CamelToSnake]] PractitionerProfileResponse

instance ToSchema PractitionerProfileResponse where
  declareNamedSchema = genericDeclareNamedSchema (mkStripPrefixSchemaOptions "practitioner")

practitionerProfileToResponse :: T.PractitionerProfileInformation -> PractitionerProfileResponse
practitionerProfileToResponse p =
  PractitionerProfileResponse
    { practitionerId = T.practitionerId p,
      practitionerName = T.practitionerName p,
      practitionerDescription = T.practitionerDescription p,
      practitionerImageURI = T.practitionerImageURI p,
      practitionerThumbnailURI = deriveThumbnailUri (T.practitionerImageURI p),
      practitionerCurrentRank = T.practitionerCurrentRank p,
      practitionerPreviousRanks = T.practitionerPreviousRanks p
    }

data OrganizationProfileResponse = OrganizationProfileResponse
  { organizationId :: Core.ProfileRefAC,
    organizationName :: Text,
    organizationDescription :: Text,
    organizationImageURI :: Text,
    organizationThumbnailURI :: Text
  }
  deriving stock (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "organization", CamelToSnake]] OrganizationProfileResponse

instance ToSchema OrganizationProfileResponse where
  declareNamedSchema = genericDeclareNamedSchema (mkStripPrefixSchemaOptions "organization")

organizationProfileToResponse :: T.OrganizationProfileInformation -> OrganizationProfileResponse
organizationProfileToResponse o =
  OrganizationProfileResponse
    { organizationId = T.organizationId o,
      organizationName = T.organizationName o,
      organizationDescription = T.organizationDescription o,
      organizationImageURI = T.organizationImageURI o,
      organizationThumbnailURI = deriveThumbnailUri (T.organizationImageURI o)
    }

-- * Unified search (snake_case JSON per backend-api-requirements §9)

data SearchResults = SearchResults
  { searchResultsQuery :: Text,
    searchResultsTotal :: Int,
    searchResultsPractitioners :: SearchGroup,
    searchResultsOrganizations :: SearchGroup,
    searchResultsPromotions :: SearchGroup,
    searchResultsAchievements :: SearchGroup
  }
  deriving stock (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "searchResults", CamelToSnake]] SearchResults

instance ToSchema SearchResults where
  declareNamedSchema = genericDeclareNamedSchema (mkStripPrefixSchemaOptions "searchResults")

data SearchGroup = SearchGroup
  { searchGroupTotal :: Int,
    searchGroupItems :: [SearchHit]
  }
  deriving stock (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "searchGroup", CamelToSnake]] SearchGroup

instance ToSchema SearchGroup where
  declareNamedSchema = genericDeclareNamedSchema (mkStripPrefixSchemaOptions "searchGroup")

data SearchHit = SearchHit
  { searchHitId :: Text,
    searchHitTitle :: Text,
    searchHitSubtitle :: Text,
    searchHitBadges :: [Text],
    searchHitStatus :: Maybe Text,
    searchHitTarget :: SearchTarget
  }
  deriving stock (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "searchHit", CamelToSnake]] SearchHit

instance ToSchema SearchHit where
  declareNamedSchema = genericDeclareNamedSchema (mkStripPrefixSchemaOptions "searchHit")

data SearchTarget = SearchTarget
  { searchTargetKind :: Text,
    searchTargetId :: Text
  }
  deriving stock (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "searchTarget", CamelToSnake]] SearchTarget

instance ToSchema SearchTarget where
  declareNamedSchema = genericDeclareNamedSchema (mkStripPrefixSchemaOptions "searchTarget")

-- * Lineage graph (snake_case JSON per backend-api-requirements §10)

data LineageGraphData = LineageGraphData
  { lineageGraphDataNodes :: [LineageNode],
    lineageGraphDataEdges :: [LineageEdge]
  }
  deriving stock (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "lineageGraphData", CamelToSnake]] LineageGraphData

instance ToSchema LineageGraphData where
  declareNamedSchema = genericDeclareNamedSchema (mkStripPrefixSchemaOptions "lineageGraphData")

data LineageNode = LineageNode
  { lineageNodeProfileId :: Core.ProfileRefAC,
    lineageNodeName :: Text,
    lineageNodeImageUri :: Text,
    lineageNodeThumbnailUri :: Text,
    lineageNodeCurrentBelt :: BJJBelt,
    lineageNodeProfileType :: Core.ProfileType,
    lineageNodeIsMasterCapable :: Bool
  }
  deriving stock (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "lineageNode", CamelToSnake]] LineageNode

instance ToSchema LineageNode where
  declareNamedSchema = genericDeclareNamedSchema (mkStripPrefixSchemaOptions "lineageNode")

data LineageEdge = LineageEdge
  { lineageEdgeFrom :: Core.ProfileRefAC,
    lineageEdgeTo :: Core.ProfileRefAC,
    lineageEdgeAwardedBelt :: BJJBelt,
    lineageEdgeDate :: GYTime
  }
  deriving stock (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "lineageEdge", CamelToSnake]] LineageEdge

instance ToSchema LineageEdge where
  declareNamedSchema = genericDeclareNamedSchema (mkStripPrefixSchemaOptions "lineageEdge")

-- * Detail aggregates (backend-api-requirements §12.2–12.3)

-- | Enriched promotion for detail and page composites (nested practitioner profiles).
data PromotionInformationResponse = PromotionInformationResponse
  { promotionInfoId :: Core.RankAC,
    promotionInfoBelt :: BJJBelt,
    promotionInfoAchievementDate :: GYTime,
    promotionInfoAchievedBy :: PractitionerProfileResponse,
    promotionInfoAwardedBy :: PractitionerProfileResponse,
    promotionInfoState :: Core.PromotionState
  }
  deriving stock (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "promotionInfo", CamelToSnake]] PromotionInformationResponse

instance ToSchema PromotionInformationResponse where
  declareNamedSchema = genericDeclareNamedSchema (mkStripPrefixSchemaOptions "promotionInfo")

promotionInformationToResponse ::
  T.PractitionerProfileInformation ->
  T.PractitionerProfileInformation ->
  Core.Promotion ->
  PromotionInformationResponse
promotionInformationToResponse achievedBy awardedBy p =
  PromotionInformationResponse
    { promotionInfoId = Core.promotionId p,
      promotionInfoBelt = Core.promotionBelt p,
      promotionInfoAchievementDate = Core.promotionAchievementDate p,
      promotionInfoAchievedBy = practitionerProfileToResponse achievedBy,
      promotionInfoAwardedBy = practitionerProfileToResponse awardedBy,
      promotionInfoState = Core.promotionState p
    }

-- | Single-call practitioner profile page (§12.2).
data PractitionerDetailResponse = PractitionerDetailResponse
  { practitionerDetailPractitioner :: PractitionerProfileResponse,
    practitionerDetailAchievements :: [AchievementResponse],
    practitionerDetailMemberships :: [T.MembershipHistoryInformation],
    practitionerDetailPromotionsGiven :: [PromotionInformationResponse],
    practitionerDetailPromotionsReceived :: [PromotionInformationResponse],
    practitionerDetailOrgIdToName :: Map Core.ProfileRefAC Text,
    practitionerDetailOrgIdToOrg :: Map Core.ProfileRefAC OrganizationProfileResponse,
    practitionerDetailAwardedByProfiles :: Map Core.ProfileRefAC PractitionerProfileResponse,
    practitionerDetailAwardedByOrgs :: Map Core.ProfileRefAC OrganizationProfileResponse
  }
  deriving stock (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "practitionerDetail", CamelToSnake]] PractitionerDetailResponse

instance ToSchema PractitionerDetailResponse where
  declareNamedSchema = genericDeclareNamedSchema (mkStripPrefixSchemaOptions "practitionerDetail")

-- | Single-call organization profile page (§12.3).
data OrganizationDetailResponse = OrganizationDetailResponse
  { organizationDetailOrganization :: OrganizationProfileResponse,
    organizationDetailMemberships :: [T.MembershipHistoryInformation],
    organizationDetailAchievementsIssued :: [AchievementResponse],
    organizationDetailAchievementsReceived :: [AchievementResponse],
    organizationDetailMemberProfiles :: Map Core.ProfileRefAC PractitionerProfileResponse
  }
  deriving stock (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "organizationDetail", CamelToSnake]] OrganizationDetailResponse

instance ToSchema OrganizationDetailResponse where
  declareNamedSchema = genericDeclareNamedSchema (mkStripPrefixSchemaOptions "organizationDetail")

-- * Explorer page aggregates (backend-api-requirements §12.5)

-- | One month of promotion counts by belt (dashboard / promotions page).
data MonthlyPromotionsDataResponse = MonthlyPromotionsDataResponse
  { monthlyPromotionsDataMonth :: Text,
    monthlyPromotionsDataBeltFrequency :: [(BJJBelt, Int)]
  }
  deriving stock (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "monthlyPromotionsData", CamelToSnake]] MonthlyPromotionsDataResponse

instance ToSchema MonthlyPromotionsDataResponse where
  declareNamedSchema = genericDeclareNamedSchema (mkStripPrefixSchemaOptions "monthlyPromotionsData")

-- | Accepted vs pending achievement counts per calendar month.
data AchievementsMonthlyStatsResponse = AchievementsMonthlyStatsResponse
  { achievementsMonthlyStatsMonth :: Text,
    achievementsMonthlyStatsAccepted :: Int,
    achievementsMonthlyStatsPending :: Int
  }
  deriving stock (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "achievementsMonthlyStats", CamelToSnake]] AchievementsMonthlyStatsResponse

instance ToSchema AchievementsMonthlyStatsResponse where
  declareNamedSchema = genericDeclareNamedSchema (mkStripPrefixSchemaOptions "achievementsMonthlyStats")

data PromotionsPageResponse = PromotionsPageResponse
  { promotionsPageItems :: [PromotionInformationResponse],
    promotionsPageTotal :: Int,
    promotionsPageFrequency :: [(BJJBelt, Int)],
    promotionsPageMonthly :: [MonthlyPromotionsDataResponse]
  }
  deriving stock (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "promotionsPage", CamelToSnake]] PromotionsPageResponse

instance ToSchema PromotionsPageResponse where
  declareNamedSchema = genericDeclareNamedSchema (mkStripPrefixSchemaOptions "promotionsPage")

data AchievementsPageResponse = AchievementsPageResponse
  { achievementsPageItems :: [AchievementResponse],
    achievementsPageTotal :: Int,
    achievementsPageMonthly :: [AchievementsMonthlyStatsResponse]
  }
  deriving stock (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "achievementsPage", CamelToSnake]] AchievementsPageResponse

instance ToSchema AchievementsPageResponse where
  declareNamedSchema = genericDeclareNamedSchema (mkStripPrefixSchemaOptions "achievementsPage")

data ProfilesPageResponse = ProfilesPageResponse
  { profilesPageItems :: [ProfileResponse],
    profilesPageTotal :: Int,
    profilesPageFrequency :: [(Core.ProfileType, Int)]
  }
  deriving stock (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "profilesPage", CamelToSnake]] ProfilesPageResponse

instance ToSchema ProfilesPageResponse where
  declareNamedSchema = genericDeclareNamedSchema (mkStripPrefixSchemaOptions "profilesPage")

data PractitionerExplorerRowResponse = PractitionerExplorerRowResponse
  { practitionerExplorerRowPractitioner :: PractitionerProfileResponse,
    practitionerExplorerRowMemberships :: [T.MembershipHistoryInformation]
  }
  deriving stock (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "practitionerExplorerRow", CamelToSnake]] PractitionerExplorerRowResponse

instance ToSchema PractitionerExplorerRowResponse where
  declareNamedSchema = genericDeclareNamedSchema (mkStripPrefixSchemaOptions "practitionerExplorerRow")

data PractitionerExplorerPageResponse = PractitionerExplorerPageResponse
  { practitionerExplorerPageItems :: [PractitionerExplorerRowResponse],
    practitionerExplorerPageTotal :: Int,
    practitionerExplorerPageOrgIdToName :: Map Core.ProfileRefAC Text
  }
  deriving stock (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "practitionerExplorerPage", CamelToSnake]] PractitionerExplorerPageResponse

instance ToSchema PractitionerExplorerPageResponse where
  declareNamedSchema = genericDeclareNamedSchema (mkStripPrefixSchemaOptions "practitionerExplorerPage")

data HomeExplorerHubPageResponse = HomeExplorerHubPageResponse
  { homeExplorerHubPageRecentPromotions :: [PromotionInformationResponse],
    homeExplorerHubPageLatestPractitioners :: [PractitionerProfileResponse]
  }
  deriving stock (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "homeExplorerHubPage", CamelToSnake]] HomeExplorerHubPageResponse

instance ToSchema HomeExplorerHubPageResponse where
  declareNamedSchema = genericDeclareNamedSchema (mkStripPrefixSchemaOptions "homeExplorerHubPage")

-- * §12.1 Dashboard

-- | Top organization by active member count.
data TopOrganizationResponse = TopOrganizationResponse
  { topOrganizationOrganization :: OrganizationProfileResponse,
    topOrganizationMemberCount :: Int
  }
  deriving stock (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "topOrganization", CamelToSnake]] TopOrganizationResponse

instance ToSchema TopOrganizationResponse where
  declareNamedSchema = genericDeclareNamedSchema (mkStripPrefixSchemaOptions "topOrganization")

-- | Build a 'TopOrganizationResponse' from an org profile response and member count.
topOrganizationResponse :: OrganizationProfileResponse -> Int -> TopOrganizationResponse
topOrganizationResponse org count =
  TopOrganizationResponse
    { topOrganizationOrganization = org,
      topOrganizationMemberCount = count
    }

-- | §12.1 dashboard aggregate.
data DashboardPageResponse = DashboardPageResponse
  { dashboardPageTotalPractitioners :: Int,
    dashboardPageTotalOrganizations :: Int,
    dashboardPageTotalPromotions :: Int,
    dashboardPageTotalAchievements :: Int,
    dashboardPageBeltFrequency :: [(BJJBelt, Int)],
    dashboardPageRecentPromotions :: [PromotionInformationResponse],
    dashboardPageMonthlyPromotions :: [MonthlyPromotionsDataResponse],
    dashboardPageMonthlyAchievements :: [AchievementsMonthlyStatsResponse],
    dashboardPageLatestOrganizations :: [OrganizationProfileResponse],
    dashboardPageLatestPractitioners :: [PractitionerProfileResponse],
    dashboardPageTopOrganizations :: [TopOrganizationResponse]
  }
  deriving stock (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "dashboardPage", CamelToSnake]] DashboardPageResponse

instance ToSchema DashboardPageResponse where
  declareNamedSchema = genericDeclareNamedSchema (mkStripPrefixSchemaOptions "dashboardPage")

-- * Pending actions (My Dojo inbox)

-- | Promotion summary for the pending-actions inbox.
data PendingActionsPromotionResponse = PendingActionsPromotionResponse
  { pendingPromotionId :: Core.RankAC,
    pendingPromotionBelt :: BJJBelt,
    pendingPromotionAchievedByProfileId :: Core.ProfileRefAC,
    pendingPromotionAwardedByProfileId :: Core.ProfileRefAC,
    pendingPromotionAchievementDate :: GYTime,
    pendingPromotionState :: Core.PromotionState
  }
  deriving stock (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "pendingPromotion", CamelToSnake]] PendingActionsPromotionResponse

instance ToSchema PendingActionsPromotionResponse where
  declareNamedSchema = genericDeclareNamedSchema (mkStripPrefixSchemaOptions "pendingPromotion")

pendingPromotionToResponse :: Core.Promotion -> PendingActionsPromotionResponse
pendingPromotionToResponse p =
  PendingActionsPromotionResponse
    { pendingPromotionId = Core.promotionId p,
      pendingPromotionBelt = Core.promotionBelt p,
      pendingPromotionAchievedByProfileId = Core.promotionAchievedByProfileId p,
      pendingPromotionAwardedByProfileId = Core.promotionAwardedByProfileId p,
      pendingPromotionAchievementDate = Core.promotionAchievementDate p,
      pendingPromotionState = Core.promotionState p
    }

-- | Composite pending-actions response for a profile's inbox.
data PendingActionsResponse = PendingActionsResponse
  { pendingActionsPromotions :: [PendingActionsPromotionResponse],
    pendingActionsAchievements :: [AchievementResponse],
    pendingActionsMemberships :: [T.MembershipIntervalInformation],
    pendingActionsAwardedByName :: Map Core.ProfileRefAC Text,
    pendingActionsOrganizationName :: Map Core.ProfileRefAC Text
  }
  deriving stock (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "pendingActions", CamelToSnake]] PendingActionsResponse

instance ToSchema PendingActionsResponse where
  declareNamedSchema = genericDeclareNamedSchema (mkStripPrefixSchemaOptions "pendingActions")

-- * Activity feed (§ activity endpoint)

-- | Human-readable context for an activity event.
data ActivityEventDetails = ActivityEventDetails
  { activityEventDetailsTitle :: Text,
    activityEventDetailsBody :: Maybe Text,
    activityEventDetailsActorName :: Maybe Text,
    activityEventDetailsTargetName :: Maybe Text
  }
  deriving stock (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "activityEventDetails", CamelToSnake]] ActivityEventDetails

instance ToSchema ActivityEventDetails where
  declareNamedSchema = genericDeclareNamedSchema (mkStripPrefixSchemaOptions "activityEventDetails")

-- | Single activity feed item.
data ActivityEventResponse = ActivityEventResponse
  { activityEventEventType :: ActivityEventType,
    activityEventActorId :: Core.ProfileRefAC,
    activityEventTargetId :: Maybe Core.ProfileRefAC,
    activityEventTimestamp :: GYTime,
    activityEventDetails :: ActivityEventDetails
  }
  deriving stock (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "activityEvent", CamelToSnake]] ActivityEventResponse

instance ToSchema ActivityEventResponse where
  declareNamedSchema = genericDeclareNamedSchema (mkStripPrefixSchemaOptions "activityEvent")

-- * Promotion eligibility (rule check)

-- | Structured verdict for the @GET /practitioner/{id}/eligibility@ endpoint.
--
-- 'eligibilityEligible' is 'True' iff 'eligibilityViolations' is empty. When
-- the caller omits the @granter@ query parameter the master-authority check
-- is skipped: 'eligibilityGranterRank' is 'Nothing', 'eligibilityViolations'
-- contains only rung / time-in-grade / student-date rules, and
-- 'eligibilityRequiredGranterRank' communicates the minimum belt any would-be
-- granter must hold ('Nothing' when no granter can reach the target — i.e.
-- the practitioner is already at or beyond 'Onchain.BJJ.Red').
data PromotionEligibilityResponse = PromotionEligibilityResponse
  { eligibilityEligible :: Bool,
    eligibilityCurrentBelt :: BJJBelt,
    eligibilityTargetBelt :: BJJBelt,
    eligibilityDaysInCurrentGrade :: Integer,
    eligibilityRequiredMonthsInGrade :: Integer,
    eligibilityEarliestEligibleDate :: GYTime,
    eligibilityRequiredGranterRank :: Maybe BJJBelt,
    eligibilityGranterRank :: Maybe BJJBelt,
    eligibilityViolations :: [PromotionViolation]
  }
  deriving stock (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "eligibility", CamelToSnake]] PromotionEligibilityResponse

instance ToSchema PromotionEligibilityResponse where
  declareNamedSchema = genericDeclareNamedSchema (mkStripPrefixSchemaOptions "eligibility")

-- | Derive the thumbnail URL shown in API responses from an image URI.
-- Today mirrors @image_uri@; may later resolve cached or CDN URLs.
deriveThumbnailUri :: Text -> Text
deriveThumbnailUri = id
