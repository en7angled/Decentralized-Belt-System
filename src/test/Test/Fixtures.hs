{-# LANGUAGE OverloadedStrings #-}

module Test.Fixtures
  ( adminTestProfileData,
    studentProfileData,
    masterProfileData,
    masterBProfileData,
    orgProfileData,
    practitionerProfileData,
    practitionerBProfileData,
    practitionerCProfileData,
    achievementProfileData,
    seminarProfileData,
    cleanupTestProfileData,
  )
where

import DomainTypes.Core.Actions (ProfileData (..))

adminTestProfileData :: ProfileData
adminTestProfileData =
  ProfileData
    { profileDataName = "Admin Test User",
      profileDataDescription = "A test profile for admin action tests",
      profileDataImageURI = "ipfs://QmAdminTest"
    }

studentProfileData :: ProfileData
studentProfileData =
  ProfileData
    { profileDataName = "John Doe",
      profileDataDescription = "John Doe is a student",
      profileDataImageURI = "ipfs://QmReBRNMe7tBr6WbA89uwnHHW7f7Zoe8wY2mzVpA8STdAk"
    }

masterProfileData :: ProfileData
masterProfileData =
  ProfileData
    { profileDataName = "Master",
      profileDataDescription = "Master is a master",
      profileDataImageURI = "ipfs://Qmb3JXJHQxuReSUaH6rXAoP5oX9NRs6JmnRFGTj2RVhGwe"
    }

masterBProfileData :: ProfileData
masterBProfileData =
  ProfileData
    { profileDataName = "Master B",
      profileDataDescription = "Master B is also a master",
      profileDataImageURI = "ipfs://QmMasterB"
    }

orgProfileData :: ProfileData
orgProfileData =
  ProfileData
    { profileDataName = "BJJ Academy",
      profileDataDescription = "A BJJ Academy Organization",
      profileDataImageURI = "ipfs://QmOrgAcademy"
    }

practitionerProfileData :: ProfileData
practitionerProfileData =
  ProfileData
    { profileDataName = "Practitioner A",
      profileDataDescription = "A practitioner member",
      profileDataImageURI = "ipfs://QmPractitionerA"
    }

practitionerBProfileData :: ProfileData
practitionerBProfileData =
  ProfileData
    { profileDataName = "Practitioner B",
      profileDataDescription = "Second practitioner member",
      profileDataImageURI = "ipfs://QmPractitionerB"
    }

practitionerCProfileData :: ProfileData
practitionerCProfileData =
  ProfileData
    { profileDataName = "Practitioner C",
      profileDataDescription = "Third practitioner member",
      profileDataImageURI = "ipfs://QmPractitionerC"
    }

achievementProfileData :: ProfileData
achievementProfileData =
  ProfileData
    { profileDataName = "Gold Medal",
      profileDataDescription = "Won gold medal at tournament",
      profileDataImageURI = "ipfs://QmGoldMedal"
    }

seminarProfileData :: ProfileData
seminarProfileData =
  ProfileData
    { profileDataName = "Advanced Seminar",
      profileDataDescription = "Attended advanced seminar",
      profileDataImageURI = "ipfs://QmSeminar"
    }

cleanupTestProfileData :: ProfileData
cleanupTestProfileData =
  ProfileData
    { profileDataName = "Cleanup Test User",
      profileDataDescription = "A test profile to verify dust cleanup safety",
      profileDataImageURI = "ipfs://QmCleanupTest"
    }
