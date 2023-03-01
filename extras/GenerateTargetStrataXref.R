# Create strata cross reference settings files


# cohortGroups <- readr::read_csv("inst/settings/CohortGroups.csv", show_col_types = FALSE)
# 
# # Create the corresponding diagnostic file 
# for (i in 1:nrow(cohortGroups)) {
#   ParallelLogger::logInfo("* Creating diagnostics settings file for: ", cohortGroups$cohortGroup[i], " *")
#   cohortsToCreate <- readr::read_csv(file.path("inst/", cohortGroups$fileName[i]), col_types = readr::cols())
#   cohortsToCreate$name <- cohortsToCreate$cohortId
#   readr::write_csv(cohortsToCreate, file.path("inst/settings/diagnostics/", basename(cohortGroups$fileName[i])))
# }

library(dplyr)

# Create the list of combinations of T, TwS, TwoS for the combinations of strata ----------------------------
# T = Target, TwS = Target with strata, TwoS = Target without strata

settingsPath <- file.path('inst', 'settings_original')

# The Atlas Name is used as the name. The 'name' column (containing same id as cohortId) is ignored.
targetCohorts <- read.csv(file.path(settingsPath, "CohortsToCreateTarget.csv"), col.names = c('unused','name','atlasId','cohortId'))

cohorts <- readr::read_csv("inst/settings/CohortsToCreate.csv", show_col_types = F) %>% 
  select(name = atlasName, atlasId, cohortId, group)

targetCohorts <- filter(cohorts, group == "Target")
atlasCohortStrata <- filter(cohorts, group == "Stratification")
outcomeCohorts <- filter(cohorts, group == "Outcome")

bulkStrata <- readr::read_csv("inst/settings/BulkStrata.csv", show_col_types = F)
# atlasCohortStrata <- read.csv(file.path(settingsPath, "CohortsToCreateStrata.csv"), col.names = c('unused','name','atlasId','cohortId'))
# outcomeCohorts <- read.csv(file.path(settingsPath, "CohortsToCreateOutcome.csv"), col.names = c('unused','name','atlasId','cohortId'))

# Target cohorts
# colNames <- c("name", "cohortId") # Use this to subset to the columns of interest
targetCohorts <- select(targetCohorts, targetName = name, targetId = cohortId)
# names(targetCohorts) <- c("targetName", "targetId")

# Strata cohorts
strata <- bind_rows(
  bulkStrata %>% 
  select(name, strataId = cohortId) %>% 
  mutate(strataName = paste("with", trimws(name)),
         strataInverseName = paste("without", trimws(name)))
  ,
  atlasCohortStrata %>% 
  select(name, strataId = cohortId) %>% 
  mutate(strataName = paste("with", trimws(name)),
         strataInverseName = paste("without", trimws(name)))
) 
  
# Get all of the unique combinations of target + strata
targetStrataCP <- do.call(expand.grid, lapply(list(targetCohorts$targetId, strata$strataId), unique))
names(targetStrataCP) <- c("targetId", "strataId")
targetStrataCP <- merge(targetStrataCP, targetCohorts)
targetStrataCP <- merge(targetStrataCP, strata)
targetStrataCP <- targetStrataCP[order(targetStrataCP$strataId, targetStrataCP$targetId),]
targetStrataCP$cohortId <- (targetStrataCP$targetId * 1000000) + (targetStrataCP$strataId*10)
tWithS <- targetStrataCP
tWithoutS <- targetStrataCP[targetStrataCP$strataId %in% atlasCohortStrata$cohortId, ]
tWithS$cohortId <- tWithS$cohortId + 1
tWithS$cohortType <- "TwS"
tWithS$name <- paste(tWithS$targetName, tWithS$strataName)
tWithoutS$cohortId <- tWithoutS$cohortId + 2
tWithoutS$cohortType <- "TwoS"
tWithoutS$name <- paste(tWithoutS$targetName, tWithoutS$strataInverseName)
targetStrataXRef <- rbind(tWithS, tWithoutS)

# For shiny, construct a data frame to provide details on the original cohort names
xrefColumnNames <- c("cohortId", "targetId", "targetName", "strataId", "strataName", "cohortType")
targetCohortsForShiny <- targetCohorts
targetCohortsForShiny$cohortId <- targetCohortsForShiny$targetId
targetCohortsForShiny$strataId <- 0
targetCohortsForShiny$strataName <- "All"
targetCohortsForShiny$cohortType <- "Target"
inverseStrata <- targetStrataXRef[targetStrataXRef$cohortType == "TwoS",]
inverseStrata$strataName <- inverseStrata$strataInverseName

shinyCohortXref <- rbind(targetCohortsForShiny[,xrefColumnNames], 
                         inverseStrata[,xrefColumnNames],
                         targetStrataXRef[targetStrataXRef$cohortType == "TwS",xrefColumnNames])
readr::write_csv(shinyCohortXref, file.path("inst/shiny/PioneerTreatmentPathwaysExplorer", "cohortXref.csv"))

# Write out the final targetStrataXRef
targetStrataXRef <- targetStrataXRef[,c("targetId","strataId","cohortId","cohortType","name")]
readr::write_csv(targetStrataXRef, file.path("inst/settings/targetStrataXref.csv"))


