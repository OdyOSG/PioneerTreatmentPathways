# Copyright 2022 Observational Health Data Sciences and Informatics
#
# This file is part of PioneerMetastaticTreatment
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


cohortGroups <- readr::read_csv("inst/settings/CohortGroups.csv", col_types = readr::cols())

# Create the corresponding diagnostic file 
for (i in 1:nrow(cohortGroups)) {
  ParallelLogger::logInfo("* Creating diagnostics settings file for: ", cohortGroups$cohortGroup[i], " *")
  cohortsToCreate <- readr::read_csv(file.path("inst/", cohortGroups$fileName[i]), col_types = readr::cols())
  cohortsToCreate$name <- cohortsToCreate$cohortId
  readr::write_csv(cohortsToCreate, file.path("inst/settings/diagnostics/", basename(cohortGroups$fileName[i])))
}


settingsPath <- file.path('inst', 'settings')

# Create the list of combinations of T, TwS, TwoS for the combinations of strata ----------------------------

# The Atlas Name is used as the name. The 'name' column (containing same id as cohortId) is ignored.
targetCohorts <- read.csv(file.path(settingsPath, "CohortsToCreateTarget.csv"), col.names = c('unused','name','atlasId','cohortId'))
bulkStrata <- read.csv(file.path(settingsPath, "BulkStrata.csv"))
atlasCohortStrata <- read.csv(file.path(settingsPath, "CohortsToCreateStrata.csv"), col.names = c('unused','name','atlasId','cohortId'))
outcomeCohorts <- read.csv(file.path(settingsPath, "CohortsToCreateOutcome.csv"), col.names = c('unused','name','atlasId','cohortId'))

# Target cohorts
colNames <- c("name", "cohortId") # Use this to subset to the columns of interest
targetCohorts <- targetCohorts[, match(colNames, names(targetCohorts))]
names(targetCohorts) <- c("targetName", "targetId")
# Strata cohorts
bulkStrata <- bulkStrata[, match(colNames, names(bulkStrata))]
bulkStrata$withStrataName <- paste("with", trimws(bulkStrata$name))
bulkStrata$inverseName <- paste("without", trimws(bulkStrata$name))
atlasCohortStrata <- atlasCohortStrata[, match(colNames, names(atlasCohortStrata))]
atlasCohortStrata$withStrataName <- paste("with", trimws(atlasCohortStrata$name))
atlasCohortStrata$inverseName <- paste("without", trimws(atlasCohortStrata$name))
strata <- rbind(bulkStrata, atlasCohortStrata)
names(strata) <- c("name", "strataId", "strataName", "strataInverseName")
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
readr::write_csv(shinyCohortXref, file.path("inst/shiny/PIONEERmetastaticTreatmentExplorer", "cohortXref.csv"))

# Write out the final targetStrataXRef
targetStrataXRef <- targetStrataXRef[,c("targetId","strataId","cohortId","cohortType","name")]
readr::write_csv(targetStrataXRef, file.path(settingsPath, "targetStrataXref.csv"))


