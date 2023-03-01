getBulkStrata <- function() {
  resourceFile <- file.path(getPathToResource(), "BulkStrata.csv")
  return(readCsv(resourceFile))
}

getCohortGroupNamesForDiagnostics <- function() {
  return(getCohortGroupsForDiagnostics()$cohortGroup)
}

getCohortGroupsForDiagnostics <- function() {
  resourceFile <- file.path(getPathToResource(), "CohortGroupsDiagnostics.csv")
  return(readCsv(resourceFile))
}

getCohortGroups <- function() {
  resourceFile <- file.path(getPathToResource(), "CohortGroups.csv")
  return(readCsv(resourceFile))
}

getCohortBasedStrata <- function() {
  resourceFile <- file.path(getPathToResource(), "CohortsToCreateStrata.csv")
  return(readCsv(resourceFile))
}


getTimeToEventSettings <- function(){
  resourceFile <- file.path(getPathToResource(), "TimeToEvent.csv")
  return(readCsv(resourceFile))
}

# getTargetStrataXref <- function() {
#   resourceFile <- file.path(getPathToResource(), "targetStrataXref.csv")
#   return(readCsv(resourceFile))
# }

getCohortsToCreate <- function(cohortGroups = getCohortGroups()) {
  packageName <- getThisPackageName()
  cohorts <- data.frame()
  for(i in 1:nrow(cohortGroups)) {
    c <- data.table::fread(system.file(cohortGroups$fileName[i], package = packageName, mustWork = TRUE))
    c <- c[,c('name', 'atlasName', 'atlasId', 'cohortId')]
    c$cohortType <- cohortGroups$cohortGroup[i]
    cohorts <- rbind(cohorts, c)
  }
  return(cohorts)  
}

getAllStrata <- function() {
  colNames <- c("name", "cohortId", "generationScript") # Use this to subset to the columns of interest
  bulkStrata <- getBulkStrata()
  bulkStrata <- bulkStrata[, ..colNames]
  atlasCohortStrata <- getCohortBasedStrata()
  atlasCohortStrata$generationScript <- paste0(atlasCohortStrata$cohortId, ".sql")
  atlasCohortStrata <- atlasCohortStrata[, ..colNames]
  strata <- rbind(bulkStrata, atlasCohortStrata)
  return(strata)  
}

getAllStudyCohorts <- function() {
  cohortsToCreate <- getCohortsToCreate()
  targetStrataXref <- getTargetStrataXref()
  colNames <- c("name", "cohortId")
  cohortsToCreate <- cohortsToCreate[, ..colNames]
  targetStrataXref <- targetStrataXref[, ..colNames]
  allCohorts <- rbind(cohortsToCreate, targetStrataXref)
  return(allCohorts)
}
