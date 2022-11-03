#' @export
runCohortDiagnostics <- function(connectionDetails = NULL,
                                 connection = NULL,
                                 cdmDatabaseSchema,
                                 cohortDatabaseSchema = cdmDatabaseSchema,
                                 cohortStagingTable = "cohort_stg",
                                 oracleTempSchema = cohortDatabaseSchema,
                                 cohortGroupNames = getCohortGroupNamesForDiagnostics(),
                                 cohortIdsToExcludeFromExecution = c(),
                                 exportFolder,
                                 databaseId = "Unknown",
                                 databaseName = "Unknown",
                                 databaseDescription = "Unknown",
                                 incrementalFolder = file.path(exportFolder, "RecordKeeping"),
                                 minCellCount = 5) {
  # Verify that the cohortGroups are the ones that are specified in the 
  # CohortGroupsDiagnostics.csv
  cohortGroups <- getCohortGroupsForDiagnostics()
  cohortGroupsExist <- cohortGroupNames %in% cohortGroups$cohortGroup
  if (!all(cohortGroupsExist)) {
    ParallelLogger::logError(paste("Invalid cohort group name. Must be one of:", paste(getCohortGroupNamesForDiagnostics(), collapse = ', ')))
    stop()
  }
  cohortGroups <- cohortGroups[cohortGroups$cohortGroup %in% cohortGroupNames, ]
  ParallelLogger::logDebug(paste("CohortGroups: ", cohortGroups))
  
  # NOTE: The exportFolder is the root folder where the
  # study results will live. The diagnostics will be written
  # to a subfolder called "diagnostics". Both the diagnostics
  # and main study code (RunStudy.R) will share the same
  # RecordKeeping folder so that we can ensure that cohorts
  # are only created one time.
  diagnosticOutputFolder <- file.path(exportFolder, "diagnostics")
  cohortGroups$outputFolder <- file.path(diagnosticOutputFolder, cohortGroups$cohortGroup)
  if (!file.exists(diagnosticOutputFolder)) {
    dir.create(diagnosticOutputFolder, recursive = TRUE)
  }
  
  if (!is.null(getOption("fftempdir")) && !file.exists(getOption("fftempdir"))) {
    warning("fftempdir '", getOption("fftempdir"), "' not found. Attempting to create folder")
    dir.create(getOption("fftempdir"), recursive = TRUE)
  }
  ParallelLogger::addDefaultFileLogger(file.path(diagnosticOutputFolder, "cohortDiagnosticsLog.txt"))
  on.exit(ParallelLogger::unregisterLogger("DEFAULT"))
  
  # Write out the system information
  ParallelLogger::logInfo(.systemInfo())
  
  if (is.null(connection)) {
    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))
  }
  
  # Create cohorts -----------------------------
  cohorts <- getCohortsToCreate(cohortGroups = cohortGroups)
  # cohorts <- cohorts[1:10,]
  cohorts <- cohorts[!(cohorts$cohortId %in% cohortIdsToExcludeFromExecution) & cohorts$atlasId > 0, ] # cohorts$atlasId > 0 is used to avoid those cohorts that use custom SQL identified with an atlasId == -1
  ParallelLogger::logInfo("Creating cohorts in incremental mode")
  instantiateCohortSet(connectionDetails = connectionDetails,
                       connection = connection,
                       cdmDatabaseSchema = cdmDatabaseSchema,
                       oracleTempSchema = oracleTempSchema,
                       cohortDatabaseSchema = cohortDatabaseSchema,
                       cohortTable = cohortStagingTable,
                       cohortIds = cohorts$cohortId,
                       minCellCount = minCellCount,
                       createCohortTable = TRUE,
                       generateInclusionStats = FALSE,
                       incremental = TRUE,
                       incrementalFolder = incrementalFolder,
                       inclusionStatisticsFolder = diagnosticOutputFolder)
  
  ParallelLogger::logInfo("Running cohort diagnostics")
  for (i in 1:nrow(cohortGroups)) {
    tryCatch(expr = {
      target_file = read.csv(system.file(cohortGroups$fileName[i], package = getThisPackageName(), mustWork = TRUE))
      target_file_ids = target_file$cohortId
      cohorts_set = data.frame(cohortId = NULL, cohortName = NULL, sql = NULL, json = NULL)
      for(id in target_file_ids){
        cohort_name <- target_file[i, 'name']
        sql_file <- system.file('sql', 'sql_server', paste(id, 'sql', sep = '.'), package = getThisPackageName(), mustWork = TRUE)
        json_file <- system.file('cohorts', paste(id, 'json', sep = '.'), package = getThisPackageName(), mustWork = TRUE)
        cohorts_set <- rbind(cohorts_set, data.frame(cohortId = id, 
                                                     cohortName = cohort_name,
                                                     sql = readChar(sql_file, file.info(sql_file)$size),
                                                     json = readChar(json_file, file.info(json_file)$size),
                                                     stringsAsFactors = FALSE))
      }
      
      
      
      CohortDiagnostics::executeDiagnostics(cohortDefinitionSet = cohorts_set,
                                            exportFolder = cohortGroups$outputFolder[i],
                                            databaseId = databaseId,
                                            databaseName = databaseName,
                                            databaseDescription = databaseDescription,
                                            connection = connection,
                                            connectionDetails = connectionDetails,
                                            cdmDatabaseSchema = cdmDatabaseSchema,
                                            tempEmulationSchema = oracleTempSchema,
                                            cohortDatabaseSchema = cohortDatabaseSchema,
                                            cohortTable = cohortStagingTable,
                                            cohortIds = cohorts$cohortId,
                                            runInclusionStatistics = FALSE,
                                            runIncludedSourceConcepts = TRUE,
                                            runOrphanConcepts = TRUE,
                                            runBreakdownIndexEvents = TRUE,
                                            runIncidenceRate = TRUE,
                                            runTemporalCohortCharacterization = FALSE,
                                            minCellCount = minCellCount,
                                            incremental = TRUE,
                                            incrementalFolder = incrementalFolder)
    },error = function(e){
      ParallelLogger::logError(paste0("Error when running CohortDiagnostics::runCohortDiagnostics on CohortGroup: ", cohortGroups$cohortGroup[i]))
      ParallelLogger::logError(e)
    })
  }
  
  # Bundle the diagnostics for export
  bundledResultsLocation <- bundleDiagnosticsResults(diagnosticOutputFolder, databaseId)
  ParallelLogger::logInfo(paste("PIONEER cohort diagnostics are bundled for sharing at: ", bundledResultsLocation))
}

#' @export
bundleDiagnosticsResults <- function(diagnosticOutputFolder, databaseId) {
  # Prepare additional metadata files
  # codemetar::write_codemeta(pkg = find.package(getThisPackageName()), 
  #                           path = file.path(diagnosticOutputFolder, "codemeta.json"))
  
  # Write metadata, log, and diagnostics results files to single ZIP file
  date <- format(Sys.time(), "%Y%m%dT%H%M%S")
  zipName <- file.path(diagnosticOutputFolder, paste0("Results_diagnostics_", databaseId, "_", date, ".zip")) 
  files <- list.files(diagnosticOutputFolder, "^Results_.*.zip$|codemeta.json|cohortDiagnosticsLog.txt", full.names = TRUE, recursive = TRUE)
  oldWd <- setwd(diagnosticOutputFolder)
  on.exit(setwd(oldWd), add = TRUE)
  DatabaseConnector::createZipFile(zipFile = zipName, files = files)
  return(zipName)
}