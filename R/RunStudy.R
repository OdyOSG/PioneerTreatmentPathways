
#' @export
#' @import data.table
runStudy <- function(connectionDetails = NULL,
                     connection = NULL,
                     cdmDatabaseSchema,
                     oracleTempSchema = NULL,
                     cohortDatabaseSchema,
                     cohortStagingTable = "cohort_stg",
                     cohortTable = "cohort",
                     featureSummaryTable = "cohort_smry",
                     cohortIdsToExcludeFromExecution = c(),
                     cohortIdsToExcludeFromResultsExport = NULL,
                     cohortGroups = getUserSelectableCohortGroups(),
                     exportFolder,
                     databaseId,
                     databaseName = databaseId,
                     databaseDescription = "",
                     useBulkCharacterization = FALSE,
                     minCellCount = 5,
                     incremental = FALSE,
                     incrementalFolder = file.path(exportFolder, "RecordKeeping"),
                     minimumSubjectCountForCharacterization = 140) {
  
  start <- Sys.time()
  
  if (!file.exists(exportFolder)) {
    dir.create(exportFolder, recursive = TRUE)
  }
  
  ParallelLogger::addDefaultFileLogger(file.path(exportFolder, "PioneerTreatmentPathways.txt"))
  on.exit(ParallelLogger::unregisterLogger("DEFAULT"))
  
  # Write out the system information
  ParallelLogger::logInfo(.systemInfo())
  
  useSubset = Sys.getenv("USE_SUBSET")
  if (!is.na(as.logical(useSubset)) && as.logical(useSubset)) {
    ParallelLogger::logWarn("Running in subset mode for testing")
  }
  
  if (incremental) {
    if (is.null(incrementalFolder)) {
      stop("Must specify incrementalFolder when incremental = TRUE")
    }
    if (!file.exists(incrementalFolder)) {
      dir.create(incrementalFolder, recursive = TRUE)
    }
  }
  
  if (!is.null(getOption("fftempdir")) && !file.exists(getOption("fftempdir"))) {
    warning("fftempdir '", getOption("fftempdir"), "' not found. Attempting to create folder")
    dir.create(getOption("fftempdir"), recursive = TRUE)
  }
  
  if (is.null(connection)) {
    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))
  }
  
  andrData <- Andromeda::andromeda()

  # Instantiate cohorts -----------------------------------------------------------------------
  cohorts <- getCohortsToCreate()
  # Remove any cohorts that are to be excluded
  cohorts <- cohorts[!(cohorts$cohortId %in% cohortIdsToExcludeFromExecution), ]
  targetCohortIds  <- cohorts[cohorts$cohortType == "target",  "cohortId"][[1]]
  strataCohortIds  <- cohorts[cohorts$cohortType == "strata",  "cohortId"][[1]]
  outcomeCohortIds <- cohorts[cohorts$cohortType == "outcome", "cohortId"][[1]]
  # treatmentCohortIds <- cohorts[cohorts$cohortType == "treatment", "cohortId"][[1]]
  
  # Start with the target cohorts
  ParallelLogger::logInfo("**********************************************************")
  ParallelLogger::logInfo("  ---- Creating target cohorts ---- ")
  ParallelLogger::logInfo("**********************************************************")
  instantiateCohortSet(connectionDetails = connectionDetails,
                       connection = connection,
                       cdmDatabaseSchema = cdmDatabaseSchema,
                       oracleTempSchema = oracleTempSchema,
                       cohortDatabaseSchema = cohortDatabaseSchema,
                       cohortTable = cohortStagingTable,
                       cohortIds = targetCohortIds,
                       minCellCount = minCellCount,
                       createCohortTable = TRUE,
                       generateInclusionStats = FALSE,
                       incremental = incremental,
                       incrementalFolder = incrementalFolder,
                       inclusionStatisticsFolder = exportFolder)
  
  # Next do the strata cohorts
  ParallelLogger::logInfo("******************************************")
  ParallelLogger::logInfo("  ---- Creating strata cohorts  ---- ")
  ParallelLogger::logInfo("******************************************")
  instantiateCohortSet(connectionDetails = connectionDetails,
                       connection = connection,
                       cdmDatabaseSchema = cdmDatabaseSchema,
                       oracleTempSchema = oracleTempSchema,
                       cohortDatabaseSchema = cohortDatabaseSchema,
                       cohortTable = cohortStagingTable,
                       cohortIds = strataCohortIds,
                       minCellCount = minCellCount,
                       createCohortTable = FALSE,
                       generateInclusionStats = FALSE,
                       incremental = incremental,
                       incrementalFolder = incrementalFolder,
                       inclusionStatisticsFolder = exportFolder)
  
  # Create the feature cohorts
  ParallelLogger::logInfo("**********************************************************")
  ParallelLogger::logInfo(" ---- Creating outcome cohorts ---- ")
  ParallelLogger::logInfo("**********************************************************")
  instantiateCohortSet(connectionDetails = connectionDetails,
                       connection = connection,
                       cdmDatabaseSchema = cdmDatabaseSchema,
                       oracleTempSchema = oracleTempSchema,
                       cohortDatabaseSchema = cohortDatabaseSchema,
                       cohortTable = cohortStagingTable,
                       cohortIds = outcomeCohortIds,
                       minCellCount = minCellCount,
                       createCohortTable = FALSE,
                       generateInclusionStats = FALSE,
                       incremental = incremental,
                       incrementalFolder = incrementalFolder,
                       inclusionStatisticsFolder = exportFolder)
  
  # Create the stratified cohorts
  ParallelLogger::logInfo("**********************************************************")
  ParallelLogger::logInfo(" ---- Creating stratified target cohorts ---- ")
  ParallelLogger::logInfo("**********************************************************")
  createBulkStrata(connection = connection,
                   cdmDatabaseSchema = cdmDatabaseSchema,
                   cohortDatabaseSchema = cohortDatabaseSchema,
                   cohortStagingTable = cohortStagingTable,
                   targetIds = targetCohortIds,
                   oracleTempSchema = oracleTempSchema)
  
  # Copy and censor cohorts to the final table
  ParallelLogger::logInfo("**********************************************************")
  ParallelLogger::logInfo(" ---- Copy cohorts to main table ---- ")
  ParallelLogger::logInfo("**********************************************************")
  copyAndCensorCohorts(connection = connection,
                       cohortDatabaseSchema = cohortDatabaseSchema,
                       cohortStagingTable = cohortStagingTable,
                       cohortTable = cohortTable,
                       minCellCount = minCellCount,
                       targetIds = targetCohortIds,
                       oracleTempSchema = oracleTempSchema)
  
  # Compute the features
  ParallelLogger::logInfo("**********************************************************")
  ParallelLogger::logInfo(" ---- Create feature proportions ---- ")
  ParallelLogger::logInfo("**********************************************************")
  createFeatureProportions(connection = connection,
                           cohortDatabaseSchema = cohortDatabaseSchema,
                           cohortStagingTable = cohortStagingTable,
                           cohortTable = cohortTable,
                           featureSummaryTable = featureSummaryTable,
                           oracleTempSchema = oracleTempSchema)
  
  ParallelLogger::logInfo("Saving database metadata")
  database <- data.frame(databaseId = databaseId,
                         databaseName = databaseName,
                         description = databaseDescription,
                         vocabularyVersion = getVocabularyInfo(connection = connection,
                                                               cdmDatabaseSchema = cdmDatabaseSchema,
                                                               oracleTempSchema = oracleTempSchema),
                         isMetaAnalysis = 0)
  writeToCsv(database, file.path(exportFolder, "database.csv"))
  andrData$database <- database
  
  # Counting staging cohorts ---------------------------------------------------------------
  ParallelLogger::logInfo("Counting staging cohorts")
  counts <- getCohortCounts(connection = connection,
                            cohortDatabaseSchema = cohortDatabaseSchema,
                            cohortTable = cohortStagingTable)
  if (nrow(counts) > 0) {
    counts$databaseId <- databaseId
    counts <- enforceMinCellValue(counts, "cohortEntries", minCellCount)
    counts <- enforceMinCellValue(counts, "cohortSubjects", minCellCount)
  }
  allStudyCohorts <- getAllStudyCohorts()
  counts <- dplyr::left_join(x = allStudyCohorts, y = counts, by = "cohortId")
  andrData$cohort_staging_count = counts

  
  # Generate survival info -----------------------------------------------------------------
  ParallelLogger::logInfo("Generating time to event data")
  start <- Sys.time()

  targetIds <- c(targetCohortIds, getTargetStrataXref()$cohortId)
  events <- getTimeToEventSettings()
  timeToEvent <- generateSurvival(connection = connection,
                                  cohortDatabaseSchema = cohortDatabaseSchema,
                                  cohortTable = cohortStagingTable,
                                  targetIds = targetIds,
                                  events = events,
                                  databaseId = databaseId,
                                  packageName = getThisPackageName())
  andrData$cohort_time_to_event <- timeToEvent
  
  delta <- Sys.time() - start
  ParallelLogger::logInfo(paste("Generating time to event data took",
                                signif(delta, 3),
                                attr(delta, "units")))
  
  
  # Generate time to treatment switch info -------------------------------------------------
  ParallelLogger::logInfo("Generating time to treatment switch data")
  ParallelLogger::logInfo("Create treatment tables")
  
  start <- Sys.time()
  sql <- SqlRender::loadRenderTranslateSql(dbms = connection@dbms,
                                           sqlFilename = "TreatmentComplementaryTables.sql",
                                           packageName = getThisPackageName(),
                                           warnOnMissingParameters = TRUE,
                                           cohort_database_schema = cohortDatabaseSchema,
                                           cdm_database_schema = cdmDatabaseSchema,
                                           treatment_cohort_ids = paste(targetIds, collapse = ', '),
                                           cohort_table = cohortStagingTable
                                           )
  DatabaseConnector::executeSql(connection, sql)
  
  
  ParallelLogger::logInfo("Get time to treatment switch")
  deathCohortId <- events %>% dplyr::filter(name == 'Time to Death') %>% dplyr::pull(outcomeCohortIds)
  sql <- SqlRender::loadRenderTranslateSql(dbms = connection@dbms,
                                           sqlFilename = "TimeToTreatmentSwitch.sql",
                                           packageName = getThisPackageName(),
                                           warnOnMissingParameters = TRUE,
                                           cohort_database_schema = cohortDatabaseSchema,
                                           cohort_table = cohortStagingTable,
                                           death_cohort_id = deathCohortId
                                           )
  data <- DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = T)
  
  #calculate locality metrics for Time to Treatment Switch
  browser()
  metrics <- data %>%
    dplyr::filter(event == 1) %>% 
    dplyr::select(cohortDefinitionId, timeToEvent) %>% 
    dplyr::group_by(cohortDefinitionId) %>% 
    dplyr::summarise(minimum = min(timeToEvent),
                       q1 = quantile(timeToEvent, 0.25),
                       median = median(timeToEvent),
                       q3 = quantile(timeToEvent, 0.75),
                       maximum = max(timeToEvent)) %>% 
    dplyr::mutate(iqr = q3 - q1, analysisName = "Time to Treatment Switch") %>% 
    dplyr::relocate(iqr, .before = minimum)
    
  timeToTreatmentSwitch <- purrr::map_df(targetIds, function(targetId){
    data <- data %>% dplyr::filter(cohortDefinitionId == targetId) %>% dplyr::select(id, timeToEvent, event)
    if (nrow(data) < 30 | length(data$event[data$event == 1]) < 1) {return(NULL)}
    surv_info <- survival::survfit(survival::Surv(timeToEvent, event) ~ 1, data = data)
    surv_info <- survminer::surv_summary(surv_info)
    
    data.frame(targetId = targetId, time = surv_info$time, surv = surv_info$surv, 
               n.censor = surv_info$n.censor, n.event = surv_info$n.event, n.risk = surv_info$n.risk,
               lower = surv_info$lower, upper = surv_info$upper, databaseId = databaseId)
  })
  andrData$cohort_time_to_treatment_switch <- timeToTreatmentSwitch
  
  
  ParallelLogger::logInfo("Get sankey data")
  sql <- SqlRender::loadRenderTranslateSql(dbms = connection@dbms,
                                           sqlFilename = "Sankey.sql",
                                           packageName = getThisPackageName(),
                                           warnOnMissingParameters = TRUE,
                                           cohort_database_schema = cohortDatabaseSchema,
                                           second_line_treatment_gap = '0'
                                           )
  data = DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = T)
  
  data = data %>% 
    dplyr::group_by(cohortId, id) %>% 
    dplyr::mutate(first_line = paste(beforeCodesetTag, collapse = " ")) %>%
    dplyr::mutate(second_line = paste(afterCodesetTag, collapse = " ")) %>%
    dplyr::select(id, cohortId, first_line, second_line) %>%
    dplyr::distinct() %>%
    dplyr::mutate(first_line = formatPattern(first_line)) %>%
    dplyr::mutate(second_line = formatPattern(second_line))
  
  treatmentPatternMap <- data.frame(name = unique(data$first_line))
  treatmentPatternMap$code <- 1:nrow(treatmentPatternMap)
  sankeyData <- dplyr::inner_join(data, treatmentPatternMap, by = c('first_line' = 'name')) %>% 
    dplyr::rename('sourceId' = 'code') %>%
    dplyr::rename('sourceName' = 'first_line')
  
  rowCount <- nrow(treatmentPatternMap)
  treatmentPatternMap <- data.frame(name = unique(data$second_line))
  treatmentPatternMap$code <- (rowCount + 1):(nrow(treatmentPatternMap) + rowCount)
  sankeyData <- dplyr::inner_join(sankeyData, treatmentPatternMap, by = c('second_line' = 'name')) %>%
    dplyr::rename('targetId' = 'code') %>%
    dplyr::rename('targetName' = 'second_line')
  
  sankeyData <- sankeyData %>%
    dplyr::group_by(cohortId, sourceName, targetName, sourceId, targetId) %>%
    dplyr::summarise(value = dplyr::n()) %>%
    dplyr::filter(sourceName != 'discontinued') %>% 
    dplyr::select_all()
  sankeyData$databaseId = databaseId
  andrData$treatment_sankey <- sankeyData
  
  delta <- Sys.time() - start
  ParallelLogger::logInfo(paste("Generating time to treatment switch data took",
                                signif(delta, 3),
                                attr(delta, "units")))
  
  # Locality estimation of some time periods ------------------------------------------------
  # median follow-up time
  ParallelLogger::logInfo("Time periods locality estimation")
  start <- Sys.time()
  browser()

  sqlAggreg <- SqlRender::loadRenderTranslateSql(dbms = connection@dbms,
                                                 sqlFilename = file.path("quartiles", "QuartilesAggregation.sql"),
                                                 packageName = getThisPackageName(),
                                                 warnOnMissingParameters = TRUE,
                                                 analysis_name = "Follow up Time")
  
  sql <- SqlRender::loadRenderTranslateSql(dbms = connection@dbms,
                                           sqlFilename = file.path("quartiles", "MedianFollowUp.sql"),
                                           packageName = getThisPackageName(),
                                           warnOnMissingParameters = TRUE,
                                           cohort_database_schema = cohortDatabaseSchema,
                                           cohort_table = cohortTable,
                                           target_ids = paste(targetIds, collapse = ', '))
  
  sql <- paste0(sql, sqlAggreg)
  metrics <- rbind(metrics, DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = T))
 
  sqlAggreg <- SqlRender::loadRenderTranslateSql(dbms = connection@dbms,
                                                 sqlFilename = file.path("quartiles", "QuartilesAggregation.sql"),
                                                 packageName = getThisPackageName(),
                                                 warnOnMissingParameters = TRUE,
                                                 analysis_name = "Time to Treatment")
  
  sql <- SqlRender::loadRenderTranslateSql(dbms = connection@dbms,
                                           sqlFilename = file.path("quartiles", "MedianTimeToTreatment.sql"),
                                           packageName = getThisPackageName(),
                                           warnOnMissingParameters = TRUE,
                                           cdm_database_schema = cdmDatabaseSchema,
                                           cohort_database_schema = cohortDatabaseSchema,
                                           cohort_table = cohortTable,
                                           target_ids = paste(targetIds, collapse = ', '))
  
  sql <- paste0(sql, sqlAggreg)
  metrics <- rbind(metrics, DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = T))

  andrData$metrics_distribution <- metrics

  # drop treatment complementary tables
  sql <- SqlRender::loadRenderTranslateSql(dbms = connection@dbms,
                                           sqlFilename = "TreatmentTablesDrop.sql",
                                           packageName = getThisPackageName(),
                                           warnOnMissingParameters = TRUE,
                                           cohort_database_schema = cohortDatabaseSchema
                                          )
  DatabaseConnector::executeSql(connection, sql)
  
  # Counting cohorts -----------------------------------------------------------------------
  ParallelLogger::logInfo("Counting cohorts")
  counts <- getCohortCounts(connection = connection,
                            cohortDatabaseSchema = cohortDatabaseSchema,
                            cohortTable = cohortTable)
  if (nrow(counts) > 0) {
    counts$databaseId <- databaseId
    counts <- enforceMinCellValue(counts, "cohortEntries", minCellCount)
    counts <- enforceMinCellValue(counts, "cohortSubjects", minCellCount)
  }
  writeToCsv(counts, file.path(exportFolder, "cohort_count.csv"), incremental = incremental, cohortId = counts$cohortId)
  andrData$cohort_count <- counts
  
  # Read in the cohort counts
  counts <- data.table::fread(file.path(exportFolder, "cohort_count.csv"))
  colnames(counts) <- SqlRender::snakeCaseToCamelCase(colnames(counts))
  
  # Export the cohorts from the study
  cohortsForExport <- loadCohortsForExportFromPackage(cohortIds = counts$cohortId)
  andrData$cohort <- cohortsForExport
  
  # Extract feature counts -----------------------------------------------------------------------
  ParallelLogger::logInfo("Extract feature counts")
  featureProportions <- exportFeatureProportions(connection = connection,
                                                 cohortDatabaseSchema = cohortDatabaseSchema,
                                                 cohortTable = cohortTable,
                                                 featureSummaryTable = featureSummaryTable)
  if (nrow(featureProportions) > 0) {
    featureProportions$databaseId <- databaseId
    featureProportions <- enforceMinCellValue(featureProportions, "featureCount", minCellCount)
    featureProportions <- featureProportions[featureProportions$totalCount >= minimumSubjectCountForCharacterization, ]
  }
  features <- formatCovariates(featureProportions)
  andrData$covariate <- features
  featureValues <- formatCovariateValues(featureProportions, counts, minCellCount, databaseId)
  featureValues <- featureValues[,c("cohortId", "covariateId", "mean", "sd", "databaseId")]
  andrData$covariate_value <- featureValues
  # Also keeping a raw output for debugging
  # writeToCsv(featureProportions, file.path(exportFolder, "feature_proportions.csv"))
  
  # Cohort characterization ---------------------------------------------------------------
  # Note to package maintainer: If any of the logic to this changes, you'll need to revist
  # the function createBulkCharacteristics
  runCohortCharacterization <- function(cohortId, cohortName, covariateSettings, windowId, curIndex, totalCount) {
    ParallelLogger::logInfo("- (windowId=", windowId, ", ", curIndex, " of ", totalCount, ") Creating characterization for cohort: ", cohortName)
    data <- getCohortCharacteristics(connection = connection,
                                     cdmDatabaseSchema = cdmDatabaseSchema,
                                     oracleTempSchema = oracleTempSchema,
                                     cohortDatabaseSchema = cohortDatabaseSchema,
                                     cohortTable = cohortTable,
                                     cohortId = cohortId,
                                     covariateSettings = covariateSettings)
    if (nrow(data) > 0) {
      data$cohortId <- cohortId
    }
    
    data$covariateId <- data$covariateId * 10 + windowId
    return(data)
  }
  
  # Subset the cohorts to the target/strata for running feature extraction
  # that are >= 140 per protocol to improve efficency
  featureExtractionCohorts <-  loadCohortsForExportWithChecksumFromPackage(counts[counts$cohortSubjects >= minimumSubjectCountForCharacterization, c("cohortId")]$cohortId)
  # Bulk approach ----------------------
  if (useBulkCharacterization) {
    ParallelLogger::logInfo("********************************************************************************************")
    ParallelLogger::logInfo("Bulk characterization of all cohorts for all time windows")
    ParallelLogger::logInfo("********************************************************************************************")
    createBulkCharacteristics(connection, 
                              oracleTempSchema, 
                              cohortIds = featureExtractionCohorts$cohortId, 
                              cdmDatabaseSchema, 
                              cohortDatabaseSchema, 
                              cohortTable)
    writeBulkCharacteristics(connection, oracleTempSchema, counts, minCellCount, databaseId, exportFolder, andrData)
  } else {
    # Sequential Approach --------------------------------
    if (incremental) {
      recordKeepingFile <- file.path(incrementalFolder, "CreatedAnalyses.csv")
    }
    featureTimeWindows <- getFeatureTimeWindows()
    for (i in 1:nrow(featureTimeWindows)) {
      windowStart <- featureTimeWindows$windowStart[i]
      windowEnd <- featureTimeWindows$windowEnd[i]
      windowId <- featureTimeWindows$windowId[i]
      ParallelLogger::logInfo("********************************************************************************************")
      ParallelLogger::logInfo(paste0("Characterize concept features for start: ", windowStart, ", end: ", windowEnd, " (windowId=", windowId, ")"))
      ParallelLogger::logInfo("********************************************************************************************")
      createDemographics <- (i == 1)
      covariateSettings <- FeatureExtraction::createCovariateSettings(useDemographicsGender = createDemographics,
                                                                      useDemographicsAgeGroup = createDemographics,
                                                                      useConditionGroupEraShortTerm = TRUE,
                                                                      useDrugGroupEraShortTerm = TRUE,
                                                                      shortTermStartDays = windowStart,
                                                                      endDays = windowEnd)
      task <- paste0("runCohortCharacterizationWindowId", windowId)
      if (incremental) {
        subset <- subsetToRequiredCohorts(cohorts = featureExtractionCohorts,
                                          task = task,
                                          incremental = incremental,
                                          recordKeepingFile = recordKeepingFile)
      } else {
        subset <- featureExtractionCohorts
      }
      
      if (nrow(subset) > 0) {
        for (j in 1:nrow(subset)) {
          data <- runCohortCharacterization(cohortId = subset$cohortId[j],
                                            cohortName = subset$cohortName[j],
                                            covariateSettings = covariateSettings,
                                            windowId = windowId,
                                            curIndex = j,
                                            totalCount = nrow(subset))
          covariates <- formatCovariates(data)
          writeToCsv(covariates, file.path(exportFolder, "covariate.csv"), incremental = incremental, covariateId = covariates$covariateId)
          data <- formatCovariateValues(data, counts, minCellCount, databaseId)
          writeToCsv(data, file.path(exportFolder, "covariate_value.csv"), incremental = incremental, cohortId = data$cohortId, data$covariateId)
          if (incremental) {
            recordTasksDone(cohortId = subset$cohortId[j],
                            task = task,
                            checksum = subset$checksum[j],
                            recordKeepingFile = recordKeepingFile,
                            incremental = incremental)
          }
        }
      }
    }
  }
  
  # Format results -----------------------------------------------------------------------------------
  ParallelLogger::logInfo("********************************************************************************************")
  ParallelLogger::logInfo("Formatting Results")
  ParallelLogger::logInfo("********************************************************************************************")
  # Ensure that the covariate_value is free of any duplicate values. This can happen after more than
  # one run of the package.
  andrData$covariate_value <- andrData$covariate_value %>% dplyr::distinct()
  
  
  # # Export to zip file -------------------------------------------------------------------------------
  # exportResults(exportFolder, databaseId, cohortIdsToExcludeFromResultsExport)
  # delta <- Sys.time() - start
  # ParallelLogger::logInfo(paste("Running study took",
  #                               signif(delta, 3),
  #                               attr(delta, "units")))
  Andromeda::saveAndromeda(andrData, file.path(exportFolder, "study_results.zip"))
}

