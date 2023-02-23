
#' @export
exportResults <- function(exportFolder, databaseId, cohortIdsToExcludeFromResultsExport = NULL) {
  filesWithCohortIds <- c("covariate_value.csv","cohort_count.csv")
  tempFolder <- NULL
  ParallelLogger::logInfo("Adding results to zip file")
  if (!is.null(cohortIdsToExcludeFromResultsExport)) {
    ParallelLogger::logInfo("Exclude cohort ids: ", paste(cohortIdsToExcludeFromResultsExport, collapse = ", "))
    # Copy files to temp location to remove the cohorts to remove
    tempFolder <- file.path(exportFolder, "temp")
    files <- list.files(exportFolder, pattern = ".*\\.csv$")
    if (!file.exists(tempFolder)) {
      dir.create(tempFolder)
    }
    file.copy(file.path(exportFolder, files), tempFolder)
    
    # Censor out the cohorts based on the IDs passed in
    for(i in 1:length(filesWithCohortIds)) {
      fileName <- file.path(tempFolder, filesWithCohortIds[i])
      fileContents <- data.table::fread(fileName)
      fileContents <- fileContents[!(fileContents$cohort_id %in% cohortIdsToExcludeFromResultsExport),]
      data.table::fwrite(fileContents, fileName)
    }
    
    # Zip the results and copy to the main export folder
    zipName <- zipResults(tempFolder, databaseId)
    file.copy(zipName, exportFolder)
    unlink(tempFolder, recursive = TRUE)
    zipName <- file.path(exportFolder, basename(zipName))
  } else {
    zipName <- zipResults(exportFolder, databaseId)
  }
  ParallelLogger::logInfo("Results are ready for sharing at:", zipName)
}

formatPattern <- function(raw){
  pattern = sort(strsplit(raw, ' ')[[1]])
  pattern = pattern[pattern != 'NA']
  pattern = paste(pattern, collapse = ' + ')
  if (nchar(pattern) != 0){
    return(pattern)}
  else{
    return('discontinued')
  }
}

zipResults <- function(exportFolder, databaseId) {
  date <- format(Sys.time(), "%Y%m%dT%H%M%S")
  zipName <- file.path(exportFolder, paste0("Results_v", getThisPackageVersion(), "_", databaseId, "_", date, ".zip")) 
  files <- list.files(exportFolder, ".*\\.csv$")
  oldWd <- setwd(exportFolder)
  on.exit(setwd(oldWd), add = TRUE)
  DatabaseConnector::createZipFile(zipFile = zipName, files = files)
  return(zipName)
}

getVocabularyInfo <- function(connection, cdmDatabaseSchema, oracleTempSchema) {
  sql <- "SELECT vocabulary_version FROM @cdm_database_schema.vocabulary WHERE vocabulary_id = 'None';"
  sql <- SqlRender::render(sql, cdm_database_schema = cdmDatabaseSchema)
  sql <- SqlRender::translate(sql, targetDialect = attr(connection, "dbms"), oracleTempSchema = oracleTempSchema)
  vocabInfo <- DatabaseConnector::querySql(connection, sql)
  return(vocabInfo[[1]])
}

#' @export
getUserSelectableCohortGroups <- function() {
  cohortGroups <- getCohortGroups()
  return(unlist(cohortGroups[cohortGroups$userCanSelect == TRUE, c("cohortGroup")], use.names = FALSE))
}

formatCovariates <- function(data) {
  # Drop covariates with mean = 0 after rounding to 4 digits:
  if (nrow(data) > 0) {
    data <- data[round(data$mean, 4) != 0, ]
    covariates <- unique(data.table::setDT(data[, c("covariateId", "covariateName", "analysisId")]))
    colnames(covariates)[[3]] <- "covariateAnalysisId"
  } else {
    covariates <- data.table::data.table("covariateId" = integer(), "covariateName" = character(), "covariateAnalysisId" = integer())
  }
  return(covariates)
}

formatCovariateValues <- function(data, counts, minCellCount, databaseId) {
  data$covariateName <- NULL
  data$analysisId <- NULL
  if (nrow(data) > 0) {
    data$databaseId <- databaseId
    data <- merge(data, counts[, c("cohortId", "cohortEntries")])
    data <- enforceMinCellValue(data, "mean", minCellCount/data$cohortEntries)
    data$sd[data$mean < 0] <- NA
    data$cohortEntries <- NULL
    data$mean <- round(data$mean, 3)
    data$sd <- round(data$sd, 3)
  }
  return(data)  
}

loadCohortsFromPackage <- function(cohortIds) {
  packageName = getThisPackageName()
  cohorts <- getCohortsToCreate()
  cohorts <- cohorts %>%  dplyr::mutate(atlasId = NULL)
  if (!is.null(cohortIds)) {
    cohorts <- cohorts[cohorts$cohortId %in% cohortIds, ]
  }
  if ("atlasName" %in% colnames(cohorts)) {
    # Remove PIONEER cohort identifier (3.g. [PIONEER O2])
    cohorts <- cohorts %>% 
      dplyr::mutate(cohortName = trimws(gsub("(\\[.+?\\])", "", atlasName)),
                    cohortFullName = atlasName) %>%
      dplyr::select(-atlasName, -name)
  } else {
    cohorts <- cohorts %>% dplyr::rename(cohortName = name, cohortFullName = fullName)
  }
  
  getSql <- function(name) {
    pathToSql <- system.file("sql", "sql_server", paste0(name, ".sql"), package = packageName, mustWork = TRUE)
    sql <- readChar(pathToSql, file.info(pathToSql)$size)
    return(sql)
  }
  cohorts$sql <- sapply(cohorts$cohortId, getSql)
  getJson <- function(name) {
    pathToJson <- system.file("cohorts", paste0(name, ".json"), package = packageName, mustWork = TRUE)
    json <- readChar(pathToJson, file.info(pathToJson)$size)
    return(json)
  }
  cohorts$json <- sapply(cohorts$cohortId, getJson)
  return(cohorts)
}

loadCohortsForExportFromPackage <- function(cohortIds) {
  packageName = getThisPackageName()
  cohorts <- getCohortsToCreate()
  cohorts <- cohorts %>%  dplyr::mutate(atlasId = NULL)
  if ("atlasName" %in% colnames(cohorts)) {
    # Remove PIONEER cohort identifier (3.g. [PIONEER O2])
    # Remove atlasName and name from object to prevent clashes when combining with stratXref
    cohorts <- cohorts %>% 
      dplyr::mutate(cohortName = trimws(gsub("(\\[.+?\\])", "", atlasName)),
                    cohortFullName = atlasName) %>%
      dplyr::select(-atlasName, -name)
  } else {
    cohorts <- cohorts %>% dplyr::rename(cohortName = name, cohortFullName = fullName)
  }
  
  # Get the stratified cohorts for the study
  # and join to the cohorts to create to get the names
  targetStrataXref <- getTargetStrataXref() 
  targetStrataXref <- targetStrataXref %>% 
    dplyr::rename(cohortName = name) %>%
    dplyr::mutate(cohortFullName = cohortName,
                  targetId = NULL,
                  strataId = NULL)
  
  cols <- names(cohorts)
  cohorts <- rbind(cohorts, targetStrataXref[,..cols])
  
  if (!is.null(cohortIds)) {
    cohorts <- cohorts[cohorts$cohortId %in% cohortIds, ]
  }
  
  return(cohorts)
}

loadCohortsForExportWithChecksumFromPackage <- function(cohortIds) {
  packageName = getThisPackageName()
  strata <- getAllStrata()
  targetStrataXref <- getTargetStrataXref()
  cohorts <- loadCohortsForExportFromPackage(cohortIds)
  
  # Match up the cohorts in the study w/ the targetStrataXref and 
  # set the target/strata columns
  cohortsWithStrata <- dplyr::left_join(cohorts, targetStrataXref, by = "cohortId")
  cohortsWithStrata <- dplyr::rename(cohortsWithStrata, cohortType = "cohortType.x")
  cohortsWithStrata$targetId <- ifelse(is.na(cohortsWithStrata$targetId), cohortsWithStrata$cohortId, cohortsWithStrata$targetId)
  cohortsWithStrata$strataId <- ifelse(is.na(cohortsWithStrata$strataId), 0, cohortsWithStrata$strataId)
  
  getChecksum <- function(targetId, strataId, cohortType) {
    pathToSql <- system.file("sql", "sql_server", paste0(targetId, ".sql"), package = packageName, mustWork = TRUE)
    sql <- readChar(pathToSql, file.info(pathToSql)$size)
    if (strataId > 0) {
      sqlFileName <- strata[strata$cohortId == strataId, c("generationScript")][[1]]
      pathToSql <- system.file("sql", "sql_server", sqlFileName, package = packageName, mustWork = TRUE)
      strataSql <- readChar(pathToSql, file.info(pathToSql)$size)
      sql <- paste(sql, strataSql, cohortType)
    }
    checksum <- computeChecksum(sql)
    return(checksum)
  }
  cohortsWithStrata$checksum <- mapply(getChecksum, 
                                       cohortsWithStrata$targetId, 
                                       strataId = cohortsWithStrata$strataId, 
                                       cohortType = cohortsWithStrata$cohortType)
  
  if (!is.null(cohortIds)) {
    cohortsWithStrata <- cohortsWithStrata[cohortsWithStrata$cohortId %in% cohortIds, ]
  }
  
  return(cohortsWithStrata)
}

writeToCsv <- function(data, fileName, incremental = FALSE, ...) {
  colnames(data) <- SqlRender::camelCaseToSnakeCase(colnames(data))
  if (incremental) {
    params <- list(...)
    names(params) <- SqlRender::camelCaseToSnakeCase(names(params))
    params$data = data
    params$fileName = fileName
    do.call(saveIncremental, params)
  } else {
    data.table::fwrite(data, fileName)
  }
}

enforceMinCellValue <- function(data, fieldName, minValues, silent = FALSE) {
  toCensor <- !is.na(data[, fieldName]) & data[, fieldName] < minValues & data[, fieldName] != 0
  if (!silent) {
    percent <- round(100 * sum(toCensor)/nrow(data), 1)
    ParallelLogger::logInfo("   censoring ",
                            sum(toCensor),
                            " values (",
                            percent,
                            "%) from ",
                            fieldName,
                            " because value below minimum")
  }
  if (length(minValues) == 1) {
    data[toCensor, fieldName] <- -minValues
  } else {
    data[toCensor, fieldName] <- -minValues[toCensor]
  }
  return(data)
}

getCohortCounts <- function(connectionDetails = NULL,
                            connection = NULL,
                            cohortDatabaseSchema,
                            cohortTable = "cohort",
                            cohortIds = c()) {
  start <- Sys.time()
  
  if (is.null(connection)) {
    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))
  }
  sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "CohortCounts.sql",
                                           packageName = getThisPackageName(),
                                           dbms = connection@dbms,
                                           cohort_database_schema = cohortDatabaseSchema,
                                           cohort_table = cohortTable,
                                           cohort_ids = cohortIds)
  counts <- DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = TRUE)
  delta <- Sys.time() - start
  ParallelLogger::logInfo(paste("Counting cohorts took",
                                signif(delta, 3),
                                attr(delta, "units")))
  return(counts)
  
}

subsetToRequiredCohorts <- function(cohorts, task, incremental, recordKeepingFile) {
  if (incremental) {
    tasks <- getRequiredTasks(cohortId = cohorts$cohortId,
                              task = task,
                              checksum = cohorts$checksum,
                              recordKeepingFile = recordKeepingFile)
    return(cohorts[cohorts$cohortId %in% tasks$cohortId, ])
  } else {
    return(cohorts)
  }
}

getKeyIndex <- function(key, recordKeeping) {
  if (nrow(recordKeeping) == 0 || length(key[[1]]) == 0 || !all(names(key) %in% names(recordKeeping))) {
    return(c())
  } else {
    key <- unique(tibble::as_tibble(key))
    recordKeeping$idxCol <- 1:nrow(recordKeeping)
    idx <- merge(recordKeeping, key)$idx
    return(idx)
  }
}

recordTasksDone <- function(..., checksum, recordKeepingFile, incremental = TRUE) {
  if (!incremental) {
    return()
  }
  if (length(list(...)[[1]]) == 0) {
    return()
  }
  if (file.exists(recordKeepingFile)) {
    recordKeeping <- data.table::fread(recordKeepingFile)
    idx <- getKeyIndex(list(...), recordKeeping)
    if (length(idx) > 0) {
      recordKeeping <- recordKeeping[-idx, ]
    }
  } else {
    recordKeeping <- tibble::tibble()
  }
  newRow <- tibble::as_tibble(list(...))
  newRow$checksum <- checksum
  newRow$timeStamp <-  Sys.time()
  recordKeeping <- dplyr::bind_rows(recordKeeping, newRow)
  data.table::fwrite(recordKeeping, recordKeepingFile)
}

saveIncremental <- function(data, fileName, ...) {
  if (length(list(...)[[1]]) == 0) {
    return()
  }
  if (file.exists(fileName)) {
    previousData <- data.table::fread(fileName)
    idx <- getKeyIndex(list(...), previousData)
    if (length(idx) > 0) {
      previousData <- previousData[-idx, ] 
    }
    data <- dplyr::bind_rows(previousData, data)
  } 
  data.table::fwrite(data, fileName)
}