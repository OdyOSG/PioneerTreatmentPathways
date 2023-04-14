# Copyright 2021 Observational Health Data Sciences and Informatics
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

copyAndCensorCohorts <- function(connectionDetails,
                                 cohortDatabaseSchema,
                                 cohortStagingTable,
                                 cohortTable,
                                 targetIds = NULL, 
                                 minCellCount = 5) {
  
  connection <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection), add = TRUE)
  
  targetStrataXref <- readr::read_csv(file.path("inst", "settings", "targetStrataXref.csv"), 
                                      show_col_types = FALSE)
  
  if (!is.null(targetIds)) {
    stopifnot(is.numeric(targetIds))
    tsXrefSubset <- targetStrataXref[targetStrataXref$targetId %in% targetIds, ]
  } else {
    tsXrefSubset <- targetStrataXref
  }
  
  unions <- paste(glue::glue("SELECT 
              {targetId} AS target_id,
              {strataId} AS strata_id,
              {cohortId} AS cohort_id,
              '{cohortType}' AS cohort_type", 
              .envir = tsXrefSubset),
        collapse = "\nUNION ALL \n")
  
  createSql <- glue::glue(
    "WITH data AS ({unions}) 
    SELECT target_id, strata_id, cohort_id, cohort_type
    INTO #TARGET_STRATA_XREF 
    FROM data;")
  
  sql <- "
  @target_strata_xref_table_create
  
  IF OBJECT_ID('@cohort_database_schema.@cohort_table', 'U') IS NOT NULL
  DROP TABLE @cohort_database_schema.@cohort_table;
  
  CREATE TABLE @cohort_database_schema.@cohort_table (
    cohort_definition_id INT,
    subject_id BIGINT,
    cohort_start_date DATE,
    cohort_end_date DATE
  );
  
  --summarize counts of cohorts so we can filter to those that are feasible
  select cohort_definition_id, count(distinct subject_id) as num_persons
  into #cohort_summary
  from @cohort_database_schema.@cohort_staging_table
  group by cohort_definition_id
  ;
  
  --find all feasible analyses:   T > X;   TwS and TwoS  > X
  INSERT INTO @cohort_database_schema.@cohort_table (
    cohort_definition_id,
    subject_id,
    cohort_start_date,
    cohort_end_date
  )
  -- T > X;
  select 
  s.cohort_definition_id,
  s.subject_id,
  s.cohort_start_date,
  s.cohort_end_date
  from @cohort_database_schema.@cohort_staging_table as s
  inner join (
    SELECT cs.cohort_definition_id
    FROM #cohort_summary cs
    INNER JOIN (SELECT DISTINCT target_id cohort_definition_id FROM #TARGET_STRATA_XREF) t 
                ON t.cohort_definition_id = cs.cohort_definition_id 
                where cs.num_persons > @min_cell_count
                UNION ALL
                -- Bulk strata cohorts will contain only 1 entry
                -- so they must be identified by the presence of only a single
                -- cohort_type
                SELECT DISTINCT xref.cohort_id
                FROM (
                  SELECT strata_id, target_id, COUNT(DISTINCT cohort_type) cnt
                  FROM #TARGET_STRATA_XREF
                  group by strata_id, target_id HAVING COUNT(DISTINCT cohort_type) = 1
                ) single
                INNER JOIN #TARGET_STRATA_XREF xref ON single.strata_id = xref.strata_id
                AND single.target_id = xref.target_id
    ) cs1 on s.cohort_definition_id = cs1.cohort_definition_id
    
    union all
    -- TwS and TwoS  > X
    select 
      s.cohort_definition_id,
      s.subject_id,
      s.cohort_start_date,
      s.cohort_end_date
    from @cohort_database_schema.@cohort_staging_table as s
    inner join (
      SELECT cr1.cohort_id cohort_definition_id
      from #TARGET_STRATA_XREF cr1
      inner join #cohort_summary cs1
      on cr1.cohort_id = cs1.cohort_definition_id
      inner join #TARGET_STRATA_XREF cr2
      on cr1.target_id = cr2.target_id
      and cr1.strata_id = cr2.strata_id
      and cr1.cohort_type <> cr2.cohort_type
      inner join #cohort_summary cs2
      on cr2.cohort_id = cs2.cohort_definition_id 
      where cs1.num_persons > @min_cell_count
      and cs2.num_persons > @min_cell_count
    ) cs1 ON s.cohort_definition_id = cs1.cohort_definition_id
    ;
    
    CREATE INDEX IDX_@cohort_table ON @cohort_database_schema.@cohort_table (cohort_definition_id, subject_id, cohort_start_date);
    
    TRUNCATE TABLE #cohort_summary;
    DROP TABLE #cohort_summary;
    
    TRUNCATE TABLE #TARGET_STRATA_XREF;
    DROP TABLE #TARGET_STRATA_XREF;"
  
  sql <- SqlRender::render(sql,
                           warnOnMissingParameters = TRUE,
                           cohort_database_schema = cohortDatabaseSchema,
                           cohort_staging_table = cohortStagingTable,
                           cohort_table = cohortTable,
                           min_cell_count = minCellCount,
                           target_strata_xref_table_create = createSql) %>% 
  assertNoParameters() %>% 
  SqlRender::translate(targetDialect = attr(connection, "dbms"))
  
  ParallelLogger::logInfo("Copy and censor cohorts to main analysis table")
  DatabaseConnector::executeSql(connection, sql)
}


#' Get statistics on cohort inclusion criteria
#'
#' @template Connection
#'
#' @param cohortTable                  Name of the cohort table. Used only to conveniently derive names
#'                                     of the four rule statistics tables.
#' @param cohortId                     The cohort definition ID used to reference the cohort in the
#'                                     cohort table.
#' @param simplify                     Simply output the attrition table?
#' @param resultsDatabaseSchema        Schema name where the statistics tables reside. Note that for
#'                                     SQL Server, this should include both the database and schema
#'                                     name, for example 'scratch.dbo'.
#' @param cohortInclusionTable         Name of the inclusion table, one of the tables for storing
#'                                     inclusion rule statistics.
#' @param cohortInclusionResultTable   Name of the inclusion result table, one of the tables for
#'                                     storing inclusion rule statistics.
#' @param cohortInclusionStatsTable    Name of the inclusion stats table, one of the tables for storing
#'                                     inclusion rule statistics.
#' @param cohortSummaryStatsTable      Name of the summary stats table, one of the tables for storing
#'                                     inclusion rule statistics.
#'
#' @return
#' If `simplify = TRUE`, this function returns a single data frame. Else a list of data frames is
#' returned.
#'
#' @export
getInclusionStatistics <- function(connectionDetails = NULL,
                                   connection = NULL,
                                   resultsDatabaseSchema,
                                   cohortId,
                                   simplify = TRUE,
                                   cohortTable = "cohort",
                                   cohortInclusionTable = paste0(cohortTable, "_inclusion"),
                                   cohortInclusionResultTable = paste0(cohortTable,
                                                                       "_inclusion_result"),
                                   cohortInclusionStatsTable = paste0(cohortTable,
                                                                      "_inclusion_stats"),
                                   cohortSummaryStatsTable = paste0(cohortTable,
                                                                    "_summary_stats")) {
  start <- Sys.time()
  ParallelLogger::logInfo("Fetching inclusion statistics for cohort with cohort_definition_id = ",
                          cohortId)
  if (is.null(connection)) {
    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))
  }
  fetchStats <- function(table) {
    ParallelLogger::logDebug("- Fetching data from ", table)
    sql <- "SELECT * FROM @database_schema.@table WHERE cohort_definition_id = @cohort_id"
    DatabaseConnector::renderTranslateQuerySql(sql = sql,
                                               connection = connection,
                                               snakeCaseToCamelCase = TRUE,
                                               database_schema = resultsDatabaseSchema,
                                               table = table,
                                               cohort_id = cohortId)
  }
  inclusion <- fetchStats(cohortInclusionTable)
  summaryStats <- fetchStats(cohortSummaryStatsTable)
  inclusionStats <- fetchStats(cohortInclusionStatsTable)
  inclusionResults <- fetchStats(cohortInclusionResultTable)
  result <- processInclusionStats(inclusion = inclusion,
                                  inclusionResults = inclusionResults,
                                  inclusionStats = inclusionStats,
                                  summaryStats = summaryStats,
                                  simplify = simplify)
  delta <- Sys.time() - start
  writeLines(paste("Fetching inclusion statistics took", signif(delta, 3), attr(delta, "units")))
  return(result)
}

#' Get inclusion criteria statistics from files
#'
#' @description
#' Gets inclusion criteria statistics from files, as stored when using the
#' \code{ROhdsiWebApi::insertCohortDefinitionSetInPackage} function with \code{generateStats = TRUE}.
#'
#' @param cohortId                    The cohort definition ID used to reference the cohort in the
#'                                    cohort table.
#' @param simplify                    Simply output the attrition table?
#' @param folder                      The path to the folder where the inclusion statistics are stored.
#' @param cohortInclusionFile         Name of the inclusion table, one of the tables for storing
#'                                    inclusion rule statistics.
#' @param cohortInclusionResultFile   Name of the inclusion result table, one of the tables for storing
#'                                    inclusion rule statistics.
#' @param cohortInclusionStatsFile    Name of the inclusion stats table, one of the tables for storing
#'                                    inclusion rule statistics.
#' @param cohortSummaryStatsFile      Name of the summary stats table, one of the tables for storing
#'                                    inclusion rule statistics.
#'
#' @return
#' If `simplify = TRUE`, this function returns a single data frame. Else a list of data frames is
#' returned.
#'
#' @export
getInclusionStatisticsFromFiles <- function(cohortId,
                                            folder,
                                            cohortInclusionFile = file.path(folder,
                                                                            "cohortInclusion.csv"),
                                            cohortInclusionResultFile = file.path(folder,
                                                                                  "cohortIncResult.csv"),
                                            cohortInclusionStatsFile = file.path(folder,
                                                                                 "cohortIncStats.csv"),
                                            cohortSummaryStatsFile = file.path(folder,
                                                                               "cohortSummaryStats.csv"),
                                            simplify = TRUE) {
  start <- Sys.time()
  ParallelLogger::logInfo("Fetching inclusion statistics for cohort with cohort_definition_id = ",
                          cohortId)
  
  fetchStats <- function(file) {
    ParallelLogger::logDebug("- Fetching data from ", file)
    stats <- data.table::fread(file)
    stats <- stats[stats$cohortDefinitionId == cohortId, ]
    return(stats)
  }
  inclusion <- fetchStats(cohortInclusionFile)
  summaryStats <- fetchStats(cohortSummaryStatsFile)
  inclusionStats <- fetchStats(cohortInclusionStatsFile)
  inclusionResults <- fetchStats(cohortInclusionResultFile)
  result <- processInclusionStats(inclusion = inclusion,
                                  inclusionResults = inclusionResults,
                                  inclusionStats = inclusionStats,
                                  summaryStats = summaryStats,
                                  simplify = simplify)
  delta <- Sys.time() - start
  writeLines(paste("Fetching inclusion statistics took", signif(delta, 3), attr(delta, "units")))
  return(result)
}

processInclusionStats <- function(inclusion,
                                  inclusionResults,
                                  inclusionStats,
                                  summaryStats,
                                  simplify) {
  if (simplify) {
    if (nrow(inclusion) == 0 || nrow(inclusionStats) == 0) {
      return(data.frame())
    }
    result <- merge(unique(inclusion[, c("ruleSequence", "name")]),
                    inclusionStats[inclusionStats$modeId ==
                                     0, c("ruleSequence", "personCount", "gainCount", "personTotal")], )
    
    result$remain <- rep(0, nrow(result))
    inclusionResults <- inclusionResults[inclusionResults$modeId == 0, ]
    mask <- 0
    for (ruleId in 0:(nrow(result) - 1)) {
      mask <- bitwOr(mask, 2^ruleId)
      idx <- bitwAnd(inclusionResults$inclusionRuleMask, mask) == mask
      result$remain[result$ruleSequence == ruleId] <- sum(inclusionResults$personCount[idx])
    }
    colnames(result) <- c("ruleSequenceId",
                          "ruleName",
                          "meetSubjects",
                          "gainSubjects",
                          "totalSubjects",
                          "remainSubjects")
  } else {
    if (nrow(inclusion) == 0) {
      return(list())
    }
    result <- list(inclusion = inclusion,
                   inclusionResults = inclusionResults,
                   inclusionStats = inclusionStats,
                   summaryStats = summaryStats)
  }
  return(result)
}


createTempInclusionStatsTables <- function(connection, oracleTempSchema, cohorts) {
  ParallelLogger::logInfo("Creating temporary inclusion statistics tables")
  pathToSql <- system.file( "inclusionStatsTables.sql", package = "ROhdsiWebApi", mustWork = TRUE)
  sql <- SqlRender::readSql(pathToSql)
  sql <- SqlRender::translate(sql, targetDialect = connection@dbms, oracleTempSchema = oracleTempSchema)
  DatabaseConnector::executeSql(connection, sql)
  
  inclusionRules <- data.frame()
  for (i in 1:nrow(cohorts)) {
    cohortDefinition <- RJSONIO::fromJSON(cohorts$json[i])
    if (!is.null(cohortDefinition$InclusionRules)) {
      nrOfRules <- length(cohortDefinition$InclusionRules)
      if (nrOfRules > 0) {
        for (j in 1:nrOfRules) {
          inclusionRules <- rbind(inclusionRules, data.frame(cohortId = cohorts$cohortId[i],
                                                             ruleSequence = j - 1,
                                                             ruleName = cohortDefinition$InclusionRules[[j]]$name))
        }
      }
    }
  }
  inclusionRules <- merge(inclusionRules, data.frame(cohortId = cohorts$cohortId,
                                                     cohortName = cohorts$cohortFullName))
  inclusionRules <- data.frame(cohort_definition_id = inclusionRules$cohortId,
                               rule_sequence = inclusionRules$ruleSequence,
                               name = inclusionRules$ruleName)
  DatabaseConnector::insertTable(connection = connection,
                                 tableName = "#cohort_inclusion",
                                 data = inclusionRules,
                                 dropTableIfExists = FALSE,
                                 createTable = FALSE,
                                 tempTable = TRUE,
                                 oracleTempSchema = oracleTempSchema)
  
}

saveAndDropTempInclusionStatsTables <- function(connection, 
                                                oracleTempSchema, 
                                                inclusionStatisticsFolder, 
                                                incremental,
                                                cohortIds) {
  fetchStats <- function(table, fileName) {
    ParallelLogger::logDebug("- Fetching data from ", table)
    sql <- "SELECT * FROM @table"
    data <- DatabaseConnector::renderTranslateQuerySql(sql = sql,
                                                       connection = connection,
                                                       oracleTempSchema = oracleTempSchema,
                                                       snakeCaseToCamelCase = TRUE,
                                                       table = table)
    fullFileName <- file.path(inclusionStatisticsFolder, fileName)
    if (incremental) {
      saveIncremental(data, fullFileName, cohortDefinitionId = cohortIds)
    } else {
      data.table::fread(data, fullFileName)
    }
  }
  fetchStats("#cohort_inclusion", "cohortInclusion.csv")
  fetchStats("#cohort_inc_result", "cohortIncResult.csv")
  fetchStats("#cohort_inc_stats", "cohortIncStats.csv")
  fetchStats("#cohort_summary_stats", "cohortSummaryStats.csv")
  
  sql <- "TRUNCATE TABLE #cohort_inclusion; 
    DROP TABLE #cohort_inclusion;
    
    TRUNCATE TABLE #cohort_inc_result; 
    DROP TABLE #cohort_inc_result;
    
    TRUNCATE TABLE #cohort_inc_stats; 
    DROP TABLE #cohort_inc_stats;
    
    TRUNCATE TABLE #cohort_summary_stats; 
    DROP TABLE #cohort_summary_stats;"
  DatabaseConnector::renderTranslateExecuteSql(connection = connection,
                                               sql = sql,
                                               progressBar = FALSE,
                                               reportOverallTime = FALSE,
                                               oracleTempSchema = oracleTempSchema)
}
