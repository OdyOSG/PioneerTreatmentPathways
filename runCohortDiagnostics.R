

# Run cohort diagnostics for Pioneer Treatment Patterns study

# This only needs to be done once
# install.packages("devtools")
# install.packages("survminer")
# devtools::install_github("OHDSI/CohortDiagnostics")
# devtools::install_github("OHDSI/CohortGenerator")
# devtools::install_github("OHDSI/CohortDiagnostics")


library(DatabaseConnector)
library(dplyr, warn.conflicts = FALSE)
library(here)
library(glue)
# connectionDetails <- createConnectionDetails("postgresql", user = "postgres", password = "", server = "localhost/covid")
# con <- connect(cd)
# dbGetQuery(con, "select * from cdm5.person limit 8")
# disconnect(con)
# 
# connectionDetails <- createConnectionDetails(dbms = "postgresql",
#                                              server = "testnode.arachnenetwork.com/synpuf_110k",
#                                              user = "ohdsi",
#                                              password = Sys.getenv("ODYS_DB_PASSWORD"),
#                                              port = "5441")

connectionDetails <- createConnectionDetails(dbms = "postgresql",
                                             server = "localhost/synpuf100k",
                                             user = "postgres",
                                             password = "")

connection = NULL
# cdmDatabaseSchema = "cdm5"
cdmDatabaseSchema = "cdm531"
oracleTempSchema = NULL
cohortDatabaseSchema = "scratch"
# cohortDatabaseSchema = "adam_black_results"
cohortStagingTable = "cohort_stg"
cohortTable = "cohort"
featureSummaryTable = "cohort_smry"
cohortIdsToExcludeFromExecution = c()
cohortIdsToExcludeFromResultsExport = NULL
# cohortGroups = getUserSelectableCohortGroups()
exportFolder = here::here("export")
databaseId = "Synpuf"
databaseName = databaseId
databaseDescription = "Synthetic medicare claims 100k person sample"
useBulkCharacterization = FALSE
minCellCount = 5
minimumSubjectCountForCharacterization = 140
```


```{r source-R-files}
# Source R code files in this project
purrr::walk(list.files(here("R"), full.names = TRUE), source)

if (!file.exists(exportFolder)) {
  dir.create(exportFolder, recursive = TRUE)
}

ParallelLogger::addDefaultFileLogger(file.path(exportFolder, "PioneerTreatmentPathways.txt"))
ParallelLogger::logInfo(.systemInfo())
```


## Check connection to database

Confirm that the database connection works and that we have write access to the `cohortDatabaseSchema`.

```{r check-database-access}
conn <- connect(connectionDetails)

# check query access
df <- renderTranslateQuerySql(conn, 
                              "select count(*) as n_persons from @cdmDatabaseSchema.person",
                              cdmDatabaseSchema = cdmDatabaseSchema) 

names(df) <- tolower(names(df))

message(glue::glue("Number of rows in the person table: {df$n_persons}"))

# check write access
insertTable(conn, cohortDatabaseSchema, "iris_temp", iris)

df <- renderTranslateQuerySql(conn, 
                              "select count(*) as n from @cohortDatabaseSchema.iris_temp",
                              cohortDatabaseSchema = cohortDatabaseSchema)

names(df) <- tolower(names(df))

renderTranslateExecuteSql(conn, 
                          "drop table @cohortDatabaseSchema.iris_temp",
                          cohortDatabaseSchema = cohortDatabaseSchema,
                          progressBar = FALSE)

if (df$n != nrow(iris)) {
  rlang::abort("Error with database write access check")
} else {
  message("Write access confirmed")
}

disconnect(conn)
```

# Save database metadata

```{r}
ParallelLogger::logInfo("Saving database metadata")

con <- connect(connectionDetails)

vocabInfo <- renderTranslateQuerySql(
  con,
  glue("SELECT vocabulary_version FROM {cdmDatabaseSchema}.vocabulary WHERE vocabulary_id = 'None';"))

database <- data.frame(databaseId = databaseId,
                       databaseName = databaseName,
                       description = databaseDescription,
                       vocabularyVersion = vocabInfo[[1]],
                       isMetaAnalysis = 0)

readr::write_csv(database, file.path(exportFolder, "database.csv"))

andrData <- Andromeda::andromeda()
andrData$database <- database

disconnect(con)
```


# Generate Study Cohorts

```{r}
start <- Sys.time()

# Instantiate cohorts -----------------------------------------------------------------------
cohortDefinitionSet <- readr::read_csv(file.path("inst", "settings", "CohortsToCreate.csv"), 
                                       show_col_types = FALSE) %>% 
  mutate(cohortName = name,
         sqlPath = file.path("inst", "sql", paste0(name, ".sql")),
         sql = purrr::map_chr(sqlPath, readr::read_file))

cohortTableNames <- CohortGenerator::getCohortTableNames(cohortStagingTable)

if (!file.exists(here(exportFolder, "incremental", "GeneratedCohorts.csv"))) {
  CohortGenerator::createCohortTables(
    connectionDetails = connectionDetails,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTableNames = cohortTableNames
  )
}

ParallelLogger::logInfo("**********************************************************")
ParallelLogger::logInfo("  ---- Creating cohorts ---- ")
ParallelLogger::logInfo("**********************************************************")

CohortGenerator::generateCohortSet(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = cdmDatabaseSchema,
  cohortDatabaseSchema = cohortDatabaseSchema,
  cohortDefinitionSet = cohortDefinitionSet,
  cohortTableNames = cohortTableNames,
  incremental = TRUE,
  incrementalFolder = file.path(exportFolder, "incremental")
)

cohortCounts <- CohortGenerator::getCohortCounts(
  connectionDetails = connectionDetails,
  cohortDatabaseSchema = cohortDatabaseSchema,
  cohortTable = cohortStagingTable) %>%  
  left_join(select(cohortDefinitionSet, cohortId, cohortName = atlasName, group), ., by = "cohortId") %>% 
  mutate(cohortEntries = coalesce(cohortEntries, 0),
         cohortSubjects = coalesce(cohortSubjects, 0),
         databaseId = databaseId)

readr::write_csv(cohortCounts, file.path("export", paste0(databaseId, "CohortCounts.csv")))

if (all(filter(cohortCounts, group == "Target") %>% pull(cohortEntries) == 0)) {
  stop("All target cohorts are empty. You cannot execute this study.")
}

# Copy and censor cohorts to the final table
ParallelLogger::logInfo("**********************************************************")
ParallelLogger::logInfo(" ---- Copy cohorts to main table ---- ")
ParallelLogger::logInfo("**********************************************************")
copyAndCensorCohorts(connectionDetails = connectionDetails,
                     cohortDatabaseSchema = cohortDatabaseSchema,
                     cohortStagingTable = cohortStagingTable,
                     cohortTable = cohortTable,
                     minCellCount = minCellCount)

con <- DatabaseConnector::connect(connectionDetails)

n1 <- renderTranslateQuerySql(con, glue::glue("select count(*) as n from {cohortDatabaseSchema}.{cohortStagingTable}")) %>%
  rename_all(tolower) %>% 
  pull(n)

n2 <- renderTranslateQuerySql(con, glue::glue("select count(*) as n from {cohortDatabaseSchema}.{cohortTable}")) %>%
  rename_all(tolower) %>% 
  pull(n)

DatabaseConnector::disconnect(con)
message(glue("cohort staging table created with {n1} rows."))
message(glue("cohort table created with {n2} rows."))

delta <- Sys.time() - start
msg <- paste("Generating cohorts took", signif(delta, 3), attr(delta, "units"))
ParallelLogger::logInfo(msg)

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
      CohortDiagnostics::runCohortDiagnostics(packageName = getThisPackageName(),
                                              connection = connection,
                                              cohortToCreateFile = cohortGroups$fileName[i],
                                              connectionDetails = connectionDetails,
                                              cdmDatabaseSchema = cdmDatabaseSchema,
                                              oracleTempSchema = oracleTempSchema,
                                              cohortDatabaseSchema = cohortDatabaseSchema,
                                              cohortTable = cohortStagingTable,
                                              cohortIds = cohorts$cohortId,
                                              inclusionStatisticsFolder = diagnosticOutputFolder,
                                              exportFolder = cohortGroups$outputFolder[i],
                                              databaseId = databaseId,
                                              databaseName = databaseName,
                                              databaseDescription = databaseDescription,
                                              runInclusionStatistics = FALSE,
                                              runIncludedSourceConcepts = TRUE,
                                              runOrphanConcepts = TRUE,
                                              runTimeDistributions = TRUE,
                                              runBreakdownIndexEvents = TRUE,
                                              runIncidenceRate = TRUE,
                                              runCohortOverlap = FALSE,
                                              runCohortCharacterization = FALSE,
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