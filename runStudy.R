# title: "PIONEER Treatment Patterns Study"
  

# This only needs to be done once
# install.packages("devtools")
# install.packages("survminer")
# devtools::install_github("OHDSI/CohortDiagnostics")
# devtools::install_github("OHDSI/CohortGenerator")


# Setup -------------
library(DatabaseConnector)
library(dplyr, warn.conflicts = FALSE)
library(here)
library(glue)

# Database Connection info
connectionDetails <- createConnectionDetails(dbms = "postgresql",
                                             server = "localhost/synpuf100k",
                                             user = "postgres",
                                             password = "")


cdmDatabaseSchema = "cdm531"
cohortDatabaseSchema = "scratch"
cohortTable = "cohort"

# CDM metadata
databaseId = "Synpuf"
databaseName = databaseId
databaseDescription = "Synthetic medicare claims 100k person sample"

# Where to save output
exportFolder = here::here("output")

minCellCount = 5
minimumSubjectCountForCharacterization = 140


cohortTable = "cohort"
featureSummaryTable = "cohort_smry"


# The R folder contains some helpful R functions that we will use in the study. Load/source them into the R environment.

# Source R code files in this project
purrr::walk(list.files(here::here("R"), full.names = TRUE), source)

if (!file.exists(exportFolder)) {
  dir.create(exportFolder, recursive = TRUE)
}

# ParallelLogger::addDefaultFileLogger(file.path(exportFolder, "PioneerTreatmentPathways.txt"))
readr::write_lines(.systemInfo(), here::here(exportFolder, "sessionInfo.txt"))


## Check connection to database

# Confirm that the database connection works and that we have write access to the `cohortDatabaseSchema`.

conn <- connect(connectionDetails)

# check query access
df <- renderTranslateQuerySql(conn, 
                              "select count(*) as n_persons from @cdmDatabaseSchema.person",
                              cdmDatabaseSchema = cdmDatabaseSchema) %>% 
  rename_all(tolower)

message(glue::glue("Number of rows in the person table: {df$n_persons}"))

insertTable(conn, cohortDatabaseSchema, "temp_test", mtcars)

df <- renderTranslateQuerySql(conn, 
                              "select count(*) as n from @cohortDatabaseSchema.temp_test",
                              cohortDatabaseSchema = cohortDatabaseSchema) %>% 
  rename_all(tolower)

renderTranslateExecuteSql(conn, 
                          "drop table @cohortDatabaseSchema.temp_test",
                          cohortDatabaseSchema = cohortDatabaseSchema,
                          progressBar = FALSE)

if (df$n != nrow(mtcars)) {
  rlang::abort("Error with database write access check")
} else {
  rlang::inform("Write access confirmed")
}

disconnect(conn)

## Save database metadata
con <- connect(connectionDetails)

sql <- glue("SELECT vocabulary_version 
             FROM {cdmDatabaseSchema}.vocabulary 
             WHERE vocabulary_id = 'None';")

vocabInfo <- renderTranslateQuerySql(con, sql)

database <- data.frame(databaseId = databaseId,
                       databaseName = databaseName,
                       description = databaseDescription,
                       vocabularyVersion = vocabInfo[[1]])

readr::write_csv(database, here::here(exportFolder, "database.csv"))
disconnect(con)


# Generate Cohorts --------

# There are multiple types of cohorts being used in the study:
  
start <- Sys.time()

cohortTable <- paste0(cohortTable, "_stg")

cohortDefinitionSet <- 
  readr::read_csv(here::here("input", "settings", "CohortsToCreate.csv"), show_col_types = FALSE) %>% 
  mutate(cohortName = name,
         sqlPath = here("input", "sql", paste0(name, ".sql")),
         sql = purrr::map_chr(sqlPath, readr::read_file))

cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable)

CohortGenerator::createCohortTables(
  connectionDetails = connectionDetails,
  cohortDatabaseSchema = cohortDatabaseSchema,
  cohortTableNames = cohortTableNames,
  incremental = TRUE
)

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
  cohortTable = cohortTable) %>%  
  left_join(select(cohortDefinitionSet, cohortId, cohortName = atlasName, group), ., by = "cohortId") %>% 
  mutate(cohortEntries = coalesce(cohortEntries, 0),
         cohortSubjects = coalesce(cohortSubjects, 0),
         databaseId = databaseId)

readr::write_csv(cohortCounts, here::here(exportFolder, paste0(databaseId, "_CohortCounts.csv")))

if (all(filter(cohortCounts, group == "Target") %>% pull(cohortEntries) == 0)) {
  stop("All target cohorts are empty. You cannot execute this study.")
}

delta <- Sys.time() - start
paste("Generating cohorts took", signif(delta, 3), attr(delta, "units"))

## Generate strata cohorts (skip for now)


# Aim 1: Characterization -----

library(FeatureExtraction)

target_ids <- cohortCounts %>% 
  filter(group == "Target") %>% 
  pull(cohortId)


pre_index_covariate_settings <- createCovariateSettings(
  useDemographicsAge = TRUE,
  useDemographicsGender = TRUE,
  useConditionGroupEraLongTerm = TRUE,
  useDrugGroupEraLongTerm = TRUE,
  longTermStartDays = -365,
  endDays = 0
)

post_index_covariate_settings <- createCovariateSettings(
  useConditionGroupEraLongTerm = TRUE,
  longTermStartDays = 0,
  endDays = 365
)

covariates <- 
  getDbCovariateData(connectionDetails,
                     oracleTempSchema = getOption("SqlRenderTempEmulationSchema"),
                     cohortTable = cohortTable,
                     cohortDatabaseSchema = cohortDatabaseSchema,
                     cdmDatabaseSchema = cdmDatabaseSchema,
                     cohortId = target_ids,
                     covariateSettings = list(pre_index_covariate_settings, post_index_covariate_settings),
                     aggregated = TRUE)




# 
# cohortTable
# 
# pre_index_covariates <- 
#   getDbCovariateData(connectionDetails,
#                      oracleTempSchema = getOption("SqlRenderTempEmulationSchema"),
#                      cohortTable = cohortTable,
#                      cohortDatabaseSchema = cohortDatabaseSchema,
#                      cdmDatabaseSchema = cdmDatabaseSchema,
#                      cohortId = target_ids,
#                      covariateSettings = pre_index_covariate_settings,
#                      aggregated = TRUE)



# saveAndromeda()




