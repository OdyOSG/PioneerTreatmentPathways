library(dplyr)


## ----install-dependencies, eval=FALSE-----------------------------------------------------------------
## # This only needs to be done once
## # install.packages("devtools")
## # install.packages("survminer")
## # devtools::install_github("OHDSI/CohortDiagnostics")
## # devtools::install_github("OHDSI/CohortGenerator")


## ----set-study-parameters-----------------------------------------------------------------------------
source(here::here("00_study_parameters.R"))

# check that study parameters are available
# these should be set in parameters.R
print(glue::glue('cohortDatabaseSchema = {cohortDatabaseSchema}'))
print(glue::glue('cohortTable = {cohortTable}'))
print(glue::glue('exportFolder = {exportFolder}'))
print(glue::glue('databaseId = {databaseId}'))
print(glue::glue('databaseName = {databaseName}'))
print(glue::glue('databaseDescription = {databaseDescription}'))
print(glue::glue('incremental = {incremental}'))
print(glue::glue('options("sqlRenderTempEmulationSchema") = {options("sqlRenderTempEmulationSchema")"}'))


## ----source-R-files-----------------------------------------------------------------------------------

# Source R code files in this project
purrr::walk(list.files(here("R"), full.names = TRUE), source)

if (!file.exists(exportFolder)) {
  dir.create(exportFolder, recursive = TRUE)
}

readr::write_lines(.systemInfo(), here::here(exportFolder, "sessionInfo.txt"))
cat(.systemInfo())


## ----save_database_info-------------------------------------------------------------------------------

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




## ----generate_cohorts---------------------------------------------------------------------------------
cohortDefinitionSet <- readr::read_csv(here("input", "settings", "CohortsToCreate.csv"), 
                                       show_col_types = FALSE) %>% 
  mutate(cohortName = name,
         sqlPath = file.path("input", "sql", "sql_server", paste0(name, ".sql")),
         sql = purrr::map_chr(sqlPath, readr::read_file))

start <- Sys.time()
cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable)

CohortGenerator::createCohortTables(
    connection = con,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTableNames = cohortTableNames,
    incremental = incremental)


CohortGenerator::generateCohortSet(
  connection = con,
  cdmDatabaseSchema = cdmDatabaseSchema,
  cohortDatabaseSchema = cohortDatabaseSchema,
  cohortDefinitionSet = cohortDefinitionSet,
  cohortTableNames = cohortTableNames,
  incremental = incremental,
  incrementalFolder = here(exportFolder, "incremental")
)

delta <- Sys.time() - start
cat(paste("Generating cohorts took", signif(delta, 3), attr(delta, "units")))


## ----check_cohort_generation--------------------------------------------------------------------------
n <- renderTranslateQuerySql(con, glue::glue("select count(*) as n from {cohortDatabaseSchema}.{cohortTable}")) %>%
  rename_all(tolower) %>% 
  pull(n)

message(glue("cohort table created with {n} rows."))


## ----get_cohort_counts--------------------------------------------------------------------------------

cohortCounts <- CohortGenerator::getCohortCounts(
    connection = con,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTable = cohortTable) %>%
  rename_all(tolower) %>% 
  tibble() %>%
  full_join(select(cohortDefinitionSet, cohortId, name = atlasName, group), by = c("cohortid" = "cohortId")) %>%
  mutate(across(c(cohortentries, cohortsubjects), ~tidyr::replace_na(., 0))) %>% 
  mutate(databaseId = databaseId) %>%
  arrange(cohortid)

readr::write_csv(cohortCounts, here(exportFolder, paste0(databaseId, "CohortCounts.csv")))

print(cohortCounts, n=100)

if (all(filter(cohortCounts, group == "Target") %>% pull(cohortEntries) == 0)) {
  stop("All target cohorts are empty. You cannot execute this study.")
}


## ----features_one_year_prior_to_index-----------------------------------------------------------------
library(FeatureExtraction)

target_ids <- c(1:12, 40)

preIndexCovariateSettings <- createCovariateSettings(
  useDemographicsAge = TRUE,
  useDemographicsGender = TRUE,
  useConditionGroupEraLongTerm = TRUE,
  useDrugGroupEraLongTerm = TRUE,
  longTermStartDays = -365,
  endDays = 0
)

covariates_minus365_minus1 <- 
  getDbCovariateData(connection = con,
                     oracleTempSchema = getOption("SqlRenderTempEmulationSchema"),
                     cohortTable = cohortTable,
                     cdmDatabaseSchema = cdmDatabaseSchema,
                     cohortDatabaseSchema = cohortDatabaseSchema,
                     cohortId = target_ids,
                     covariateSettings = preIndexCovariateSettings,
                     aggregated = TRUE)

Andromeda::saveAndromeda(covariates_minus365_minus1, here::here(exportFolder, "covariates_minus365_minus1"))


## ----features_one_year_post_index---------------------------------------------------------------------
postIndexCovariateSettings_0_365 <- createCovariateSettings(
  useConditionGroupEraLongTerm = TRUE,
  useDrugGroupEraLongTerm = TRUE,
  longTermStartDays = 0,
  endDays = 365
)

postIndexCovariates <- 
  getDbCovariateData(connection = con,
                     oracleTempSchema = getOption("SqlRenderTempEmulationSchema"),
                     cohortTable = cohortTable,
                     cdmDatabaseSchema = cdmDatabaseSchema,
                     cohortDatabaseSchema = cohortDatabaseSchema,
                     cohortId = target_ids,
                     covariateSettings = postIndexCovariateSettings_0_365,
                     aggregated = TRUE)

Andromeda::saveAndromeda(postIndexCovariates, here::here(exportFolder, "covariates_0_365"))


## ----features_year2_post_index------------------------------------------------------------------------

postIndexCovariateSettings_366_710 <- createCovariateSettings(
  useConditionGroupEraLongTerm = TRUE,
  useDrugGroupEraLongTerm = TRUE,
  longTermStartDays = 366,
  endDays = 710
)

covariates_366_710 <- 
  getDbCovariateData(connection = con,
                     oracleTempSchema = getOption("SqlRenderTempEmulationSchema"),
                     cohortTable = cohortTable,
                     cdmDatabaseSchema = cdmDatabaseSchema,
                     cohortDatabaseSchema = cohortDatabaseSchema,
                     cohortId = target_ids,
                     covariateSettings = postIndexCovariateSettings_366_710,
                     aggregated = TRUE)

Andromeda::saveAndromeda(covariates_366_710, here::here(exportFolder, "covariates_366_710"))


## ----extract_cohort_table-----------------------------------------------------------------------------

sql <- glue("
  select * 
  from {cohortDatabaseSchema}.{cohortTable}
  where subject_id in (
    select distinct subject_id 
    from {cohortDatabaseSchema}.{cohortTable}
    where cohort_definition_id = 1
  )") %>% 
  SqlRender::translate(dbms(con))

cohort <- Andromeda::andromeda()
DatabaseConnector::querySqlToAndromeda(con, sql, cohort, "cohort")
cohort$cohort <- dplyr::rename_all(cohort$cohort, tolower)

Andromeda::saveAndromeda(cohort, here::here("temp", "cohort"))

cohort <- Andromeda::loadAndromeda(here::here("temp", "cohort"))

print(paste(collect(tally(cohort$cohort))$n, "rows in the cohort table")) # old Andromeda
print(paste(nrow(cohort$cohort), "rows in the cohort table")) # new Andromeda

Andromeda::close(cohort)


## ----get_charlson_scores_for_strata-------------------------------------------------------------------

# Compute charlson group for each person in the target cohort an upload it to the database
library(FeatureExtraction)

charlson <- getDbCovariateData(
  connection = con,
  oracleTempSchema = getOption("SqlRenderTempEmulationSchema"),
  cohortTable = cohortTable,
  cdmDatabaseSchema = cdmDatabaseSchema,
  cohortDatabaseSchema = cohortDatabaseSchema,
  cohortId = 1,
  covariateSettings = createCovariateSettings(useCharlsonIndex = TRUE, longTermStartDays = -365, endDays = -1),
  aggregated = FALSE)

charlson_df <- charlson$covariates %>% 
  mutate(charlson_group = case_when(
         covariateValue == 0 ~ 'CCI=0',
         covariateValue == 1 ~ 'CCI=0',
         covariateValue >= 2 ~ 'CCI>=2')) %>%
  select(subject_id = rowId, charlson_group) %>%
  collect() %>%
  mutate(subject_id = as.integer(subject_id))

insertTable(con, 
            databaseSchema = cohortDatabaseSchema,
            tableName = "charlson_strata",
            data = charlson_df,
            bulkLoad = FALSE,
            dropTableIfExists = TRUE,
            progressBar = TRUE)


## ----create_stratified_cohort_table-------------------------------------------------------------------
# create a new cohort target cohort table with additional stratafication columns

renderTranslateExecuteSql(con, glue("
DROP TABLE IF EXISTS {cohortDatabaseSchema}.pioneer_target_strata;

CREATE TABLE {cohortDatabaseSchema}.pioneer_target_strata AS
select distinct
    a.cohort_definition_id,
    a.subject_id,
    a.cohort_start_date,
    a.cohort_end_date,
    YEAR(a.cohort_start_date) as index_year,
    CASE 
        WHEN YEAR(a.cohort_start_date) - p.year_of_birth < 60 THEN '<60'
        WHEN YEAR(a.cohort_start_date) - p.year_of_birth >= 60 
            AND YEAR(a.cohort_start_date) - p.year_of_birth < 70 THEN '60-69'
        WHEN YEAR(a.cohort_start_date) - p.year_of_birth >= 70 
            AND YEAR(a.cohort_start_date) - p.year_of_birth < 80 THEN '70-79'
        WHEN YEAR(a.cohort_start_date) - p.year_of_birth >= 80 THEN '>80'
    END AS age_group,
    CASE WHEN b.cohort_definition_id = 23 AND b.cohort_start_date <= a.cohort_start_date THEN 1 ELSE 0 END AS obesity,
    CASE WHEN b.cohort_definition_id = 24 AND b.cohort_start_date <= a.cohort_start_date THEN 1 ELSE 0 END AS hypertension,
    CASE WHEN b.cohort_definition_id = 25 AND b.cohort_start_date <= a.cohort_start_date THEN 1 ELSE 0 END AS cve,
    CASE WHEN b.cohort_definition_id = 26 AND b.cohort_start_date <= a.cohort_start_date THEN 1 ELSE 0 END AS t2dm,
    CASE WHEN b.cohort_definition_id = 27 AND b.cohort_start_date <= a.cohort_start_date THEN 1 ELSE 0 END AS vte,
    CASE WHEN b.cohort_definition_id = 28 AND b.cohort_start_date <= a.cohort_start_date THEN 1 ELSE 0 END AS copd,
    COALESCE(c.charlson_group, 'CCI=0') AS charlson_group,
    o.observation_period_start_date,
    o.observation_period_end_date
from {cohortDatabaseSchema}.{cohortTable} a
join {cohortDatabaseSchema}.{cohortTable} b on a.subject_id = b.subject_id
join {cdmDatabaseSchema}.person p on a.subject_id = p.person_id
join {cdmDatabaseSchema}.observation_period o 
  ON a.subject_id = o.person_id 
  AND o.observation_period_start_date <= a.cohort_start_date
  AND a.cohort_start_date <= o.observation_period_end_date
left join {cohortDatabaseSchema}.charlson_strata c on a.subject_id = c.subject_id
where a.cohort_definition_id = 1;
"))


## -----------------------------------------------------------------------------------------------------
target_strata <- renderTranslateQuerySql(con, glue("select * from {cohortDatabaseSchema}.pioneer_target_strata"))
readr::write_rds(target_strata, here::here("temp", "target_strata.rds"))


## ----get_strata_levels_in_data------------------------------------------------------------------------
# Get all levels of all strata that exist in the data
strata_columns <- c("index_year", "age_group", "obesity", "hypertension", "cve", "t2dm", "copd", "charlson_group")

strata_levels <- purrr::map(strata_columns, ~renderTranslateQuerySql(con, 
  glue("select distinct {.} as x from {cohortDatabaseSchema}.pioneer_target_strata;"))[[1]])
names(strata_levels) <- strata_columns
strata_level_names <- purrr::map(strata_levels, ~stringr::str_remove_all(., "[:symbol:]"))


## ----extract_strata_features--------------------------------------------------------------------------
# Run feature extraction for each strata level
start_days = c(-365, 0, 366)
end_days = c(-1, 365, 710)

if (!dir.exists(here::here(exportFolder, "strata_covariates"))) {
  dir.create(here::here(exportFolder, "strata_covariates"))
}

for (i in seq_along(strata_levels)) {   
    for (j in seq_along(strata_levels[[i]])) {
        
        # add string quotes
        lv <- ifelse(is.character(strata_levels[[i]][j]), glue("'{strata_levels[[i]][j]}'"), strata_levels[[i]][j])
        # create a subset cohort table
        renderTranslateExecuteSql(con, glue("
            drop table if exists {cohortDatabaseSchema}.strata_temp_cohort;
            create table {cohortDatabaseSchema}.strata_temp_cohort as
            select * from {cohortDatabaseSchema}.pioneer_target_strata
            where {names(strata_levels)[i]} = {lv}"))
        
        cnt <- renderTranslateQuerySql(con, glue("select count(distinct subject_id) as n from {cohortDatabaseSchema}.strata_temp_cohort"))[[1]]
        
        if (cnt < 10) {
          print(glue("skipping FeatureExtraction for {names(strata_levels)[i]} level {strata_levels[[i]][j]} with {cnt} persons"))
          next
        }
        
        print(glue("running FeatureExtraction for {names(strata_levels)[i]} level {strata_levels[[i]][j]} with {cnt} persons"))

        for (k in 1:3) {
            # run FE for all three time windows 
            print(glue("Feature time window {start_days[k]} - {end_days[k]}"))
            FESettings <- createCovariateSettings(
              useDemographicsAge = TRUE,
              useDemographicsGender = TRUE,
              useConditionGroupEraLongTerm = TRUE,
              useDrugGroupEraLongTerm = TRUE,
              longTermStartDays = start_days[k],
              endDays = end_days[k])

            covariates <- 
                getDbCovariateData(
                     connection = con,
                     oracleTempSchema = getOption("SqlRenderTempEmulationSchema"),
                     cohortTable = "strata_temp_cohort",
                     cdmDatabaseSchema = cdmDatabaseSchema,
                     cohortDatabaseSchema = cohortDatabaseSchema,
                     covariateSettings = FESettings,
                     aggregated = TRUE)

            nm <- glue("strata_covariates_{start_days[k]}_{end_days[k]}_{names(strata_levels)[i]}_{strata_level_names[[i]][j]}") 
            Andromeda::saveAndromeda(covariates, here::here(exportFolder, "strata_covariates", nm))
        }
    }          
}

renderTranslateExecuteSql(con, glue("drop table if exists {cohortDatabaseSchema}.strata_temp_cohort;"))


## ----disconnect---------------------------------------------------------------------------------------
disconnect(con)

