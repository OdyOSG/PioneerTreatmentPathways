# Install packages. This only needs to be done once.
#install.packages("devtools")
#install.packages("survminer")
#install.packages("here")
#install.packages("glue")
#devtools::install_github("OHDSI/CohortGenerator")
#devtools::install_github("OHDSI/FeatureExtraction")


# Fill in this script with your database connection information

library(DatabaseConnector)
library(dplyr, warn.conflicts = FALSE)
library(here)
library(glue)

# fill in your connection details
connectionDetails <- createConnectionDetails(
  dbms = "postgresql", 
  user = Sys.getenv("LOCAL_POSTGRESQL_USER"), 
  password = Sys.getenv("LOCAL_POSTGRESQL_PASSWORD"), 
  server = "localhost/synpuf100k")

cdmDatabaseSchema <- Sys.getenv("LOCAL_POSTGRESQL_CDM_SCHEMA") 
writeSchema <- Sys.getenv("LOCAL_POSTGRESQL_SCRATCH_SCHEMA")
cohortDatabaseSchema = writeSchema
cohortTable = "pioneer_cohort"
databaseId = "SYNPUF100K"
databaseName = "SYNPUF100K"
databaseDescription = "SYNPUF100K"
options(sqlRenderTempEmulationSchema = writeSchema)
exportFolder = here::here(paste0(tolower(databaseId), "_pioneer_export"))

if (!dir.exists(exportFolder)) dir.create(exportFolder)

conn <- connect(connectionDetails)

# check database access
df <- renderTranslateQuerySql(conn, 
                              "select count(*) as n_persons from @cdmDatabaseSchema.person",
                              cdmDatabaseSchema = cdmDatabaseSchema) %>% 
  rename_all(tolower)

message(glue::glue("Number of rows in the person table: {df$n_persons}"))

insertTable(conn, cohortDatabaseSchema, "cars_temp", cars)

df <- renderTranslateQuerySql(conn, 
                              "select count(*) as n from @cohortDatabaseSchema.cars_temp",
                              cohortDatabaseSchema = cohortDatabaseSchema) %>% 
  rename_all(tolower)

renderTranslateExecuteSql(conn, 
                          "drop table @cohortDatabaseSchema.cars_temp",
                          cohortDatabaseSchema = cohortDatabaseSchema,
                          progressBar = FALSE)

if (df$n != nrow(cars)) {
  rlang::abort("Error with database write access check")
} else {
  message("Write access confirmed")
}

disconnect(conn)
