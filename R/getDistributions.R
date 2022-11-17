getAtEventDistribution <- function(connection, 
                                   cohortDatabaseSchema, 
                                   cdmDatabaseSchema, 
                                   cohortTable,
                                   targetIds, 
                                   outcomeId, 
                                   databaseId, 
                                   analysisName) {
  sqlFileName <- if (length(outcomeId) == 0) paste(analysisName, 'sql', sep = '.')  else paste('TimeToOutcome', 'sql', sep = '.')
  
  sql <- SqlRender::readSql(system.file("sql", "sql_server", "quartiles", sqlFileName, package = getThisPackageName()))
  sqlAggreg <- SqlRender::readSql(system.file("sql", "sql_server", "quartiles", 'QuartilesAggregation.sql', package = getThisPackageName()))
  sql <- paste0(sql, sqlAggreg)
  sql <- SqlRender::render(sql, 
                           cohort_database_schema = cohortDatabaseSchema, 
                           cdm_database_schema = cdmDatabaseSchema,
                           cohort_table = cohortTable,
                           target_ids = paste(targetIds, collapse = ', '), 
                           analysis_name = substring(SqlRender::camelCaseToTitleCase(analysisName), 2), 
                           outcome_id = outcomeId, 
                           warnOnMissingParameters = FALSE)
  sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
  data <- DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = T)
  
  if (nrow(data) == 0) {
    ParallelLogger::logWarn("There is NO data for atEventDistribution")
    df <- data.frame(matrix(nrow = 0, ncol = 9))
    colnames(df) <- c("cohortDefinitionId", "iqr", "minimum", "q1", "median", "q3", "maximum", "analysisName", "database_id")
    return(df)
  }

  data.frame(cohortDefinitionId = data$cohortDefinitionId, 
             iqr = data$iqr,
             minimum = data$minimum, 
             q1 = data$q1, 
             median = data$median,
             q3 = data$q3, 
             maximum = data$maximum, 
             analysisName = data$analysisName, 
             database_id = databaseId)
}
