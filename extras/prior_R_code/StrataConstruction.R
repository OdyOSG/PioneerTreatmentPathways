createBulkStrata <- function(connection,
                             cdmDatabaseSchema,
                             cohortDatabaseSchema,
                             cohortStagingTable,
                             targetIds, 
                             oracleTempSchema) {
  
  # Create the bulk strata from the CSV
  createBulkStrataFromFile(connection,
                           cdmDatabaseSchema,
                           cohortDatabaseSchema,
                           cohortStagingTable,
                           targetIds, 
                           oracleTempSchema)
  
  # Create the bulk strata from the cohorts of interest
  createBulkStrataFromCohorts(connection,
                               cohortDatabaseSchema,
                               cohortStagingTable,
                               targetIds, 
                               oracleTempSchema)
  
}

createBulkStrataFromFile <- function(connection,
                                     cdmDatabaseSchema,
                                     cohortDatabaseSchema,
                                     cohortStagingTable,
                                     targetIds, 
                                     oracleTempSchema) {
  packageName <- getThisPackageName()
  bulkStrataToCreate <- getBulkStrata()
  targetStrataXref <- getTargetStrataXref()
  
  for (i in 1:nrow(bulkStrataToCreate)) {
    .strataId <- bulkStrataToCreate$cohortId[i]
    # Get the strata to create for the targets selected
    tsXrefSubset <- targetStrataXref[targetStrataXref$targetId %in% targetIds & targetStrataXref$strataId == .strataId, ]
    # Create the SQL for the temp table to hold the cohorts to be stratified
    tsXrefTempTableSql <- cohortStrataXrefTempTableSql(connection, tsXrefSubset, oracleTempSchema)
    # Execute the SQL to create the stratified cohorts
    ParallelLogger::logInfo(paste0("Stratify by ", bulkStrataToCreate$name[i]))
    sql <- SqlRender::loadRenderTranslateSql(dbms = attr(connection, "dbms"),
                                             sqlFilename = bulkStrataToCreate$generationScript[i], 
                                             packageName = packageName,
                                             warnOnMissingParameters = FALSE,
                                             oracleTempSchema = oracleTempSchema,
                                             cdm_database_schema = cdmDatabaseSchema,
                                             cohort_database_schema = cohortDatabaseSchema,
                                             cohort_staging_table = cohortStagingTable,
                                             lb_operator = bulkStrataToCreate$lbOperator[i],
                                             lb_strata_value = bulkStrataToCreate$lbStrataValue[i],
                                             ub_operator = bulkStrataToCreate$ubOperator[i],
                                             ub_strata_value = bulkStrataToCreate$ubStrataValue[i],
                                             target_strata_xref_table_create = tsXrefTempTableSql$create,
                                             target_strata_xref_table_drop = tsXrefTempTableSql$drop)
    DatabaseConnector::executeSql(connection, sql)
    #write(sql,paste0(i, ".sql"))
  }
}

createBulkStrataFromCohorts <- function(connection,
                                        cohortDatabaseSchema,
                                        cohortStagingTable,
                                        targetIds, 
                                        oracleTempSchema) {
  packageName <- getThisPackageName()
  strataCohorts <- getCohortBasedStrata()
  targetStrataXref <- getTargetStrataXref()
  
  # Get the strata to create for the targets selected
  tsXrefSubset <- targetStrataXref[targetStrataXref$targetId %in% targetIds & targetStrataXref$strataId %in% strataCohorts$cohortId, ]
  # Create the SQL for the temp table to hold the cohorts to be stratified
  tsXrefTempTableSql <- cohortStrataXrefTempTableSql(connection, tsXrefSubset, oracleTempSchema)
  
  
  sql <- SqlRender::loadRenderTranslateSql(dbms = attr(connection, "dbms"),
                                           sqlFilename = "strata/StratifyByCohort.sql",
                                           packageName = packageName,
                                           oracleTempSchema = oracleTempSchema,
                                           warnOnMissingParameters = TRUE,
                                           cohort_database_schema = cohortDatabaseSchema,
                                           cohort_staging_table = cohortStagingTable,
                                           target_strata_xref_table_create = tsXrefTempTableSql$create,
                                           target_strata_xref_table_drop = tsXrefTempTableSql$drop)
  
  ParallelLogger::logInfo("Stratify by cohorts")
  DatabaseConnector::executeSql(connection, sql)
}



serializeBulkStrataName <- function(bulkStrataToCreate) {
  return(paste(bulkStrataToCreate$generationScript, bulkStrataToCreate$name, bulkStrataToCreate$parameterValue, sep = "|"))
}

