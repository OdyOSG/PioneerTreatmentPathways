
featureWindowsTempTableSql <- function(connection, featureWindows) {
  sql <- "WITH data AS (
            @unions
          ) 
          SELECT window_id, window_start, window_end, window_type
          INTO #feature_windows
          FROM data;"
  
  unions <- "";
  for(i in 1:nrow(featureWindows)) {
    stmt <- paste0("SELECT ", featureWindows$windowId[i], " window_id, ", 
                   featureWindows$windowStart[i], " window_start, ", 
                   featureWindows$windowEnd[i], " window_end, ", 
                   "'", featureWindows$windowType[i], "' window_type")
    unions <- paste(unions, stmt, sep="\n")
    if (i < nrow(featureWindows)) {
      unions <- paste(unions, "UNION ALL", sep="\n")
    }
  }
  
  sql <- SqlRender::render(sql, unions = unions)
  sql <- SqlRender::translate(sql = sql, targetDialect = attr(connection, "dbms"))
  
  dropSql <- "TRUNCATE TABLE #feature_windows;\nDROP TABLE #feature_windows;\n\n"
  dropSql <- SqlRender::translate(sql = dropSql, targetDialect = attr(connection, "dbms"))
  return(list(create = sql, drop = dropSql))
}


createFeatureProportions <- function(connectionDetails,
                                     cohortDatabaseSchema,
                                     cohortStagingTable,
                                     cohortTable,
                                     featureSummaryTable) {
  
  connection <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection), add = TRUE)
  
  sql <- "
    @feature_time_window_table_create
    
    IF OBJECT_ID('@cohort_database_schema.@feature_summary_table', 'U') IS NOT NULL
    	DROP TABLE @cohort_database_schema.@feature_summary_table;
    
    CREATE TABLE @cohort_database_schema.@feature_summary_table (
      cohort_definition_id BIGINT, 
      feature_cohort_definition_id BIGINT,
      window_id INT, 
    	feature_count BIGINT
    );
    
    /*
    * Evaluate the O's intersecting with T, TwS, TwoS. 
    */
    
    -- Get the sumamry of {T} with {O} in {windows}
    INSERT INTO @cohort_database_schema.@feature_summary_table (
      cohort_definition_id, 
      feature_cohort_definition_id,
      window_id, 
    	feature_count
    )
    SELECT 
      a.cohort_definition_id,
      a.feature_cohort_definition_id,
      a.window_id,
      COUNT(DISTINCT a.subject_id) feature_count
    FROM (
      SELECT DISTINCT
      	ts.cohort_definition_id, 
      	ts.subject_id,
      	o.cohort_definition_id feature_cohort_definition_id, 
      	ts.window_id
      from (
        SELECT * 
        FROM @cohort_database_schema.@cohort_table, #feature_windows
      ) ts 
      inner join (
      	SELECT *
      	FROM @cohort_database_schema.@cohort_staging_table c
      	WHERE c.cohort_definition_id IN (@feature_ids)
      ) o ON o.subject_id = ts.subject_id
      WHERE DATEADD(dd, ts.window_start, ts.cohort_start_date) <=  CASE WHEN ts.window_type = 'start' THEN o.cohort_start_date ELSE o.cohort_end_date END
      AND DATEADD(dd, ts.window_end, ts.cohort_start_date) >= o.cohort_start_date 
    ) a
    GROUP BY
      a.cohort_definition_id,
      a.feature_cohort_definition_id,
      a.window_id
    ;
    
    @feature_time_window_table_drop
  "
  
  featureIds <- readr::read_csv(here::here("inst", "settings","CohortsToCreate.csv"), 
                                show_col_types = FALSE) %>% 
    dplyr::filter(group %in% c("Stratification", "Outcome")) %>% 
    dplyr::pull(.data$cohortId)
  
  featureTimeWindows <- readr::read_csv(here::here("inst", "settings","featureTimeWindows.csv"), 
                                        show_col_types = FALSE)

  featureTimeWindowTempTableSql <- featureWindowsTempTableSql(connection, featureTimeWindows)
  sql <- SqlRender::render(sql,
                           warnOnMissingParameters = TRUE,
                           cohort_database_schema = cohortDatabaseSchema,
                           cohort_staging_table = cohortStagingTable,
                           cohort_table = cohortTable,
                           feature_summary_table = featureSummaryTable,
                           feature_ids = featureIds,
                           feature_time_window_table_create = featureTimeWindowTempTableSql$create,
                           feature_time_window_table_drop = featureTimeWindowTempTableSql$drop) %>% 
    SqlRender::translate(attr(connection, "dbms"))
  
  ParallelLogger::logInfo("Compute feature proportions for all target and strata")
  DatabaseConnector::executeSql(connection, sql)
}

exportFeatureProportions <- function(connectionDetails,
                                     cohortDatabaseSchema,
                                     cohortTable,
                                     featureSummaryTable) {
  
  connection <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection), add = TRUE)
  
  sql <- "
    SELECT 
      c.cohort_definition_id, 
      f.feature_cohort_definition_id,
      f.window_id,
      c.total_count,
      f.feature_count,
      1.0*f.feature_count/c.total_count AS mean,
      sqrt(1.0*(total_count*f.feature_count - f.feature_count*f.feature_count)/(c.total_count*(c.total_count - 1))) AS sd
    FROM (
      SELECT cohort_definition_id, COUNT_BIG(DISTINCT subject_id) total_count
      FROM @cohort_database_schema.@cohort_table 
      GROUP BY cohort_definition_id
    ) c
    INNER JOIN @cohort_database_schema.@feature_summary_table f -- Feature Count
      ON c.cohort_definition_id = f.cohort_definition_id
      AND c.total_count > 1 -- Prevent divide by zero;
  "
  
  sql <- SqlRender::render(sql,
                           warnOnMissingParameters = TRUE,
                           cohort_database_schema = cohortDatabaseSchema,
                           cohort_table = cohortTable,
                           feature_summary_table = featureSummaryTable) %>% 
    SqlRender::translate(attr(connection, "dbms"))
  
  data <- DatabaseConnector::querySql(connection, sql) 
  names(data) <- SqlRender::snakeCaseToCamelCase(names(data))
  
  # formatFeatureProportions ----
  featureTimeWindows <- readr::read_csv(here::here("inst", "settings", "featureTimeWindows.csv"), 
                                        show_col_types = FALSE)
  
  featureCohorts <- readr::read_csv(here::here("inst", "settings","CohortsToCreateStrata.csv"), show_col_types = FALSE) %>% 
    dplyr::filter(group %in% c("Stratification", "Outcome")) %>% 
    dplyr::select(name, cohortId)
  
  data <- merge(data, featureTimeWindows, by = "windowId")
  data <- merge(data, featureCohorts, by.x = "featureCohortDefinitionId", by.y = "cohortId")
  names(data)[names(data) == 'name'] <- 'featureName'
  names(data)[names(data) == 'cohortDefinitionId'] <- 'cohortId'
  
  data$covariateId <- data$featureCohortDefinitionId * 1000 + data$windowId
  
  if (nrow(data) != 0) {
    data$covariateName <- paste0("Cohort during day ", 
                                 data$windowStart, " through ", 
                                 data$windowEnd, " days ", 
                                 data$windowType, " the index: ", 
                                 data$featureName)
    
    data$analysisId <- 10000
    
  } else {
    # Add empty columns
    data <- data.frame(data, matrix(nrow = 0, ncol = 2))
    data <- tibble::tibble(data) %>%
      dplyr::rename(covariateName=X1, analysisId=X2) %>% 
      as.data.frame()
  }
  return(data)
}
