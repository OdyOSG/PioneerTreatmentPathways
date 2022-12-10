#' @importFrom magrittr %>%
generateSurvival <- function(connection,
                             cohortDatabaseSchema, 
                             cohortTable, 
                             targetIds,
                             events,
                             databaseId,
                             packageName) {
  eventIds <- events %>% dplyr::pull(eventId)
  purrr::map_df(eventIds, function(.eventId){
    outcomeIds <- events %>% 
      dplyr::filter(eventId == .eventId) %>% 
      dplyr::pull(outcomeCohortIds)
    sql <- SqlRender::loadRenderTranslateSql(dbms = connection@dbms,
                                             sqlFilename = "TimeToEvent.sql",
                                             packageName = packageName,
                                             warnOnMissingParameters = TRUE,
                                             cohort_database_schema = cohortDatabaseSchema,
                                             cohort_table = cohortTable,
                                             outcome_ids = outcomeIds, 
                                             target_ids = paste(targetIds, collapse = ', '))
    km_grouped <- DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = T)
    
    purrr::map_df(targetIds, function(.targetId){
      
      km <- km_grouped %>% filter(targetId == .targetId)
      
      if (nrow(km) < 100 | length(km$event[km$event == 1]) < 1) {return(NULL)}

      # TODO: Change to Cyclops
      surv_info <- survival::survfit(survival::Surv(timeToEvent, event) ~ 1, data = km)
      surv_info <- survminer::surv_summary(surv_info)
      data.frame(targetId = .targetId, eventId = .eventId, time = surv_info$time, surv = surv_info$surv, 
                 n.censor = surv_info$n.censor, n.event = surv_info$n.event, n.risk = surv_info$n.risk,
                 lower = surv_info$lower, upper = surv_info$upper, databaseId = databaseId)
    })
  })
}
