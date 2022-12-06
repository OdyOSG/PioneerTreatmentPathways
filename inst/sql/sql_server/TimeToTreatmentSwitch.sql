SELECT cohort_definition_id, 
       ROW_NUMBER() OVER (PARTITION BY cohort_definition_id) AS id,
       DATEDIFF(day, cohort_start_date, event_date) AS time_to_event,
       event
FROM (
      SELECT DISTINCT 
             tp.cohort_definition_id, tp.cohort_start_date, 
             COALESCE(trt.after_first_exposure, coh.cohort_start_date, tp.cohort_end_date) AS event_date,
             CASE WHEN trt.after_first_exposure IS NULL AND 
                       coh.cohort_start_date IS NULL 
                  THEN 0 ELSE 1 END AS event
      FROM @cohort_database_schema.treatment_pat tp
      LEFT JOIN (
          SELECT cohort_definition_id, person_id, 
                 MIN(after_first_exposure) AS after_first_exposure
          FROM @cohort_database_schema.treatment_pat
          WHERE first_exposure is NULL
          GROUP BY cohort_definition_id, person_id
          ) trt
      ON tp.cohort_definition_id = trt.cohort_definition_id
        AND tp.person_id = trt.person_id
      LEFT JOIN (
          SELECT subject_id, cohort_start_date 
          FROM @cohort_database_schema.@cohort_table 
          WHERE cohort_definition_id = @death_cohort_id
          ) coh
      ON tp.person_id = coh.subject_id
     ) tab
ORDER BY cohort_definition_id, id;
