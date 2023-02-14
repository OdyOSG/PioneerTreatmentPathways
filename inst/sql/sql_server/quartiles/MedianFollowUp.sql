WITH init_data  AS (
SELECT cohort_definition_id,
       DATEDIFF(day, cohort_start_date, cohort_end_date) AS value
FROM @cohort_database_schema.@cohort_table
WHERE cohort_definition_id IN (@target_ids)
),
