WITH tab as (
SELECT cohort_definition_id, cohort_start_date, min(drug_exposure_start_date) as drug_exposure_date
FROM @cohort_database_schema.@cohort_table coh
JOIN @cdm_database_schema.drug_exposure de
    ON coh.subject_id = de.person_id
        AND de.drug_concept_id IN (
                                  SELECT concept_id
                                  FROM @cohort_database_schema.drug_codesets
                                  )
        AND de.drug_exposure_start_date >= coh.cohort_start_date
        AND de.drug_exposure_start_date <= coh.cohort_end_date
WHERE coh.cohort_definition_id IN (@target_ids)
GROUP BY cohort_definition_id, subject_id, cohort_start_date
ORDER BY cohort_definition_id),
    init_data AS(
                  SELECT cohort_definition_id,
                         DATEDIFF(day, cohort_start_date, drug_exposure_date) AS value
                  FROM tab
                  ),
