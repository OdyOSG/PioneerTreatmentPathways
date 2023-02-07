SELECT pat.cohort_definition_id AS cohort_id, pat.id, 
       pat.before_codeset_tag, pat.after_codeset_tag
FROM  @cohort_database_schema.treatment_pat pat
LEFT JOIN (
    SELECT cohort_definition_id, person_id, codeset_tag, 
           MAX(drug_exposure_start_date) AS last_exposure
    FROM @cohort_database_schema.treatment_tagged
    GROUP BY cohort_definition_id, person_id, codeset_tag
    ) le
ON pat.cohort_definition_id = le.cohort_definition_id
  AND pat.person_id = le.person_id
  AND pat.before_codeset_tag = le.codeset_tag
LEFT JOIN (
    -- take the earliest date of new drug exposure
    SELECT  cohort_definition_id, person_id, MIN(after_first_exposure) first_switch
    FROM @cohort_database_schema.treatment_pat
    WHERE before_codeset_tag IS NULL
    GROUP BY cohort_definition_id, person_id
    ) fs
ON pat.cohort_definition_id = fs.cohort_definition_id
  AND pat.person_id = fs.person_id
WHERE DATEDIFF(day, pat.cohort_start_date, pat.cohort_end_date) >= 183
  AND first_exposure IS NOT NULL OR 
      DATEADD(day, @second_line_treatment_gap, first_switch) >= after_first_exposure
ORDER BY pat.cohort_definition_id, pat.person_id;
