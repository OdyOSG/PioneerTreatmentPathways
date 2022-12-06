-- assign drug tag to each drug_exposure instead of individual drug concept_ids
DROP TABLE IF EXISTS @cohort_database_schema.treatment_tagged;
CREATE TABLE @cohort_database_schema.treatment_tagged AS
SELECT coh.cohort_definition_id, de.person_id, 
       cs.codeset_tag, de.drug_exposure_start_date,
       coh.cohort_start_date, coh.cohort_end_date
FROM @cdm_database_schema.drug_exposure de 
JOIN @cohort_database_schema.@cohort_table coh
    ON de.person_id = coh.subject_id
JOIN @cohort_database_schema.codesets cs
    ON cs.concept_id = de.drug_concept_id
WHERE coh.cohort_definition_id IN (@treatment_cohort_ids)
  AND cohort_end_date >= de.drug_exposure_start_date
  AND cohort_start_date <= de.drug_exposure_start_date
ORDER BY cohort_definition_id, de.person_id, de.drug_exposure_start_date;


-- collect initial treatment patterns info
DROP TABLE IF EXISTS @cohort_database_schema.treatment_pat;
CREATE TABLE @cohort_database_schema.treatment_pat AS
SELECT DENSE_RANK() OVER(PARTITION BY cohort_definition_id ORDER BY person_id) AS id,
       cohort_definition_id, person_id, before_codeset_tag, first_exposure, 
       after_codeset_tag, after_first_exposure, cohort_start_date, cohort_end_date
FROM (
      SELECT COALESCE(before_cohort_id, after_cohort_id) AS cohort_definition_id, 
             COALESCE(before_person_id, after_person_id) AS person_id, before_codeset_tag, first_exposure,
             after_codeset_tag, COALESCE(after_first_exposure, first_exposure) AS after_first_exposure, 
             COALESCE(before.cohort_start_date, after.cohort_start_date) AS cohort_start_date,
             COALESCE(before.cohort_end_date, after.cohort_end_date) AS cohort_end_date
      FROM (
            -- first six months treatment
            SELECT cohort_definition_id AS before_cohort_id, person_id AS before_person_id, codeset_tag AS before_codeset_tag,
                   MIN(drug_exposure_start_date) AS first_exposure, cohort_start_date, cohort_end_date
            FROM scratch_artem.treatment_tagged tp
            WHERE drug_exposure_start_date <= DATEADD(month, 6, cohort_start_date)
            GROUP BY cohort_definition_id, person_id, codeset_tag, cohort_start_date, cohort_end_date
            ) before
      FULL JOIN (
            -- post six months treatment
            SELECT cohort_definition_id AS after_cohort_id, person_id AS after_person_id, codeset_tag AS after_codeset_tag,
                   MIN(drug_exposure_start_date) AS after_first_exposure, cohort_start_date, cohort_end_date
            FROM scratch_artem.treatment_tagged tp
            WHERE drug_exposure_start_date > DATEADD(month, 6, cohort_start_date)
            GROUP BY cohort_definition_id, person_id, codeset_tag, cohort_start_date, cohort_end_date
            ) after
      ON before.before_person_id = after.after_person_id
        AND before.before_codeset_tag = after.after_codeset_tag
        AND before_cohort_id = after_cohort_id
      ORDER BY after.after_person_id, after_first_exposure
      ) tab
ORDER BY cohort_definition_id;
