DROP TABLE IF EXISTS @cohort_database_schema.drug_codesets;
CREATE TABLE @cohort_database_schema.drug_codesets
(
    codeset_tag VARCHAR NOT NULL,
    concept_id BIGINT NOT NULL
);


INSERT INTO @cohort_database_schema.drug_codesets (codeset_tag, concept_id)
SELECT codeset_tag, c.concept_id
FROM (
     --ADT
     SELECT 'ADT' AS codeset_tag, c.concept_id
     FROM (
          SELECT DISTINCT I.concept_id
          FROM (
               SELECT concept_id
               FROM @vocabulary_database_schema.CONCEPT
               WHERE concept_id IN (19058410, 739471, 35834903, 1343039, 19089810, 1366310, 1351541, 1344381, 19010792,
                                    1356461, 1500211, 1300978, 1315286, 35807385, 35807349)
               UNION
               SELECT C.concept_id
               FROM @vocabulary_database_schema.CONCEPT C
                   JOIN @vocabulary_database_schema.CONCEPT_ANCESTOR ca
               ON C.concept_id = ca.descendant_concept_id
                   AND ca.ancestor_concept_id IN (19058410, 739471, 35834903, 1343039, 19089810, 1366310, 1351541, 1344381,
                                                  19010792, 1356461, 1500211, 1300978, 1315286, 35807385, 35807349)
                   AND C.invalid_reason IS NULL
               ) I
          ) C

     UNION
     
     -- ARTA
     SELECT 'ARTA' AS codeset_tag, c.concept_id
     FROM (
          SELECT DISTINCT I.concept_id
          FROM (
               SELECT concept_id
               FROM @vocabulary_database_schema.CONCEPT
               WHERE concept_id IN (40239056, 963987, 42900250, 1361291)
               UNION
               SELECT C.concept_id
               FROM @vocabulary_database_schema.CONCEPT C
                   JOIN @vocabulary_database_schema.CONCEPT_ANCESTOR ca
               ON C.concept_id = ca.descendant_concept_id
                   AND ca.ancestor_concept_id IN (40239056, 963987, 42900250, 1361291)
                   AND C.invalid_reason IS NULL
               ) I
          ) C

     UNION
     
     --Chemo
     SELECT 'Chemo' AS codeset_tag, c.concept_id
     FROM (
          SELECT DISTINCT I.concept_id
          FROM (
               SELECT concept_id
               FROM @vocabulary_database_schema.CONCEPT
               WHERE concept_id IN (1315942, 40222431, 1378382)
               UNION
               SELECT C.concept_id
               FROM @vocabulary_database_schema.CONCEPT C
                   JOIN @vocabulary_database_schema.CONCEPT_ANCESTOR ca
               ON C.concept_id = ca.descendant_concept_id
                   AND ca.ancestor_concept_id IN (1315942, 40222431, 1378382)
                   AND C.invalid_reason IS NULL
               ) I
          ) C

     UNION
     
     --PARP
     SELECT 'PARP' AS codeset_tag, c.concept_id
     FROM (
          SELECT DISTINCT I.concept_id
          FROM (
               SELECT concept_id
               FROM @vocabulary_database_schema.CONCEPT
               WHERE concept_id IN (45892579, 1718850)
               UNION
               SELECT C.concept_id
               FROM @vocabulary_database_schema.CONCEPT C
                   JOIN @vocabulary_database_schema.CONCEPT_ANCESTOR ca
               ON C.concept_id = ca.descendant_concept_id
                   AND ca.ancestor_concept_id IN (45892579, 1718850)
                   AND C.invalid_reason IS NULL
               ) I
          ) C

     UNION
     
     --Immunotherapy
     SELECT 'Immuno' AS codeset_tag, c.concept_id
     FROM (
          SELECT DISTINCT I.concept_id
          FROM (
               SELECT concept_id
               FROM @vocabulary_database_schema.CONCEPT
               WHERE concept_id IN (45775965, 40224095)
               UNION
               SELECT C.concept_id
               FROM @vocabulary_database_schema.CONCEPT C
                   JOIN @vocabulary_database_schema.CONCEPT_ANCESTOR ca
               ON C.concept_id = ca.descendant_concept_id
                   AND ca.ancestor_concept_id IN (45775965, 40224095)
                   AND C.invalid_reason IS NULL
               ) I
          ) C

     UNION
     
     --lutetium
     SELECT 'lutetium' AS codeset_tag, c.concept_id
     FROM (
          SELECT DISTINCT I.concept_id
          FROM (
               SELECT concept_id
               FROM @vocabulary_database_schema.CONCEPT
               WHERE concept_id IN (44816340)
               UNION
               SELECT C.concept_id
               FROM @vocabulary_database_schema.CONCEPT C
                   JOIN @vocabulary_database_schema.CONCEPT_ANCESTOR ca
               ON C.concept_id = ca.descendant_concept_id
                   AND ca.ancestor_concept_id IN (44816340)
                   AND C.invalid_reason IS NULL
               ) I
          ) C

     UNION
     
     --radium223
     SELECT 'radium' AS codeset_tag, c.concept_id
     FROM (
          SELECT DISTINCT I.concept_id
          FROM (
               SELECT concept_id
               FROM @vocabulary_database_schema.CONCEPT
               WHERE concept_id IN (45775578, 902727, 43526934)
               UNION
               SELECT C.concept_id
               FROM @vocabulary_database_schema.CONCEPT C
                   JOIN @vocabulary_database_schema.CONCEPT_ANCESTOR ca
               ON C.concept_id = ca.descendant_concept_id
                   AND ca.ancestor_concept_id IN (45775578, 902727, 43526934)
                   AND C.invalid_reason IS NULL
               ) I
          LEFT JOIN
              (
              SELECT concept_id
              FROM @vocabulary_database_schema.CONCEPT
              WHERE concept_id IN (41267222)

              ) E
              ON I.concept_id = E.concept_id
          WHERE E.concept_id IS NULL
          ) C
     ) tab
;
