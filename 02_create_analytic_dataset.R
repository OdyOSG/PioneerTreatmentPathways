library(dplyr)

# create a de-identified analytic dataset

target <- readr::read_rds(here::here("temp", "target_strata.rds")) %>% 
  rename_all(tolower) %>% 
  distinct() %>% 
  tibble() %>% 
  arrange(subject_id) 

id_crosswalk <- target %>% 
  distinct(subject_id) %>% 
  mutate(person_id = row_number())

target <- target %>% 
  inner_join(id_crosswalk, by = "subject_id") %>% 
  group_by(cohort_definition_id, subject_id, cohort_start_date, 
           cohort_end_date, index_year, age_group, charlson_group, 
           observation_period_start_date, 
           observation_period_end_date, person_id) %>% 
  summarise_all(max) %>% 
  ungroup()

n <- target %>% 
  add_count(subject_id) %>% 
  filter(n>1) %>% 
  nrow()

# check that people are in the target cohort only once
if (n != 0) {
  warning("Some people are in the main target cohort more than once!")
}
if (n_distinct(target$subject_id) != nrow(target)) {
  warning("Some people are in the main target cohort more than once!")
}

# convert dates to days relative to index
target2 <- target %>% 
  mutate(cohort_end_date = as.numeric(cohort_end_date - cohort_start_date),
         observation_period_start_date = as.numeric(observation_period_start_date - cohort_start_date),
         observation_period_end_date = as.numeric(observation_period_end_date - cohort_start_date)) %>% 
  rename(target_cohort_end_day = cohort_end_date,
         observation_period_start = observation_period_start_date,
         observation_period_end = observation_period_end_date)

index_dates <- target2 %>% 
  select(subject_id, person_id, index_date = cohort_start_date) %>% 
  distinct()

if (n_distinct(index_dates$person_id) != nrow(index_dates)) {
  warning("Some people have more than one index date!")
}

target3 <- target2 %>% 
  select(-cohort_start_date)

readr::write_csv(target3, here::here(exportFolder, "target.csv"))

cohort <- Andromeda::loadAndromeda(here::here("temp", "cohort")) 
cohort$index <- index_dates
cohort_export <- Andromeda::andromeda()

cohort_export$cohort <- cohort$cohort %>% 
  left_join(cohort$index, by = "subject_id") %>% 
  mutate(start_day = cohort_start_date - index_date,
         end_day = cohort_end_date - index_date) %>% 
  select(cohort_definition_id, person_id, start_day, end_day) 

# try a few file formats export formats
Andromeda::saveAndromeda(cohort_export, here::here(exportFolder, "cohort.andr"))

a <- Andromeda::loadAndromeda(here::here(exportFolder, "cohort.andr")) 

ch <- collect(a$cohort)
Andromeda::close(a)

readr::write_csv(ch, here::here(exportFolder, "cohort.csv"))
if (require("arrow")) {
  arrow::write_parquet(ch, here::here(exportFolder, "cohort.parquet"))
}

cli::cat_rule("Done!")
