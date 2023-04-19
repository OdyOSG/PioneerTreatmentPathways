





cohort

df <- df1 %>% 
  filter(group != "Stratification") %>% 
  select(-cohort_end_date) %>% 
  inner_join(index_dates, by = "subject_id") %>% # remove subjects who don't have an index date
  mutate(day = as.integer(cohort_start_date - index_date)) %>% 
  mutate(index_year = lubridate::year(index_date)) %>% 
  select(-cohort_start_date, -index_date)

df


# Summarize baseline characteristics for every strata ----

# saveCovariatesAsCsv <- function(covariateName, outputFolder) {
#   cov1 <- Andromeda::loadAndromeda(here::here(covariateName))
#   if (!dir.exists(here::here(outputFolder, covariateName))) {
#     dir.create(here::here(outputFolder, covariateName))
#     dir.create(here::here(outputFolder, covariateName))
#   }
#   purrr::walk(names(cov1), ~readr::write_csv(collect(cov1[[.]]), 
#                                              here::here(outputFolder, covariateName, paste0(., ".csv"))))
#   Andromeda::close(cov1)
# }
# 
# saveCovariatesAsCsv("covariates_minus365_minus1", outputFolder = outputFolder)
# saveCovariatesAsCsv("covariates_0_365", outputFolder = outputFolder)
# saveCovariatesAsCsv("covariates_366_710", outputFolder = outputFolder)


# Time to treatment initiation -------------
first_treatment <- df %>% 
  filter(cohort_definition_id %in% 2:5) %>% 
  mutate(name = stringr::str_remove(name, "Newly diagnosed prostate cancer initiated ")) %>% 
  select(-group) %>% 
  group_by(subject_id) %>% 
  arrange(subject_id, day) %>% 
  slice_head(n=1) %>% 
  ungroup()

# check that we have one row per person
check <- first_treatment %>% 
  count(subject_id, name = "n") %>% 
  count(n, name = "nn") %>% 
  pull(n)

stopifnot(length(check) == 1, check == 1)

index_dates_without_treatment <- index_dates %>% 
  anti_join(first_treatment, by = "subject_id") 


time_to_first_trt <- first_treatment %>% 
  mutate(event = 1) %>% 
  select(timeToEvent = day, event, cohort_definition_id, treatment_group = name) %>% 
  bind_rows(transmute(index_dates_without_treatment, timeToEvent = followup_days, event = 0)) 

if (nrow(index_dates) != nrow(time_to_first_trt)) {
  warning("check on time to first treatment failed")
}

# Median time to first treatment
fit1 <- survival::survfit(survival::Surv(timeToEvent, event) ~ treatment_group, data = time_to_first_trt)
median_grouped <- survminer::surv_median(fit1)

fit2 <- survival::survfit(survival::Surv(timeToEvent, event) ~ 1, data = km)
median_overall <- survminer::surv_median(fit2)

bind_rows(median_overall, median_grouped) %>% 
  readr::write_csv(here::here(outputFolder, "timeToFirstTreatment.csv"))


# Time to next treatment -----

# Time from the start of first treatment to 

initial_treatment <- df1 %>% 
  filter(cohort_definition_id %in% 2:4) %>% 
  select(cohort_definition_id, subject_id, cohort_start_date, name) %>% 
  group_by(subject_id) %>% 
  arrange(subject_id, cohort_start_date) %>% 
  slice_head(n = 1) %>% 
  mutate(name = stringr::str_remove(name, "Newly diagnosed prostate cancer initiated ")) 



second_treatment <- df1 %>% 
  filter(cohort_definition_id %in% c(13:19, 22)) %>% 
  select(subject_id,
         outcome_id = cohort_definition_id,
         outcome_name = name,
         outcome_date = cohort_start_date) %>% 
  left_join(initial_treatment, by = "subject_id") %>% 
  filter(outcome_date > cohort_start_date) %>% 
  select(subject_id, outcome_id, outcome_name, outcome_date) %>% 
  group_by(subject_id) %>% 
  arrange(subject_id, outcome_date) %>% 
  slice_head(n=1) %>%
  ungroup()

km_ttnt <- initial_treatment %>% 
  left_join(second_treatment, by = "subject_id") %>% 
  left_join(index_dates, by = "subject_id") %>% 
  mutate(time_to_event = coalesce(outcome_date, end_of_followup) - cohort_start_date,
         event = ifelse(is.na(outcome_id), 0, 1)) %>% 
  ungroup() %>% 
  select(time_to_event, event, treatment_group = name)


km_ttnt %>% 
  count(event)

fit11 <- survival::survfit(survival::Surv(time_to_event, event) ~ treatment_group, data = km_ttnt)
fit12 <- survival::survfit(survival::Surv(time_to_event, event) ~ 1, data = km_ttnt)
bind_rows(survminer::surv_median(fit11), survminer::surv_median(fit12)) %>% 
  readr::write_csv(here::here(outputFolder, "timeToNextTreatment.csv"))


# overall survival ------

index_dates

km_survival <- df1 %>% 
  filter(cohort_definition_id == 22) %>% 
  select(subject_id, death_date = cohort_start_date) %>% 
  {left_join(index_dates, ., by = "subject_id")} %>% 
  mutate(time_to_event = as.integer(coalesce(death_date, end_of_followup) - index_date),
         event = ifelse(is.na(death_date), 0, 1))

fit21 <- survival::survfit(survival::Surv(time_to_event, event) ~ 1, data = km_survival)
fit21
plot(fit21)

fit21 %>% 
  ggsurvfit() +
  labs(
    title = "Overall Survival",
    x = "Days from initial diagnosis",
    y = "Overall survival probability"
  ) +
  add_risktable() +
  scale_y_continuous(limits = c(0,1)) +
  add_confidence_interval()

# Time to symptomatic progression ----

# cohort 20 is symptomatic progression. cohort 22 is death.

# first symptomatic progression event after index.
symptomatic_progression_or_death <- df1 %>% 
  filter(cohort_definition_id %in% c(20, 22)) %>% 
  select(subject_id, event_date = cohort_start_date) %>% 
  left_join(index_dates, by = "subject_id") %>% 
  filter(event_date > index_date) %>% 
  select(subject_id, event_date) %>% 
  group_by(subject_id) %>% 
  arrange(event_date) %>% 
  slice_head(n=1) %>% 
  ungroup()

# ttsp = time to symptomatic progression
km_ttsp <- index_dates %>% 
  left_join(symptomatic_progression_or_death, by = "subject_id") %>% 
  mutate(time_to_event = as.integer(coalesce(event_date, end_of_followup) - index_date),
         event = ifelse(is.na(event_date), 0, 1))


fit_ttsp <- survival::survfit(survival::Surv(time_to_event, event) ~ 1, data = km_ttsp)
fit_ttsp

fit_ttsp %>% 
  ggsurvfit() +
  labs(
    title = "Time to Symptomatic Progression",
    x = "Days from initial diagnosis",
    y = "Probability"
  ) +
  add_risktable() +
  scale_y_continuous(limits = c(0,1)) +
  add_confidence_interval()



km_survival <- df1 %>% 
  filter(cohort_definition_id == 22) %>% 
  select(subject_id, death_date = cohort_start_date) %>% 
  {left_join(index_dates, ., by = "subject_id")} %>% 
  mutate(time_to_event = as.integer(coalesce(death_date, end_of_followup) - index_date),
         event = ifelse(is.na(death_date), 0, 1))

fit21 <- survival::survfit(survival::Surv(time_to_event, event) ~ 1, data = km_survival)
fit21
plot(fit21)

# Frequency and time to diagnostic procedures

df1 %>% 
  filter(cohort_definition_id %in% 36:38) %>% 
  select(cohort_definition_id, subject_id, cohort_start_date, name) %>% 
  inner_join(index_dates, by = "subject_id", multiple = "all") %>% 
  mutate(day = as.integer(cohort_start_date - index_date)) %>% 
  count(name, day)




library(ggplot2)

first_treatment %>% 
  ggplot(aes(day)) +
  geom_histogram() +
  theme_bw() +
  labs(x = "Days since initial cancer diagnosis",
       y = "Person count")

first_treatment %>% 
  ggplot(aes(day)) +
  geom_histogram() +
  facet_wrap(~name) +
  theme_bw() +
  labs(x = "Days since initial cancer diagnosis",
       y = "Person count")


nrow(index_dates_without_treatment)/nrow(index_dates)




readr::write_csv(km, here::here("output", "km_all.csv"))

fit <- survival::survfit(survival::Surv(timeToEvent, event) ~ treatment_group, data = km)

median_all <- survminer::surv_median(fit)

fit <- survival::survfit(survival::Surv(timeToEvent, event) ~ 1, data = km)

survminer::surv_median(fit)



surv_info <- survminer::surv_summary(surv_info)
summary(surv_info)

survfit(Surv(time, status) ~ 1, data = lung)


km_grouped <- first_treatment %>% 
  mutate(event = 1) %>% 
  select(timeToEvent = day, event, cohort_definition_id, treatment_group = name)



surv_info <- survival::survfit(survival::Surv(timeToEvent, event) ~ treatment_group, data = km_grouped)



surv_info <- survminer::surv_summary(surv_info)

is.data.frame(surv_info)

tibble(surv_info)


tibble(#targetId = .targetId, eventId = .eventId,
  time = surv_info$time, surv = surv_info$surv, 
  n.censor = surv_info$n.censor, n.event = surv_info$n.event, n.risk = surv_info$n.risk,
  lower = surv_info$lower, upper = surv_info$upper)

library(ggsurvfit)
survfit2(Surv(timeToEvent, event) ~ 1, data = km) %>% 
  ggsurvfit(type = "risk") +
  ylim(0,1) +
  labs(
    x = "Days",
    y = "Proportion initiating treatment"
  ) +
  add_confidence_interval() +
  add_risktable()

km %>% 
  count(event) %>% 
  mutate(pct = n/sum(n))


summary(surv_info)

## Tables ----

# Can you make a table of the number of patients in the cohort and then number of patients by treatment category. 
# Also, let’s look at the median follow up time in the cohort and then by treatment category. 
df %>% 
  filter(group == "Target", stringr::str_detect(name, "radiotherapy", negate = T)) %>% 
  group_by(name) %>% 
  summarise(n_persons = n(), median_followup_time_from_index = median(followup_days)) %>% 
  arrange(name) %>% 
  rename(cohort = name) %>% 
  gt::gt() %>% 
  gt::tab_header("Overall Cohort Counts", "STARR-OMOP") %>% 
  gt::tab_row_group(rows = 2:5, label = "", id = 1) %>% 
  gt::tab_row_group(rows = 1, label = "", id = 2) %>% 
  gt::cols_label(
    cohort = "Cohort",
    n_persons = "Person Count",
    median_followup_time_from_index = "Median followup time"
  )


df %>% 
  filter(group == "Outcome") %>% 
  filter(day >=0) %>% 
  group_by(name) %>% 
  summarise(n_persons = n_distinct(subject_id), median_time_from_index_to_treatment = median(day)) %>% 
  arrange(name) %>% 
  rename(outcome = name) %>% 
  gt::gt() %>% 
  gt::tab_header("Outcome Counts", "STARR-OMOP") %>% 
  gt::cols_label(
    outcome = "Outcome",
    n_persons = "Person Count",
    median_time_from_index_to_treatment = "Median time from index to outcome"
  )



# Can you make a table of the number of patients in the cohort and then number of patients by treatment category. 
# Also, let’s look at the median follow up time in the cohort and then by treatment category. 
# And a simple description of treatments post index? And median follow up time in all, those with and without treatment.
# It can be totally ok if patients don’t get a treatment. 

