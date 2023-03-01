# Copyright 2022 Observational Health Data Sciences and Informatics
#
# This file is part of PioneerMetastaticTreatment
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.



# This script populates CohortsToCreate{Target, Outcome, Strata}.csv files
# based of Phenotype Tracker excel file.
# This is the first step in preparing setting files for the study.

library(tidyverse, warn.conflicts = FALSE)

cols <- c('Pheno ID', 'Phenotype name', 'Intended use', 'Where <link to PIONEER CENTRAL ATLAS>')
cohorts_base_url <- 'https://pioneer.hzdr.de/atlas/#/cohortdefinition/'
wepapi_base_url <- 'https://pioneer.hzdr.de/WebAPI'
bearer <- "Bearer eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJhcnRlbS5nb3JiYWNoZXZAb2R5c3NldXNpbmMuY29tIiwiZXhwIjoxNjY5MTA3OTAyfQ.d4hGbKOeEX-4BnQo_Rqj5zjBIJau3oKpiEFQwfvstWg1tism_MTvwxFxqxhxgA3muUsEiySgmRr-s-boi3F2zQ"
ROhdsiWebApi::setAuthHeader(wepapi_base_url, bearer)

cohort_types <- c('t', 'o', 's')
letter_to_cohort_type <- c('t' = 'Target', 'o' = 'Outcome', 's' = 'Strata')
cohort_group_to_code <- c('target' = 100, 'outcome' = 200, 'strata' = 300)

# will be filtered from phenotypes. Ideally should be empty 
invalid_atlas_ids <- c()


phenotypes <- readxl::read_excel('extras/PIONEER studyathon phenotype tracker OCTOBER 2022.xlsx',
                                 sheet = 'Tracker')

for (column in cols) {
  if (!column %in% names(phenotypes)) {
    stop(paste0("No column ", column, " has been found. Please check Phenotype tracker file and its column names"))
  }
}

phenotypes <- phenotypes[cols]
names(phenotypes) <- c('Id', 'Name', 'IntendedUse', 'Link')
phenotypes <- phenotypes %>% 
              drop_na('Id', 'Link') %>% 
              filter(startsWith(Link, cohorts_base_url)) %>% 
              mutate(IntendedUse = replace(IntendedUse, IntendedUse == 'Stratum', 'Strata')) %>% 
              mutate(IntendedCheck = letter_to_cohort_type[tolower(substr(Id, 1, 1))] == IntendedUse) %>%
              mutate(AtlasId = str_replace(str_replace(Link, cohorts_base_url, ''), '/', '')) %>% 
              mutate(Name = str_squish(str_replace(Name, '/', ' '))) %>% 
              mutate(Name = str_replace(Name, '>', 'gt ')) %>% 
              mutate(Name = str_replace(Name, '<', 'lt ')) %>% 
              filter(!AtlasId %in% invalid_atlas_ids)


for (i in 1:nrow(phenotypes)) {
  if (phenotypes[i, 'IntendedCheck'] == FALSE) {
    stop(paste0('Inconsistent Id and IntendedUse columns for ', phenotypes[i, 'Name'], ' cohort'))
  }
}


for (i in 1:nrow(phenotypes)) {
  tryCatch(expr = {as.integer(phenotypes[i, 'AtlasId'])},
           error = function(e){
             message(paste0('Error in cohort ', phenotypes[i, 'Name']))
             print(e)
           },
           warning = function(w){
             message(paste0('Error in cohort ', phenotypes[i, 'Name']))
             print(w)
           }
    )
}


cohortGroups <- read.csv(file.path("inst/settings/CohortGroups.csv"))
for (i in 1:nrow(cohortGroups)) {
  cohort_group <- (cohortGroups[[i, 'cohortGroup']])
  group_cohorts <- phenotypes[phenotypes$IntendedUse == str_to_title(cohort_group),]
  cohorts_to_create = data.frame()
  print(paste0('Updating ', str_to_title(cohort_group)))
  
  if (nrow(group_cohorts) == 0){
    print(paste0('No cohort records for ', str_to_title(cohort_group), ' has been found'))
    next()
  }
  
  for (i in 1:nrow(group_cohorts)) {
    tryCatch(
      expr = {
            # name <- group_cohorts[[i, 'Name']]
            name <- cohort_group_to_code[cohort_group] + i
            atlas_id <- as.integer(group_cohorts[[i, 'AtlasId']])
            cohort_id <- cohort_group_to_code[cohort_group] + i
            atlas_name <- ROhdsiWebApi::getCohortDefinition(baseUrl = wepapi_base_url, cohortId = atlas_id)$name
            cohorts_to_create <- rbind(cohorts_to_create, data.frame(name = name,
                                                                     atlasName = atlas_name,
                                                                     atlasId = atlas_id,
                                                                     cohortId = cohort_id))
            },
      error = function(e){
        message(paste0('Cannot retrieve cohort: ', name, ' with ID of ', atlas_id))
      }
    )
  }
  readr::write_csv(cohorts_to_create, file.path('inst', 'settings', paste0('CohortsToCreate', str_to_title(cohort_group), '.csv')))
}



