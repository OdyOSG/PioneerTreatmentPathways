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



# This file contains cohort definitions update code and can't be run as is
# One should get valid JWT Atlas session token and call the function with the token as an argument (without "Bearer" prefix)
# If script crashes try to rerun getCohortDefinitionsFromAtlas() function with a new token.


# get this token from an active ATLAS web session

# devtools::install_github("OHDSI/ROhdsiWebApi")

df <- readr::read_csv("extras/phenotype_tracker.csv")

#columns to create are: name, atlasName, atlasId, cohortId

library(dplyr)

df2 <- df %>% 
  transmute(name = cohort_id,
            atlasName = phenotype_name,
            atlasId = stringr::str_extract(atlas_link, "\\d+$"),
            cohortId = cohort_id,
            group = type,
            atlas_link = atlas_link) %>% 
  filter(!is.na(atlas_link)) # fix issues here


df2 %>% 
  filter_all(is.na)

# df3 <- filter(df2, is.na(atlasId))

readr::write_csv(df2, "input/settings/CohortsToCreate.csv")

bearer <- rstudioapi::askForPassword("Enter Bearer token")

baseUrl <- "https://pioneer.hzdr.de/WebAPI"
ROhdsiWebApi::setAuthHeader(baseUrl, bearer)
  
ROhdsiWebApi::insertCohortDefinitionSetInPackage(fileName = "input/settings/CohortsToCreate.csv",
                                                 baseUrl, 
                                                 jsonFolder = "input/cohorts",
                                                 sqlFolder = "input/sql/sql_server",
                                                 insertTableSql = FALSE,
                                                 insertCohortCreationR = FALSE,
                                                 packageName = "PioneerTreatmentPathways",
                                                 generateStats = TRUE)

