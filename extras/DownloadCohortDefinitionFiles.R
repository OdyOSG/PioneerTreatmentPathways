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
# One should get valid JWT Atlas session token and store it in bearer variable.
# If script crashes try to update bearer variable with a new token.

# ROhdsiWebApi version to install:
# remotes::install_github("ohdsi/ROhdsiWebApi", ref="develop")

library(PioneerMetastaticTreatement)

# get this token from an active ATLAS web session
bearer <- "Bearer eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJhcnRlbS5nb3JiYWNoZXZAb2R5c3NldXNpbmMuY29tIiwiZXhwIjoxNjY3NTEyMTk2fQ.96KA66TPEYHo9qa3V3PgUeZmiVHAUk3pduKNmKQDNZNvqSEOcfe-gGNMSTjvxXvaXZJ4qwM9g9JMfAcSi3QuFg"
baseUrl <- "https://pioneer.hzdr.de/WebAPI"
ROhdsiWebApi::setAuthHeader(baseUrl, bearer)

cohortGroups <- read.csv(file.path("inst/settings/CohortGroups.csv"))
for(i in 1:nrow(cohortGroups)) {
  ROhdsiWebApi::insertCohortDefinitionSetInPackage(fileName = file.path('inst', cohortGroups$fileName[i]),
                                                   baseUrl, insertCohortCreationR = FALSE,
                                                   packageName = getThisPackageName())
}


