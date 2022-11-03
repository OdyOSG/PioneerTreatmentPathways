## README

Scripts in this folder can be used to populate study settings files, import cohort definitions and run the study itself.

In order to fill in the settings files: CohortsToCreate{Target, Outcome, Strata}.csv,
targetStrataXref.csv run the following scripts (note: Phenotype tracker excel file is needed):

- **GenerateCohortToCreateSettingsFromPhenotypeTracker.R**: This script creates CohortToCreate csv files 
- **DownloadCohortDefinitionFiles.R**: Downloads sql and json cohort definition files and puts them into package
- **GenerateTargetStrataXRef.R**: Generates targetStrataXref.csv file - the full list of all combination of target/strata cohorts.
