# run the study
source(here::here("00_study_parameters.R"))

rmarkdown::render("01_extract_data.Rmd", 
                  "html_document",
                  output_dir = here::here(),
                  output_file = "01_extract_data.html")

source(here::here("01_create_study_outputs"))