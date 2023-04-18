# pip install jupytext
getwd()
system("jupytext --to notebook 01_extract_data.Rmd")

knitr::purl(here::here("01_extract_data.Rmd"))
