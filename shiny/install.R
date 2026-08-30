packages <- c(
  "shiny", "bslib", "DT", "readxl", "openxlsx", "dplyr", "tidyr",
  "purrr", "stringi", "stringdist", "fuzzyjoin", "data.table", "httr2", "jsonlite",
  "promises", "future", "progressr", "bcrypt", "remotes", "testthat"
)

installed <- rownames(installed.packages())
missing <- setdiff(packages, installed)
if (length(missing) > 0) {
  install.packages(missing, repos = "https://cloud.r-project.org")
}

if (!requireNamespace("activityinfo", quietly = TRUE)) {
  remotes::install_github("bedatadriven/activityinfo-R")
}
