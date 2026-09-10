# Run with: Rscript tests/testthat.R
# (this project is not an installable package - we just source R/ and run the tests)

library(testthat)
suppressPackageStartupMessages({
  library(targets)
  library(tarchetypes)
  library(tidyverse)
  library(DBI)
  library(RSQLite)
})

# Source only what the pipeline tests need. R/drake_workflow/ (the old Drake plan,
# manual QA plots, climate raster processing, old taxize-based taxonomy code)
# depends on packages that are not part of the {targets} pipeline (turfmapper,
# drake, taxize) and is intentionally excluded here.
r_files <- list.files("R", pattern = "\\.[rR]$", recursive = TRUE, full.names = TRUE)
r_files <- r_files[!grepl("^R/drake_workflow", r_files)]
for (f in r_files) source(f)

test_dir("tests/testthat")
