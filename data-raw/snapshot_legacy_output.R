# Run this script ONCE, on a checkout that still has the raw data files and
# the old Drake pipeline available, to create the regression "golden" snapshot
# that the new {targets} pipeline is checked against (see
# tests/testthat/test-regression-legacy.R and plan Section 6).
#
# It is intentionally NOT run automatically by targets/testthat: it depends
# on the legacy Drake plan (R/drake_workflow/TransPlant_DrakePlan.R) and the old
# `merge_comm_data()` inputs, which are being phased out as sites migrate.
#
# Usage:
#   Rscript data-raw/snapshot_legacy_output.R

library(drake)
library(tidyverse)

source("R/drake_workflow/TransPlant_DrakePlan.R")
source("R/functions/merge_community.R")

r_make(plan) # or drake::make(TransPlant_DrakePlan), depending on the plan object name

alldat <- drake::readd(alldat) # nolint: adjust to the actual target name holding the list of site outputs
legacy_merged_data <- merge_comm_data(alldat)

dir.create("tests/fixtures", showWarnings = FALSE, recursive = TRUE)
saveRDS(legacy_merged_data, "tests/fixtures/legacy_merged_data.rds")

message("Wrote tests/fixtures/legacy_merged_data.rds with ", nrow(legacy_merged_data), " rows.")
