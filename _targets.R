# targets pipeline for TransPlant cleaning code

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)

# Set target options:
tar_option_set(
  packages = c(
    "tidyverse",
    "vegan",
    "readxl",
    "lubridate",
    "e1071",
    "DBI",
    "RSQLite",
    "dbplyr",
    "visNetwork",
    "dataDownloader",
    "yaml",
    "rlang"
  ),
  # Each site is cleaned independently (see site_plan.R); one site's raw
  # data being missing/malformed shouldn't block cleaning every other site,
  # so let per-site targets fail without aborting the whole tar_make() run.
  error = "continue"
  # format = "qs", # Optionally set the default storage format. qs is fast.
  #
  # Pipelines that take a long time to run may benefit from
  # optional distributed computing. To use this capability
  # in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller that scales up to a maximum of two workers
  # which run as local R processes. Each worker launches when there is work
  # to do and exits if 60 seconds pass with no tasks to run.
  #
  #   controller = crew::crew_controller_local(workers = 2, seconds_idle = 60)
  #
  # Alternatively, if you want workers to run on a high-performance computing
  # cluster, select a controller from the {crew.cluster} package.
  # For the cloud, see plugin packages like {crew.aws.batch}.
  # The following example is a controller for Sun Grid Engine (SGE).
  #
  #   controller = crew.cluster::crew_controller_sge(
  #     # Number of workers that the pipeline can scale up to:
  #     workers = 10,
  #     # It is recommended to set an idle time so workers can shut themselves
  #     # down if they are not running tasks.
  #     seconds_idle = 120,
  #     # Many clusters install R as an environment module, and you can load it
  #     # with the script_lines argument. To select a specific verison of R,
  #     # you may need to include a version string, e.g. "module load R/4.3.2".
  #     # Check with your system administrator if you are unsure.
  #     script_lines = "module load R"
  #   )
  #
  # Set other options as needed.
)

# A few legacy per-site loaders (CN_Heibei, NO_Norway, US_Arizona, CN_Gongga)
# still call file_in(), a Drake-only helper that just marked a path as a
# dependency for change-detection. targets doesn't have (or need) an
# equivalent for these already-imported legacy scripts, so provide a
# no-op pass-through instead of rewriting each loader.
file_in <- function(x) x

# Run the R scripts needed by the pipeline. We source explicit paths rather
# than all of R/ recursively, because R/old_code/ (the old Drake plan, plus
# manual QA plots, climate raster processing, and old taxize-based taxonomy
# code) depends on packages (turfmapper, raster/sf, taxize, drake) that are
# not part of this pipeline's dependencies and are not needed to run it.
tar_source(c(
  "R/functions",
  "R/site_plan.R",
  "R/download_plan.R", "R/harmonization_plan.R", "R/validation_plan.R",
  "R/taxonomy_plan.R", "R/regression_plan.R", "R/database_plan.R", "R/release_plan.R"
))

# Build the per-site tar_map() plan now that all functions/data (site_registry,
# clean_site(), validate_site(), legacy recipes, ...) are guaranteed sourced.
site_plan <- build_site_plan()

# Combine target plans.
# Each stage is its own file/plan, kept in pipeline order:
#   download_plan       - fetch raw data from OSF (R/download_plan.R)
#   site_plan           - per-site import/clean/validate, one branch per site (R/site_plan.R)
#   harmonization_plan  - merge all sites' cleaned community data (R/harmonization_plan.R)
#   validation_plan     - combined validation summary/report (R/validation_plan.R)
#   taxonomy_plan       - TNRS-based taxonomic name resolution (R/taxonomy_plan.R)
#   regression_plan     - compare against legacy pipeline snapshot (R/regression_plan.R)
#   database_plan       - write the canonical output database (R/database_plan.R)
#   release_plan        - bundle dated raw+clean data files for Zenodo (R/release_plan.R)
combined_plan <- c(
  download_plan,
  site_plan,
  harmonization_plan,
  validation_plan,
  taxonomy_plan,
  regression_plan,
  database_plan,
  release_plan
)

combined_plan
