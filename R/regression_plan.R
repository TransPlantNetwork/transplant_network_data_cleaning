# Regression plan: compare the new pipeline's merged output against the
# legacy Drake pipeline's snapshot (see R/functions/regression_check.R and plan Section 6).

regression_plan <- list(
  tar_target(
    name = regression_report,
    command = compare_to_legacy(merged_community)
  )
)
