# Regression safety net (plan Section 6): compares the new pipeline's merged
# output against a fixed snapshot of the legacy Drake pipeline's output.
#
# The snapshot (tests/fixtures/legacy_merged_data.rds) is generated once via
# data-raw/snapshot_legacy_output.R. Until that has been run, this test is
# skipped rather than failed, since the fixture depends on raw data files
# that are not committed to the repository.

test_that("new pipeline output is close to the legacy snapshot (per site)", {
  legacy_path <- file.path("..", "..", "tests", "fixtures", "legacy_merged_data.rds")
  skip_if_not(file.exists(legacy_path), "legacy snapshot not generated yet - see data-raw/snapshot_legacy_output.R")

  store <- file.path("..", "..", "_targets")
  merged_community <- tryCatch(
    suppressWarnings(targets::tar_read(merged_community, store = store)),
    error = function(e) NULL
  )
  skip_if(is.null(merged_community), "targets store not built yet - run targets::tar_make() first")
  comparison <- compare_to_legacy(merged_community, legacy_path)

  big_diffs <- comparison %>%
    dplyr::filter(metric %in% c("n_rows", "n_species", "n_plots")) %>%
    dplyr::mutate(pct_diff = abs(diff) / pmax(legacy, 1)) %>%
    dplyr::filter(pct_diff > 0.05)

  expect_true(
    nrow(big_diffs) == 0,
    info = paste(
      "Unexpected regressions vs. legacy pipeline:\n",
      paste(capture.output(print(big_diffs, n = Inf)), collapse = "\n")
    )
  )
})
