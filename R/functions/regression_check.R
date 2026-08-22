# Regression safety net: compare the new pipeline's merged output against a
# fixed snapshot of the legacy Drake pipeline's output (plan Section 6).
#
# The snapshot itself (tests/fixtures/legacy_merged_data.rds) is generated
# once via data-raw/snapshot_legacy_output.R on a checkout that still has the
# raw data and the old Drake plan available - it is not regenerated
# automatically. Differences are expected to shrink to only
# intentional/documented changes as migration proceeds, not necessarily reach
# exact equality forever.

#' Compare new vs. legacy merged community data at a coarse, per-site level:
#' row counts, total cover, distinct species/treatment/plot counts.
#' Returns a tidy data frame (Region, metric, legacy, new, diff) instead of
#' throwing, so it can be inspected like the other validation reports.
compare_to_legacy <- function(new_merged, legacy_path = "tests/fixtures/legacy_merged_data.rds") {
  if (!file.exists(legacy_path)) {
    message(
      "No legacy snapshot found at '", legacy_path, "'. ",
      "Run data-raw/snapshot_legacy_output.R once to create it. Skipping regression check."
    )
    return(tibble::tibble(Region = character(), metric = character(), legacy = double(), new = double(), diff = double()))
  }

  legacy <- readRDS(legacy_path)

  summarise_region <- function(dat) {
    dat %>%
      dplyr::group_by(Region) %>%
      dplyr::summarise(
        n_rows = dplyr::n(),
        total_cover = sum(Cover, na.rm = TRUE),
        n_species = dplyr::n_distinct(SpeciesName),
        n_treatments = dplyr::n_distinct(Treatment),
        n_plots = dplyr::n_distinct(destPlotID),
        .groups = "drop"
      )
  }

  legacy_summary <- summarise_region(legacy) %>% tidyr::pivot_longer(-Region, names_to = "metric", values_to = "legacy")
  new_summary <- summarise_region(new_merged) %>% tidyr::pivot_longer(-Region, names_to = "metric", values_to = "new")

  dplyr::full_join(legacy_summary, new_summary, by = c("Region", "metric")) %>%
    dplyr::mutate(diff = new - legacy)
}
