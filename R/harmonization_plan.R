# Harmonization plan: merge every site's cleaned community data into one
# dataset. (Taxonomic name harmonization itself is a separate, later stage -
# see R/taxonomy_plan.R - this plan only merges the already-cleaned,
# per-site-validated community tables together, mirroring what
# merge_comm_data() has always done.)

harmonization_plan <- list(
  # Track network-level CSVs so edits invalidate merged_community.
  tar_target(
    name = excluded_treatments_file,
    command = "config/excluded_treatments.csv",
    format = "file"
  ),
  tar_target(
    name = gradient_map_file,
    command = "config/gradient_map.csv",
    format = "file"
  ),
  tar_target(
    name = merged_community,
    command = merge_comm_data(
      all_sites_cleaned,
      excluded_treatments_path = excluded_treatments_file,
      gradient_map_path = gradient_map_file
    )
  )
)
