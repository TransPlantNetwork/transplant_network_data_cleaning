# Harmonization plan: merge every site's cleaned community data into one
# dataset. (Taxonomic name harmonization itself is a separate, later stage -
# see R/taxonomy_plan.R - this plan only merges the already-cleaned,
# per-site-validated community tables together, mirroring what
# merge_comm_data() has always done.)

harmonization_plan <- list(
  tar_target(
    name = merged_community,
    command = merge_comm_data(all_sites_cleaned)
  )
)
