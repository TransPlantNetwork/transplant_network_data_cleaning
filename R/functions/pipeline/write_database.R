# Write the final, validated, taxonomy-harmonized merged dataset into a
# single versioned SQLite database (mirrors what merge_comm_data() has
# always returned in memory, but persisted).

write_database <- function(merged_community_harmonized, all_sites_cleaned,
                            path = "data/transplant_network_clean.sqlite") {
  if (!dir.exists(dirname(path))) dir.create(dirname(path), recursive = TRUE)
  if (file.exists(path)) file.remove(path)

  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con))

  meta <- dplyr::bind_rows(purrr::map(all_sites_cleaned, "meta"), .id = "Region")

  DBI::dbWriteTable(con, "community", merged_community_harmonized)
  DBI::dbWriteTable(con, "meta", meta)

  path
}
