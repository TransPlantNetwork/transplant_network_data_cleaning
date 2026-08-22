# Database plan: write the validated, taxonomy-harmonized merged dataset
# into a single canonical SQLite file (see R/pipeline/write_database.R).

database_plan <- list(
  tar_target(
    name = database_file,
    command = write_database(merged_community_harmonized, all_sites_cleaned),
    format = "file"
  )
)
