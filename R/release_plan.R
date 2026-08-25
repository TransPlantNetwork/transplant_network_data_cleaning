# Release plan: bundle raw + clean data into dated files under releases/,
# ready to upload to Zenodo (manual for now - see R/functions/release.R).
# Depends on validation_summary so a release only happens once every site
# has been checked (it still runs if some checks fail - validation_summary
# only warns - but you'll see the warnings in the tar_make() output first).

release_plan <- list(
  tar_target(
    name = release_files,
    command = {
      validation_summary # force validation to run first (see file header)
      create_release(database_file, raw_data_dir = "data", release_dir = "releases")
    },
    format = "file"
  )
)
