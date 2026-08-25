# Bundle raw + clean data into dated, Zenodo-ready files under releases/.
#
# Raw and clean data are versioned *separately* (see plan discussion): the
# raw bundle is a straight zip of data/ (provenance - "what raw data produced
# this?"), the clean bundle is the canonical SQLite database plus a CSV
# mirror of each table (most downstream users want SQLite; some only want
# CSV). File names carry an ISO date so they sort chronologically and it's
# obvious which is newest; uploading to Zenodo itself is a manual step for
# now - this just produces the files to drag in.

create_release <- function(database_file, raw_data_dir = "data", release_dir = "releases",
                            date = format(Sys.Date(), "%Y-%m-%d")) {
  if (!dir.exists(release_dir)) dir.create(release_dir, recursive = TRUE)

  raw_zip <- zip_directory(raw_data_dir, file.path(release_dir, paste0("transplant_raw_data_", date, ".zip")))
  clean_db <- copy_clean_database(database_file, file.path(release_dir, paste0("transplant_clean_data_", date, ".sqlite")))
  clean_csv_zip <- export_database_csvs(database_file, file.path(release_dir, paste0("transplant_clean_data_", date, "_csv.zip")))
  changelog <- write_release_changelog(file.path(release_dir, paste0("CHANGELOG_", date, ".md")), date)

  c(raw_zip, clean_db, clean_csv_zip, changelog)
}

#' Resolve `path` to an absolute path, creating its parent directory if
#' needed. Unlike normalizePath(path, mustWork = FALSE), this always returns
#' an absolute path even when `path` itself doesn't exist yet - needed here
#' because these paths must stay valid across setwd() calls (see
#' zip_directory() / export_database_csvs()).
abs_path <- function(path) {
  dir <- dirname(path)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  file.path(normalizePath(dir), basename(path))
}

#' Zip up a whole directory, preserving its top-level folder name inside the
#' archive (e.g. data/US_Colorado/... , not just US_Colorado/...).
zip_directory <- function(dir, zipfile) {
  zipfile <- abs_path(zipfile)
  if (file.exists(zipfile)) file.remove(zipfile)

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(dirname(normalizePath(dir)))
  utils::zip(zipfile = zipfile, files = basename(dir), flags = "-r9Xq")

  zipfile
}

copy_clean_database <- function(database_file, dest) {
  file.copy(database_file, dest, overwrite = TRUE)
  dest
}

#' Export every table in the clean SQLite database as a CSV, zipped together.
export_database_csvs <- function(database_file, csv_zip) {
  csv_zip <- abs_path(csv_zip)
  con <- DBI::dbConnect(RSQLite::SQLite(), database_file)
  on.exit(DBI::dbDisconnect(con))

  tmp_dir <- tempfile("transplant_clean_csv_")
  dir.create(tmp_dir)
  for (tbl in DBI::dbListTables(con)) {
    utils::write.csv(DBI::dbReadTable(con, tbl), file.path(tmp_dir, paste0(tbl, ".csv")), row.names = FALSE)
  }

  if (file.exists(csv_zip)) file.remove(csv_zip)
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(tmp_dir)
  utils::zip(zipfile = csv_zip, files = list.files("."), flags = "-9Xq")

  csv_zip
}

#' List commits since the last data release (tagged data-release-<date>) so
#' each release documents what changed in the *cleaning code* since the
#' previous one. Raw data itself isn't git-tracked, so this can't capture raw
#' data changes - only the pipeline logic that produced this version.
write_release_changelog <- function(path, date) {
  last_tag <- suppressWarnings(system2(
    "git", c("describe", "--tags", "--match", "data-release-*", "--abbrev=0"),
    stdout = TRUE, stderr = FALSE
  ))
  has_tag <- length(last_tag) == 1 && is.null(attr(last_tag, "status"))

  if (has_tag) {
    range <- paste0(last_tag, "..HEAD")
    header <- paste0("Changes to the cleaning pipeline since `", last_tag, "`:")
  } else {
    range <- "HEAD"
    header <- "No previous `data-release-*` tag found - listing all commits:"
  }

  log <- system2("git", c("log", "--oneline", "--no-merges", range), stdout = TRUE)
  if (length(log) == 0) log <- "(no pipeline changes since the last release)"

  writeLines(
    c(
      paste("# Data release", date), "",
      header, "",
      paste("-", log), "",
      "Raw data changes aren't tracked in git; compare the raw data zip against the previous release if needed.", "",
      paste0("After uploading this release, tag it so the next changelog starts from here: ",
             "`git tag data-release-", date, " && git push origin data-release-", date, "`")
    ),
    path
  )
  path
}
