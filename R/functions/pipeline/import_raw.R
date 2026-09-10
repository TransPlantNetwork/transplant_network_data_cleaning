# General-purpose raw data import, dispatching on site_cfg$raw_format.
# Used by general-pipeline sites (recipe_fn = NA). Some of those still point
# site_cfg$pipeline$import_fn at a bespoke loader when the raw layout is too
# site-specific for a one-size dispatcher (multi-file years, SeedClim sqlite
# with stomping corrections, etc.).

import_raw <- function(site_cfg) {
  # Prefer an explicit import_fn when the raw layout is genuinely bespoke
  # (e.g. CH_Lavey: different sheet/column conventions per year across
  # several files). Only the *cleaning* steps need to be general for that
  # site to count as "on the general pipeline".
  if (!is.null(site_cfg$pipeline$import_fn)) {
    return(get(site_cfg$pipeline$import_fn, mode = "function")())
  }

  path <- site_cfg$pipeline$raw_path
  switch(site_cfg$raw_format,
    excel  = readxl::read_excel(path),
    csv    = utils::read.csv(path, stringsAsFactors = FALSE),
    sqlite = import_raw_sqlite(site_cfg),
    stop("import_raw(): unsupported raw_format '", site_cfg$raw_format, "' for a general-pipeline site")
  )
}

#' Read from a sqlite raw source.
#' Turf-transplant databases (SeedClim-style) require a join query rather
#' than a single table read, so site_cfg$pipeline$sql_query (a SQL string)
#' can be supplied; otherwise the whole table named in site_cfg$pipeline$table
#' is read.
import_raw_sqlite <- function(site_cfg) {
  con <- DBI::dbConnect(RSQLite::SQLite(), site_cfg$pipeline$raw_path)
  on.exit(DBI::dbDisconnect(con))

  if (!is.null(site_cfg$pipeline$sql_query)) {
    dplyr::collect(dplyr::tbl(con, dbplyr::sql(site_cfg$pipeline$sql_query)))
  } else {
    dplyr::collect(dplyr::tbl(con, site_cfg$pipeline$table))
  }
}
