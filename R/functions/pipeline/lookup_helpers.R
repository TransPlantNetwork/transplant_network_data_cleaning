# Shared CSV lookup helpers used by standardize_columns() (cover-class
# midpoints and per-site species name fixes). Files live at config/ and are
# tracked as targets file dependencies via R/site_plan.R.

#' Load one cover-class -> midpoint percent scale from config/cover_scales.csv.
#'
#' @param scale_id Scale name (e.g. "grainau_13", "damxung_10", "kashmir_11").
#' @param path Path to the cover scales CSV.
#' @return Named numeric vector suitable for dplyr::recode() on Cover.
load_cover_scale <- function(scale_id, path = "config/cover_scales.csv") {
  if (!file.exists(path)) {
    stop("Cover scales file not found: '", path, "'", call. = FALSE)
  }
  tbl <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character")
  required <- c("scale_id", "class", "midpoint")
  missing <- setdiff(required, names(tbl))
  if (length(missing) > 0) {
    stop("Cover scales file '", path, "' is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  scale <- tbl[tbl$scale_id == scale_id, , drop = FALSE]
  if (nrow(scale) == 0) {
    stop("Cover scale '", scale_id, "' not found in '", path, "'", call. = FALSE)
  }
  stats::setNames(as.numeric(scale$midpoint), as.character(scale$class))
}

#' Recode cover-class codes to midpoint percents using a named scale.
apply_cover_scale <- function(cover, scale_id, path = "config/cover_scales.csv") {
  scale <- load_cover_scale(scale_id, path)
  dplyr::recode(as.character(cover), !!!scale)
}

#' Load species name overrides for one site from config/species_recode.csv.
#'
#' @return Named character vector (from -> to), or empty named character if
#'   the site has no rows.
load_species_recode <- function(site_id, path = "config/species_recode.csv") {
  if (!file.exists(path)) {
    stop("Species recode file not found: '", path, "'", call. = FALSE)
  }
  tbl <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character")
  required <- c("site_id", "from", "to")
  missing <- setdiff(required, names(tbl))
  if (length(missing) > 0) {
    stop("Species recode file '", path, "' is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  rows <- tbl[tbl$site_id == site_id, , drop = FALSE]
  if (nrow(rows) == 0) {
    return(stats::setNames(character(), character()))
  }
  stats::setNames(as.character(rows$to), as.character(rows$from))
}

#' Apply per-site species name overrides; no-op if the site has none.
apply_species_recode <- function(species, site_id, path = "config/species_recode.csv") {
  map <- load_species_recode(site_id, path)
  if (length(map) == 0) {
    return(species)
  }
  dplyr::recode(as.character(species), !!!map)
}
