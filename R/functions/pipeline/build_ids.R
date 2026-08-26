# Assemble UniqueID (and destPlotID, where not already present) from the
# per-site list of id components declared in the registry.

build_ids <- function(dat, site_cfg) {
  components <- site_cfg$pipeline$id_components

  if (is.null(dat[["destPlotID"]])) {
    dat$destPlotID <- do.call(paste, c(as.list(dat[intersect(components, names(dat))]), sep = "_"))
  }
  dat$destPlotID <- as.character(dat$destPlotID)
  if (is.null(dat[["destBlockID"]])) dat$destBlockID <- NA_character_
  dat$destBlockID <- as.character(dat$destBlockID)

  dat$UniqueID <- do.call(paste, c(as.list(dat[intersect(components, names(dat))]), sep = "_"))
  dat
}

#' Sum Cover for rows that share the same UniqueID x SpeciesName.
#' Several legacy per-site scripts note that a species is occasionally
#' recorded twice within the same plot x year (e.g. two observers, or a
#' species re-identified partway through), summing their Cover rather than
#' treating them as separate rows. This is a genuine feature of the raw
#' data (not a cleaning bug), so keep it as an explicit, visible step rather
#' than have it silently trip the duplicate_ids validation check.
collapse_duplicate_species <- function(dat, site_cfg) {
  id_cols <- c("UniqueID", "SpeciesName")
  other_cols <- setdiff(names(dat), c(id_cols, "Cover"))
  dat |>
    dplyr::group_by(dplyr::across(dplyr::all_of(id_cols))) |>
    dplyr::summarise(
      Cover = sum(Cover, na.rm = TRUE),
      dplyr::across(dplyr::all_of(other_cols), dplyr::first),
      .groups = "drop"
    )
}
