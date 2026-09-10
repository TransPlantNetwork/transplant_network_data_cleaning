# Assemble UniqueID (and destPlotID, where not already present) from the
# per-site list of id components declared in the registry.

build_ids <- function(site_data, site_cfg) {
  components <- site_cfg$pipeline$id_components

  if (is.null(site_data[["destPlotID"]])) {
    site_data$destPlotID <- do.call(paste, c(as.list(site_data[intersect(components, names(site_data))]), sep = "_"))
  }
  site_data$destPlotID <- as.character(site_data$destPlotID)
  if (is.null(site_data[["destBlockID"]])) site_data$destBlockID <- NA_character_
  site_data$destBlockID <- as.character(site_data$destBlockID)

  site_data$UniqueID <- do.call(paste, c(as.list(site_data[intersect(components, names(site_data))]), sep = "_"))
  site_data
}

#' Sum Cover for rows that share the same UniqueID x SpeciesName.
#' Several legacy per-site scripts note that a species is occasionally
#' recorded twice within the same plot x year (e.g. two observers, or a
#' species re-identified partway through), summing their Cover rather than
#' treating them as separate rows. This is a genuine feature of the raw
#' data (not a cleaning bug), so keep it as an explicit, visible step rather
#' than have it silently trip the duplicate_ids validation check.
collapse_duplicate_species <- function(site_data, site_cfg) {
  id_cols <- c("UniqueID", "SpeciesName")
  other_cols <- setdiff(names(site_data), c(id_cols, "Cover"))
  site_data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(id_cols))) |>
    dplyr::summarise(
      Cover = sum(Cover, na.rm = TRUE),
      dplyr::across(dplyr::all_of(other_cols), dplyr::first),
      .groups = "drop"
    )
}
