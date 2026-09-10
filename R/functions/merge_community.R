#### Merge all community data (with metadata) together ####
#
# Network-level filters and recodes live in config/ CSVs so they stay
# inspectable independently of this merge step:
#   - config/excluded_treatments.csv  treatments dropped at merge
#   - config/gradient_map.csv         legacy numeric Gradient -> site_id

#' Load treatments to exclude from the merged community table.
load_excluded_treatments <- function(path = "config/excluded_treatments.csv") {
  if (!file.exists(path)) {
    stop("Excluded treatments file not found: '", path, "'", call. = FALSE)
  }
  tbl <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  if (!"treatment" %in% names(tbl)) {
    stop("Excluded treatments file '", path, "' must have a 'treatment' column", call. = FALSE)
  }
  treatments <- as.character(tbl$treatment)
  treatments <- treatments[!is.na(treatments) & treatments != ""]
  if (length(treatments) == 0) {
    stop("Excluded treatments file '", path, "' has no treatment values", call. = FALSE)
  }
  treatments
}

#' Load Gradient code -> name lookup (legacy numeric codes for NO gradients).
#'
#' @return Named character vector suitable for dplyr::recode().
load_gradient_map <- function(path = "config/gradient_map.csv") {
  if (!file.exists(path)) {
    stop("Gradient map file not found: '", path, "'", call. = FALSE)
  }
  tbl <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  required <- c("from", "to")
  missing <- setdiff(required, names(tbl))
  if (length(missing) > 0) {
    stop(
      "Gradient map file '", path, "' is missing columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  if (any(is.na(tbl$from) | as.character(tbl$from) == "")) {
    stop("Gradient map file '", path, "' has blank 'from' values", call. = FALSE)
  }
  stats::setNames(as.character(tbl$to), as.character(tbl$from))
}

merge_comm_data <- function(site_outputs,
                            excluded_treatments_path = "config/excluded_treatments.csv",
                            gradient_map_path = "config/gradient_map.csv") {
  excluded_treatments <- load_excluded_treatments(excluded_treatments_path)
  gradient_map <- load_gradient_map(gradient_map_path)

  # clean up community data
  community_data <- site_outputs |>
    map_df("community", .id = "Region") |>
    ungroup() |>
    filter(!Treatment %in% excluded_treatments) |>
    select(dplyr::any_of(c(
      "Region", "Year", "originSiteID", "originBlockID", "destSiteID", "destBlockID",
      "destPlotID", "Treatment", "turfID", "UniqueID", "SpeciesName", "Cover", "Rel_Cover"
    ))) # Some unnecessary columns in NO and CH

  # add metadata to organize by elevations
  meta <- site_outputs |>
    map("meta") |>
    map(ungroup) |>
    map_df(mutate, Gradient = as.character(Gradient), .id = "Region") |>
    mutate(Gradient = dplyr::recode(Gradient, !!!gradient_map)) |>
    select(Region, destSiteID, Elevation) |>
    distinct()

  # bind
  community_with_meta <- left_join(community_data, meta, by = c("Region", "destSiteID"))

  # sanity checks:
  # unique(community_data$destSiteID) %in% unique(meta$destSiteID) # all true
  # unique(meta$destSiteID) %in% unique(community_data$destSiteID) # all true
  # community_with_meta[is.na(community_with_meta$Rel_Cover),] # no NA Rel_covers (cover yes, arizona only has rel_cover)
  # community_data[is.na(community_data$Treatment),] # no NA treatments
  # community_with_meta |> group_by(Region, destSiteID, Treatment) |> summarize(n = n()) |> View

  return(community_with_meta)
}
