# Two steps, run in this order relative to compute_rel_cover() (see clean_site.R):
#   1. add_other_category() - for most percent-cover sites, synthesize an
#      implicit "Other" row (100 - sum(Cover), floored at 0) BEFORE Rel_Cover
#      is computed, so Total_Cover/Rel_Cover reflect the full plot including
#      Other. Some sites (e.g. CH_Lavey) already have "Other"/"Bare ground"/...
#      as real rows in the raw data and must NOT get a synthetic one added
#      (site_cfg$pipeline$add_other = FALSE); biomass sites never add one.
#   2. split_cover_classes() - pure filter, run AFTER compute_rel_cover(), that
#      splits the (already Rel_Cover-annotated) rows into `comm` (vascular
#      species) and `cover` (non-vascular cover classes, incl. any "Other").

add_other_category <- function(site_data, site_cfg) {
  add_other <- site_cfg$pipeline$add_other
  if (is.null(add_other)) add_other <- identical(site_cfg$cover_unit, "percent")

  site_data <- site_data[!is.na(site_data$Cover), ]
  if (!add_other) return(site_data)

  # Group by UniqueID only (one "Other" row per plot x year), not by every
  # remaining column: some sites carry per-row columns (e.g. CN_Gongga's raw
  # `species` code and `flag`) that differ per species and would otherwise
  # split each plot into one singleton group per row, producing one spurious
  # "Other" row per species instead of one per plot.
  other <- site_data |>
    dplyr::group_by(UniqueID) |>
    dplyr::summarise(SpeciesName = "Other", Cover = pmax(100 - sum(Cover), 0), .groups = "drop")
  dplyr::bind_rows(site_data, other)
}

#' Split a dataset that already has Total_Cover/Rel_Cover (from
#' compute_rel_cover()) into vascular-species `comm` and non-vascular `cover`
#' tables.
split_cover_classes <- function(site_data, site_cfg) {
  non_vascular <- site_cfg$pipeline$non_vascular %||% character(0)
  add_other <- site_cfg$pipeline$add_other
  if (is.null(add_other)) add_other <- identical(site_cfg$cover_unit, "percent")
  exclude <- c(non_vascular, if (add_other) "Other")

  list(
    comm = site_data |> dplyr::filter(!SpeciesName %in% exclude, Cover > 0),
    cover = site_data |>
      dplyr::filter(SpeciesName %in% exclude) |>
      dplyr::select(UniqueID, SpeciesName, Cover, Rel_Cover) |>
      dplyr::group_by(UniqueID, SpeciesName) |>
      dplyr::summarise(OtherCover = sum(Cover), Rel_OtherCover = sum(Rel_Cover), .groups = "drop") |>
      dplyr::rename(CoverClass = SpeciesName)
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x
