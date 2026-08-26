# Orchestrates one site's import + clean chain, dispatching to either:
#   - the general pipeline functions (import_raw -> standardize_columns ->
#     derive_treatment -> build_ids -> split_cover_classes -> compute_rel_cover),
#     for sites fully migrated onto the config-driven pipeline, or
#   - a legacy recipe function (R/sites/legacy_recipes.R) that reproduces the
#     site's original, trusted cleaning chain unchanged, for sites not yet migrated.
#
# Either path returns the same contract: list(meta=, community=, cover=, taxa=[, trait=])

clean_site <- function(site_cfg) {
  if (!is.na(site_cfg$recipe_fn)) {
    recipe <- get(site_cfg$recipe_fn, mode = "function")
    args <- site_cfg$recipe_args %||% list()
    return(do.call(recipe, args))
  }

  raw <- import_raw(site_cfg)
  dat <- standardize_columns(raw, site_cfg)
  dat <- derive_treatment(dat, site_cfg)
  dat <- build_ids(dat, site_cfg)
  dat <- collapse_duplicate_species(dat, site_cfg)
  dat <- add_other_category(dat, site_cfg)
  dat <- compute_rel_cover(dat, site_cfg)
  split <- split_cover_classes(dat, site_cfg)
  comm <- split$comm
  cover <- split$cover

  meta <- build_meta(comm, site_cfg)
  taxa <- unique(comm$SpeciesName)

  list(meta = meta, community = comm, cover = cover, taxa = taxa)
}

#' Build the per-site meta table generically from a site's meta_table lookup
#' (destSiteID -> Elevation/Longitude/Latitude) plus registry-level constants.
build_meta <- function(comm, site_cfg) {
  p <- site_cfg$pipeline
  comm |>
    dplyr::select(destSiteID, Year) |>
    dplyr::group_by(destSiteID) |>
    dplyr::summarise(YearMin = min(Year), YearMax = max(Year), .groups = "drop") |>
    dplyr::left_join(p$meta_table, by = "destSiteID") |>
    dplyr::mutate(
      Gradient = p$gradient,
      Country = p$country,
      YearEstablished = p$year_established,
      PlotSize_m2 = p$plot_size_m2,
      YearRange = YearMax - YearEstablished
    ) |>
    dplyr::select(
      Gradient, destSiteID, Longitude, Latitude, Elevation, YearEstablished,
      YearMin, YearMax, YearRange, PlotSize_m2, Country
    )
}

`%||%` <- function(x, y) if (is.null(x)) y else x
