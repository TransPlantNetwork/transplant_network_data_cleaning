# Orchestrates one site's import + clean chain, dispatching to either:
#   - the general pipeline functions (import_raw -> standardize_columns ->
#     derive_treatment -> build_ids -> split_cover_classes -> compute_rel_cover),
#     for sites with recipe_fn = NA in the registry, or
#   - a legacy recipe function (R/functions/sites/legacy_recipes.R) that runs
#     the original ImportClean_* script unchanged (currently only US_Arizona).
#
# Either path returns the same contract: list(meta=, community=, cover=, taxa=[, trait=])

clean_site <- function(site_cfg) {
  if (!is.na(site_cfg$recipe_fn)) {
    recipe <- get(site_cfg$recipe_fn, mode = "function")
    args <- site_cfg$recipe_args %||% list()
    return(do.call(recipe, args))
  }

  raw <- import_raw(site_cfg)
  site_data <- standardize_columns(raw, site_cfg)
  site_data <- derive_treatment(site_data, site_cfg)
  site_data <- build_ids(site_data, site_cfg)
  site_data <- collapse_duplicate_species(site_data, site_cfg)
  site_data <- add_other_category(site_data, site_cfg)
  site_data <- compute_rel_cover(site_data, site_cfg)
  split <- split_cover_classes(site_data, site_cfg)
  comm <- split$comm
  cover <- split$cover

  meta <- build_meta(comm, site_cfg)
  taxa <- unique(comm$SpeciesName)

  list(meta = meta, community = comm, cover = cover, taxa = taxa)
}

#' Build the per-site meta table generically from a site's meta_table lookup
#' (destSiteID -> Elevation/Longitude/Latitude) plus registry-level constants.
build_meta <- function(comm, site_cfg) {
  pipeline_cfg <- site_cfg$pipeline
  comm |>
    dplyr::select(destSiteID, Year) |>
    dplyr::group_by(destSiteID) |>
    dplyr::summarise(YearMin = min(Year), YearMax = max(Year), .groups = "drop") |>
    dplyr::left_join(pipeline_cfg$meta_table, by = "destSiteID") |>
    dplyr::mutate(
      Gradient = pipeline_cfg$gradient,
      Country = pipeline_cfg$country,
      YearEstablished = pipeline_cfg$year_established,
      PlotSize_m2 = pipeline_cfg$plot_size_m2,
      YearRange = YearMax - YearEstablished
    ) |>
    dplyr::select(
      Gradient, destSiteID, Longitude, Latitude, Elevation, YearEstablished,
      YearMin, YearMax, YearRange, PlotSize_m2, Country
    )
}

`%||%` <- function(x, y) if (is.null(x)) y else x
