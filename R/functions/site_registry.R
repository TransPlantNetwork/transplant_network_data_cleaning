# Site registry: the single source of truth for what differs between sites.
#
# Every site in the network is one row in `site_registry`. For most sites
# (recipe_fn is not NA) the row simply documents metadata about a site whose
# cleaning logic is still the original, trusted per-site code from
# R/ImportData/ImportCleanAndMakeList_*.R, wrapped by a thin "recipe" function
# in R/sites/legacy_recipes.R - see that file for why.
#
# For pilot sites (recipe_fn is NA), the row is the FULL configuration used
# by the general pipeline functions in R/pipeline/ (import_raw, standardize_columns,
# derive_treatment, build_ids, compute_rel_cover, split_cover_classes) - no
# per-site function is needed at all. CH_Lavey, US_Colorado and CN_Gongga are
# migrated this way as a proof of the general pattern (see the "pilot_migration"
# plan item). Additional sites can be migrated the same way over time by
# filling in their general-pipeline columns and removing their recipe_fn.
#
# Columns:
#   site_id        - unique site identifier, matches legacy names (e.g. "CH_Lavey")
#   raw_format     - one of "excel", "csv", "csv_delim", "sqlite", "rdata", "mixed"
#   cover_unit     - "percent" (most sites) or "biomass" (no "Other" category added)
#   treatment_rule - one of "site_pair_recode", "turfid_substring", "code_lookup",
#                    "origin_dest_matrix", or "legacy" (handled entirely by recipe_fn)
#   recipe_fn      - name of a wrapper function in R/sites/legacy_recipes.R that
#                    reproduces the site's full historical cleaning chain, or NA
#                    if the site is fully migrated onto the general pipeline
#   recipe_args    - list-column of extra arguments to pass to recipe_fn (e.g. NO_Norway's `g`)
#   raw_path       - path(s) to the raw data file(s), relative to the project root
#   column_map     - list-column: named character vector mapping raw column name -> canonical name
#   id_components  - list-column: character vector of columns pasted together to build UniqueID
#   non_vascular   - list-column: character vector of SpeciesName values treated as cover classes, not community
#   meta_table     - list-column: tibble with destSiteID, Elevation, Longitude, Latitude for meta building
#   gradient       - Gradient name used in meta (defaults to site_id)
#   country        - Country used in meta
#   year_established - numeric, YearEstablished used in meta
#   plot_size_m2   - numeric, PlotSize_m2 used in meta
#   notes          - free text

site_registry <- tibble::tribble(
  ~site_id,              ~raw_format, ~cover_unit, ~treatment_rule,       ~recipe_fn,                              ~recipe_args,        ~notes,

  # --- Pilot sites: fully migrated onto the general pipeline (no recipe_fn) ---
  "CH_Lavey",            "excel",     "percent",   "site_pair_recode",   NA_character_,                            list(),          "Pilot site 1 of 3 (excel format)",
  "US_Colorado",         "csv",       "percent",   "turfid_substring",   NA_character_,                            list(),          "Pilot site 2 of 3 (csv format)",
  "CN_Gongga",           "sqlite",    "percent",   "code_lookup",        NA_character_,                            list(),          "Pilot site 3 of 3 (sqlite format)",

  # --- Remaining sites: registered for the unified pipeline/validation/database,
  #     cleaning logic still delegated to the original, trusted per-site code ---
  "CH_Calanda",          "excel",     "percent",   "legacy",             "clean_recipe_CH_Calanda",                list(),          "Bespoke duplicate-species fix; migrate later",
  "CH_Calanda2",         "csv",       "percent",   "legacy",             "clean_recipe_CH_Calanda2",               list(),          "Plot-number based origin/treatment logic",
  "NO_Ulvhaugen",        "sqlite",    "percent",   "legacy",             "clean_recipe_NO_Norway",                 list(g = 1),   "SeedClim database + gradient filter g=1",
  "NO_Lavisdalen",       "sqlite",    "percent",   "legacy",             "clean_recipe_NO_Norway",                 list(g = 2),   "SeedClim database + gradient filter g=2",
  "NO_Gudmedalen",       "sqlite",    "percent",   "legacy",             "clean_recipe_NO_Norway",                 list(g = 3),   "SeedClim database + gradient filter g=3",
  "NO_Skjellingahaugen", "sqlite",    "percent",   "legacy",             "clean_recipe_NO_Norway",                 list(g = 4),   "SeedClim database + gradient filter g=4",
  "US_Montana",          "mixed",     "percent",   "legacy",             "clean_recipe_US_Montana",                list(),          "Most cleaning happens in the loader script",
  "US_Arizona",          "excel",     "percent",   "legacy",             "clean_recipe_US_Arizona",                list(),          "Individual counts converted to relative cover",
  "CN_Damxung",          "excel",     "percent",   "legacy",             "clean_recipe_CN_Damxung",                list(),          "Cover-class midpoint recoding",
  "CN_Heibei",           "excel",     "percent",   "legacy",             "clean_recipe_CN_Heibei",                 list(),          "3-way origin x dest treatment matrix incl. Cold",
  "IN_Kashmir",          "excel",     "percent",   "legacy",             "clean_recipe_IN_Kashmir",                list(),          "Two raw files bound together; cover-class midpoints",
  "DE_Grainau",          "excel",     "percent",   "legacy",             "clean_recipe_DE_Grainau",                list(),          "Cover-class midpoint recoding",
  "DE_TransAlps",        "mixed",     "biomass",   "legacy",             "clean_recipe_DE_TransAlps",              list(),          "Biomass, no Other cover class",
  "DE_Susalps",          "mixed",     "biomass",   "legacy",             "clean_recipe_DE_Susalps",                list(),          "Biomass, no Other cover class",
  "FR_AlpeHuez",         "excel",     "percent",   "legacy",             "clean_recipe_FR_AlpeHuez",               list(),          "Cover-class recoding + date parsing",
  "FR_Lautaret",         "csv",       "percent",   "legacy",             "clean_recipe_FR_Lautaret",               list(),          "Two raw sources (2017-2021 and 2022) bound together",
  "SE_Abisko",           "excel",     "percent",   "legacy",             "clean_recipe_SE_Abisko",                 list(),          "Wide-format cover data (species as columns)",
  "IT_MatschMazia1",     "sqlite",    "percent",   "legacy",             "clean_recipe_IT_MatschMazia1",           list(),          "Wide-format cover data (species as columns)",
  "IT_MatschMazia2",     "sqlite",    "percent",   "legacy",             "clean_recipe_IT_MatschMazia2",           list(),          "Wide-format cover data (species as columns)"
)

# --- Pilot site configuration (used only by sites with recipe_fn == NA) ---
# Kept as a separate lookup (rather than more tribble columns) because each
# entry is itself a nested structure (named vectors / tibbles).

site_pipeline_config <- list(
  CH_Lavey = list(
    raw_path = "data/CH_Lavey",
    # Raw layout genuinely differs by year (different sheet/column
    # conventions each year) - reuse the existing, already-correct loader
    # rather than reinventing it in import_raw(). Only the cleaning steps
    # below are "general" for this site.
    import_fn = "load_cover_CH_Lavey",
    id_components = c("Year", "originSiteID", "destSiteID", "plotID"),
    # CH_Lavey's raw data already includes "Other"/"Dead"/"Bare ground"/...
    # as real SpeciesName rows (confirmed against the legacy processed
    # output: Total_Cover already reaches >=100 without synthesizing
    # anything) - do NOT add a synthetic "Other" row like most other sites.
    add_other = FALSE,
    non_vascular = c(
      "Other", "Dead", "Bare ground", "bare ground", "Bryophyta", "Stone",
      "Fungi", "Vegetation %", "Bare ground %", "Litter %", "Rocks %"
    ),
    treatment_map = c(
      "CRE_CRE" = "LocalControl", "RIO_RIO" = "LocalControl",
      "MAR_MAR" = "LocalControl", "PRA_PRA" = "LocalControl",
      "CRE_RIO" = "Warm", "MAR_RIO" = "Warm", "PRA_RIO" = "Warm"
    ),
    meta_table = tibble::tribble(
      ~destSiteID, ~Elevation, ~Longitude, ~Latitude,
      "PRA", 1400, 7.0396599, 46.216459,
      "MAR", 1750, 7.051020, 46.21567,
      "CRE", 1950, 7.0546499, 46.2181,
      "RIO", 2200, 7.06053, 46.20429
    ),
    gradient = "CH_Lavey",
    country = "Switzerland",
    year_established = 2016,
    plot_size_m2 = 1
  ),
  US_Colorado = list(
    raw_path = "data/US_Colorado/US_Colorado_commdata/Final_IntensiveCover_2018-2023_RMBLTransplant.csv",
    id_components = c("Year", "originSiteID", "destSiteID", "destBlockID", "destPlotID"),
    non_vascular = c(
      "Other", "Bare Soil", "Bare soil", "bare soil", "Litter", "LItter",
      "litter", "rock", "Rock", "moss"
    ),
    treatment_map = c(
      "c1" = "Cold", "c2" = "Cold", "w1" = "Warm", "w2" = "Warm",
      "nu" = "NettedControl", "u_" = "Control", "ws" = "LocalControl"
    ),
    meta_table = tibble::tribble(
      ~destSiteID, ~Elevation, ~Longitude, ~Latitude,
      "um", 2900, -107.010180, 38.9324799,
      "pf", 3200, -107.0326199, 38.9324799,
      "mo", 3300, -107.04908, 38.9727400
    ),
    gradient = "US_Colorado",
    country = "USA",
    year_established = 2017,
    plot_size_m2 = 0.25
  ),
  CN_Gongga = list(
    raw_path = "data/CN_Gongga/transplant.sqlite",
    sql_query = paste(
      "SELECT sites.siteID AS originSiteID, blocks.blockID AS originBlockID,",
      "plots.plotID AS originPlotID, turfs.turfID,",
      "plots_1.plotID AS destPlotID, blocks_1.blockID AS destBlockID, sites_1.siteID AS destSiteID,",
      "turfs.TTtreat, turfCommunity.year, turfCommunity.species, turfCommunity.cover,",
      "turfCommunity.flag, taxon.speciesName",
      "FROM blocks, sites, plots, turfs, turfCommunity, plots AS plots_1, blocks AS blocks_1, sites AS sites_1, taxon",
      "WHERE blocks.siteID = sites.siteID AND plots.blockID = blocks.blockID",
      "AND turfs.originPlotID = plots.plotID AND turfCommunity.turfID = turfs.turfID",
      "AND turfs.destinationPlotID = plots_1.plotID AND blocks_1.siteID = sites_1.siteID",
      "AND plots_1.blockID = blocks_1.blockID AND turfCommunity.species = taxon.species"
    ),
    id_components = c("Year", "originSiteID", "destSiteID", "destBlockID", "Treatment", "destPlotID", "turfID"),
    non_vascular = c("Other"),
    # Raw TTtreat codes from the sqlite database map directly to canonical
    # Treatment values (the legacy loader did this via an intermediate
    # "control"/"warm1"/... step during import, but that's not needed here).
    treatment_map = c(
      "C" = "Control", "O" = "LocalControl",
      "1" = "Warm", "2" = "Cold", "3" = "Warm", "4" = "Cold"
    ),
    meta_table = tibble::tribble(
      ~destSiteID, ~Elevation, ~Longitude, ~Latitude,
      "L", 3000, 102.0343, 29.84347,
      "M", 3500, 102.0360, 29.86192,
      "A", 3850, 102.0173, 29.88911,
      "H", 4100, 102.0118, 29.90742
    ),
    gradient = "CN_Gongga",
    country = "China",
    year_established = 2012,
    plot_size_m2 = 0.0625
  )
)

#' Get the full config (registry row + pipeline config, if any) for one site
get_site_config <- function(site_id, registry = site_registry, pipeline_config = site_pipeline_config) {
  row <- registry[registry$site_id == site_id, ]
  if (nrow(row) != 1) stop("site_id '", site_id, "' not found (or duplicated) in site_registry", call. = FALSE)
  cfg <- as.list(row)
  # recipe_args is a list-column where each row holds an argument list
  # (e.g. list(g = 1) for NO_Norway) or an empty list (meaning "no extra args").
  cfg$recipe_args <- row$recipe_args[[1]]
  cfg$pipeline <- pipeline_config[[site_id]]
  cfg
}
