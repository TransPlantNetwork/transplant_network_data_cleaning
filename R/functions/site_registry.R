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
  "CH_Calanda2",         "csv",       "percent",   "already_derived",    NA_character_,                            list(),          "Migrated: site x plot-number treatment logic",
  "US_Montana",          "mixed",     "percent",   "already_derived",    NA_character_,                            list(),          "Migrated: Treatment/originSiteID derived in the raw loader",
  "SE_Abisko",           "excel",     "percent",   "site_pair_recode",   NA_character_,                            list(),          "Migrated: wide-format sheet gathered to long in standardize_columns()",
  "DE_Grainau",          "excel",     "percent",   "already_derived",    NA_character_,                            list(),          "Migrated: site x code treatment logic + cover-class midpoint recoding",
  "CN_Damxung",          "excel",     "percent",   "already_derived",    NA_character_,                            list(),          "Migrated: site x code treatment logic + cover-class midpoint recoding",
  "CN_Heibei",           "excel",     "percent",   "already_derived",    NA_character_,                            list(),          "Migrated: 3-way origin x dest treatment matrix incl. Cold",
  "DE_Susalps",          "mixed",     "biomass",   "already_derived",    NA_character_,                            list(),          "Migrated: biomass, origin x dest treatment matrix, no Other class",

  # --- Remaining sites: registered for the unified pipeline/validation/database,
  #     cleaning logic still delegated to the original, trusted per-site code ---
  "CH_Calanda",          "excel",     "percent",   "legacy",             "clean_recipe_CH_Calanda",                list(),          "Bespoke duplicate-species fix; migrate later",
  "NO_Ulvhaugen",        "sqlite",    "percent",   "legacy",             "clean_recipe_NO_Norway",                 list(g = 1),   "SeedClim database + gradient filter g=1",
  "NO_Lavisdalen",       "sqlite",    "percent",   "legacy",             "clean_recipe_NO_Norway",                 list(g = 2),   "SeedClim database + gradient filter g=2",
  "NO_Gudmedalen",       "sqlite",    "percent",   "legacy",             "clean_recipe_NO_Norway",                 list(g = 3),   "SeedClim database + gradient filter g=3",
  "NO_Skjellingahaugen", "sqlite",    "percent",   "legacy",             "clean_recipe_NO_Norway",                 list(g = 4),   "SeedClim database + gradient filter g=4",
  "US_Arizona",          "excel",     "percent",   "legacy",             "clean_recipe_US_Arizona",                list(),          "Individual counts converted to relative cover",
  "IN_Kashmir",          "excel",     "percent",   "legacy",             "clean_recipe_IN_Kashmir",                list(),          "Two raw files bound together; cover-class midpoints",
  "DE_TransAlps",        "mixed",     "biomass",   "legacy",             "clean_recipe_DE_TransAlps",              list(),          "Biomass, no Other cover class",
  "FR_AlpeHuez",         "excel",     "percent",   "legacy",             "clean_recipe_FR_AlpeHuez",               list(),          "Cover-class recoding + date parsing",
  "FR_Lautaret",         "csv",       "percent",   "legacy",             "clean_recipe_FR_Lautaret",               list(),          "Two raw sources (2017-2021 and 2022) bound together",
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
  ),
  CH_Calanda2 = list(
    raw_path = "data/CH_Calanda2/CH_Calanda2_commdata/calanda_data_TransPlantNetwork.csv",
    # Raw file needs a filter (drop focal-individual rows) and a sum-by-group
    # step before it looks like one-row-per-plot-x-species-x-year; reuse the
    # existing, already-correct loader rather than reimplementing it as a
    # raw_format case in import_raw().
    import_fn = "load_cover_CH_Calanda2",
    id_components = c("Year", "originSiteID", "destSiteID", "destPlotID"),
    # Raw Cover here is summed quadrant area in cm2, not a 0-100 percent
    # estimate, so - like CH_Lavey - do NOT synthesize a "Other" row (there's
    # no fixed "100" for it to be a shortfall from); Total_Cover/Rel_Cover are
    # still computed as fractions of each plot's own cm2 total.
    add_other = FALSE,
    non_vascular = c("Moss Group", "Lychen Group", "Mushroom Group", "Cetraria islandica"),
    meta_table = tibble::tribble(
      ~destSiteID, ~Elevation, ~Longitude, ~Latitude,
      "Cal", 2000, 9.48939, 46.88778,
      "Nes", 1400, 9.49013, 46.86923
    ),
    gradient = "CH_Calanda2",
    country = "Switzerland",
    year_established = 2016,
    plot_size_m2 = 1
  ),
  US_Montana = list(
    raw_path = "data/US_Montana/US_Montana_commdata/MT_transplant_relevee_200421.csv",
    # The bespoke bit here isn't the file format (plain csv) but the
    # Treatment/originSiteID derivation (elevation x disturbance-treatment
    # case_when, dropping a "soil" treatment arm not used by this dataset) -
    # reuse the existing loader rather than re-deriving that in
    # standardize_columns()/derive_treatment().
    import_fn = "load_cover_US_Montana",
    # destPlotID is set directly in standardize_columns() (it's
    # originSiteID_destSiteID_turfID, not the usual id_components paste
    # order); UniqueID is Year + that already-assembled destPlotID.
    id_components = c("Year", "destPlotID"),
    non_vascular = c("Other", "Bareground", "Litter", "Moss", "Rock"),
    meta_table = tibble::tribble(
      ~destSiteID, ~Elevation, ~Longitude, ~Latitude,
      "Low", 1985, -111.496672, 45.3089900,
      "High", 2185, -111.49859499, 45.30523699
    ),
    gradient = "US_Montana",
    country = "USA",
    year_established = 2013,
    plot_size_m2 = 0.25
  ),
  SE_Abisko = list(
    raw_path = "data/SE_Abisko/SE_Abisko_commdata/Vegetation data Abisko transplantation experiment (2012 + 2013 + 2014 + 2015)_for Chelsea.xlsx",
    # Needs a specific sheet name, not just "the excel file" - reuse the
    # existing loader rather than adding sheet-name plumbing to import_raw().
    import_fn = "ImportCommunity_SE_Abisko",
    id_components = c("Year", "originSiteID", "destSiteID", "destPlotID"),
    non_vascular = c("Other", "Sph spe"),
    # Treatment isn't a simple 2-level control/warm here: transplants can
    # move either up or down the elevation gradient (3 origins x 3
    # destinations), so it's keyed by the full destSiteID_originSiteID pair
    # rather than a single code.
    treatment_map = c(
      "High_High" = "LocalControl", "Mid_Mid" = "LocalControl", "Low_Low" = "LocalControl",
      "Low_High" = "Warm", "Mid_High" = "Warm", "Low_Mid" = "Warm",
      "Mid_Low" = "Cold", "High_Low" = "Cold", "High_Mid" = "Cold"
    ),
    meta_table = tibble::tribble(
      ~destSiteID, ~Elevation, ~Longitude, ~Latitude,
      "High", 1000, 19.0921899, 68.29267099,
      "Mid", 690, 19.17353299, 68.294066999,
      "Low", 500, 19.190148, 68.300843
    ),
    gradient = "SE_Abisko",
    country = "Sweden",
    year_established = 2012,
    plot_size_m2 = 0.0177
  ),
  DE_Grainau = list(
    raw_path = "data/DE_Grainau/DE_Grainau_commdata/Vegetation 2014-17.xlsx",
    id_components = c("Year", "originSiteID", "destSiteID", "destPlotID"),
    non_vascular = c("Other"),
    meta_table = tibble::tribble(
      ~destSiteID, ~Elevation, ~Longitude, ~Latitude,
      "HIGH", 1714, 11.0617667, 47.4414333,
      "LOW", 773, 11.011217, 47.4761499
    ),
    gradient = "DE_Grainau",
    country = "Germany",
    year_established = 2013,
    plot_size_m2 = 0.25
  ),
  CN_Damxung = list(
    raw_path = "data/CN_Damxung/CN_Damxung_commdata",
    # One xls/xlsx file per year (2013-2018), with a few extra blank/duplicate
    # columns that vary by file - reuse the existing loader rather than
    # adding multi-file globbing to import_raw().
    import_fn = "ImportCommunity_CN_Damxung",
    id_components = c("Year", "originSiteID", "destSiteID", "destPlotID"),
    # Only "Other" (synthesized below via add_other) is treated as
    # non-vascular here; the raw data's one non-plant entry ("others",
    # lowercase) isn't in the legacy filter list either, so - to match the
    # legacy output exactly - it stays in `community`, not `cover`.
    non_vascular = c("Other"),
    meta_table = tibble::tribble(
      ~destSiteID, ~Elevation, ~Longitude, ~Latitude,
      "HIGH", 4800, 91.05491, 30.531410,
      "LOW", 4313, 91.0646299, 30.4971
    ),
    gradient = "CN_Damxung",
    country = "China",
    year_established = 2013,
    plot_size_m2 = 0.25
  ),
  CN_Heibei = list(
    raw_path = "data/CN_Heibei/CN_Heibei_commdata/data to J Ecology.xlsx",
    # destPlotID is assembled directly in standardize_columns() (it's
    # originSiteID_destSiteID_replicate, not just id_components pasted
    # together in the usual order - see the id_components comment in
    # US_Montana above for the same pattern).
    id_components = c("Year", "destPlotID"),
    non_vascular = c("Other"),
    meta_table = tibble::tribble(
      ~destSiteID, ~Elevation, ~Longitude, ~Latitude,
      "3200", 3200, 101.313306, 37.611750,
      "3400", 3400, 101.331306, 37.665306,
      "3800", 3800, 101.36922199, 37.704917
    ),
    gradient = "CN_Heibei",
    country = "China",
    year_established = 2007,
    plot_size_m2 = 1
  ),
  DE_Susalps = list(
    raw_path = "data/DE_Susalps/DE_Susalps_commdata/TransPlantNet_DE_SusAlps_2016-2020.csv",
    # Raw file has multiple harvest dates per year and a "water"/"seed"
    # treatment arm not used here - reuse the existing loader (which already
    # filters to "ctrl" and sums biomass per plot x species x year) rather
    # than adding that filtering/summing to import_raw()/standardize_columns().
    import_fn = "load_cover_DE_Susalps",
    id_components = c("Year", "originSiteID", "destSiteID", "destPlotID"),
    # cover_unit = "biomass" already defaults add_other to FALSE (no fixed
    # "100" for biomass grams to be a shortfall from); non_vascular here is
    # just the two literal cover-class SpeciesName values ("Moss", "Dead
    # biomass") already present in the raw data.
    non_vascular = c("Moss", "Dead biomass"),
    meta_table = tibble::tribble(
      ~destSiteID, ~Elevation, ~Longitude, ~Latitude,
      "BT", 350, 11.581944, 49.921111,
      "FE", 600, 11.066260, 47.829320,
      "GW", 860, 11.031010, 47.569750,
      "EB", 1260, 11.157730, 47.516340
    ),
    gradient = "DE_Susalps",
    country = "Germany",
    year_established = 2016,
    plot_size_m2 = 0.09
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
