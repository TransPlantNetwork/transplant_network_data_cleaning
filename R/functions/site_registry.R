# Site registry: the single source of truth for what differs between sites.
#
# Every site is one row in `site_registry`. Almost all sites have
# `recipe_fn = NA` and are cleaned by the general pipeline functions in
# R/functions/pipeline/ (import_raw, standardize_columns, derive_treatment,
# build_ids, compute_rel_cover, split_cover_classes), configured via
# `site_pipeline_config` below.
#
# Lookup tables that collaborators review line-by-line live as committed
# network-level CSVs under config/ (site_metadata, non_vascular, treatment_map,
# treatment_matrix), each with a site_id column. Structural fields (raw_path,
# import_fn, id_components, ...) stay in site_pipeline_config_base below.
#
# The one exception is US_Arizona (`recipe_fn = clean_recipe_US_Arizona`): its
# community and cover tables come from two independent raw measurements
# (individual counts vs. % green ground cover) that are not subsets of the
# same row set, so they cannot use the shared split_cover_classes() path -
# see R/functions/sites/legacy_recipes.R and the README.
#
# Columns:
#   site_id        - unique site identifier, matches legacy names (e.g. "CH_Lavey")
#   raw_format     - one of "excel", "csv", "csv_delim", "sqlite", "rdata", "mixed"
#   cover_unit     - "percent" (most sites) or "biomass" (no "Other" category added)
#   treatment_rule - one of "site_pair_recode", "turfid_substring", "code_lookup",
#                    "origin_dest_matrix", "turf_code_site", "already_derived",
#                    or "legacy" (legacy = handled entirely by recipe_fn)
#   recipe_fn      - name of a wrapper in R/functions/sites/legacy_recipes.R, or NA
#                    if the site uses the general pipeline
#   recipe_args    - list-column of extra arguments to pass to recipe_fn (unused
#                    for current sites; kept for any future recipe_fn that needs args)
#   notes          - free text

site_registry <- tibble::tribble(
  ~site_id,              ~raw_format, ~cover_unit, ~treatment_rule,       ~recipe_fn,                              ~recipe_args,        ~notes,

  # --- General pipeline (recipe_fn = NA; config in site_pipeline_config) ---
  "CH_Lavey",            "excel",     "percent",   "site_pair_recode",   NA_character_,                            list(),          "Excel; Other/Bare ground already in raw data (add_other = FALSE)",
  "US_Colorado",         "csv",       "percent",   "turfid_substring",   NA_character_,                            list(),          "CSV; treatment from turfID substring",
  "CN_Gongga",           "sqlite",    "percent",   "code_lookup",        NA_character_,                            list(),          "Sqlite; treatment code lookup",
  "CH_Calanda2",         "csv",       "percent",   "already_derived",    NA_character_,                            list(),          "Site x plot-number treatment logic",
  "US_Montana",          "mixed",     "percent",   "already_derived",    NA_character_,                            list(),          "Treatment/originSiteID derived in the raw loader",
  "SE_Abisko",           "excel",     "percent",   "site_pair_recode",   NA_character_,                            list(),          "Wide-format sheet pivoted to long in standardize_columns()",
  "DE_Grainau",          "excel",     "percent",   "turf_code_site",     NA_character_,                            list(),          "Site x code treatment + cover-class midpoint recoding",
  "CN_Damxung",          "excel",     "percent",   "turf_code_site",     NA_character_,                            list(),          "Site x code treatment + cover-class midpoint recoding",
  "CN_Heibei",           "excel",     "percent",   "origin_dest_matrix", NA_character_,                            list(),          "3-way origin x dest treatment matrix incl. Cold",
  "DE_Susalps",          "mixed",     "biomass",   "origin_dest_matrix", NA_character_,                            list(),          "Biomass, origin x dest treatment matrix, no Other class",
  "DE_TransAlps",        "mixed",     "biomass",   "origin_dest_matrix", NA_character_,                            list(),          "Biomass, origin x dest treatment matrix incl. Cold",
  "IN_Kashmir",          "excel",     "percent",   "turf_code_site",     NA_character_,                            list(),          "Site x code treatment + cover-class midpoint recoding",
  "IT_MatschMazia1",     "excel",     "percent",   "already_derived",    NA_character_,                            list(),          "Wide-format species columns + elevation/treat Treatment",
  "IT_MatschMazia2",     "excel",     "percent",   "already_derived",    NA_character_,                            list(),          "Same pattern as MatschMazia1 (1500/1950 m; originControl(s))",
  "CH_Calanda",          "csv",       "percent",   "already_derived",    NA_character_,                            list(),          "veg_away/veg_home Treatment + Cetraria islandica cover class",
  "FR_AlpeHuez",         "excel",     "percent",   "turf_code_site",     NA_character_,                            list(),          "Site x HIGH/LOW_TURF Treatment + Bare ground + date parsing",
  "FR_Lautaret",         "csv",       "percent",   "already_derived",    NA_character_,                            list(),          "Two raw sources (pinpoints + 2022) bound; Warm/Cold/LocalControl",
  "NO_Ulvhaugen",        "sqlite",    "percent",   "code_lookup",        NA_character_,                            list(),          "SeedClim sqlite; TTC/TT2 code lookup; destSiteID filter (Ulv/Alr/Fau)",
  "NO_Lavisdalen",       "sqlite",    "percent",   "code_lookup",        NA_character_,                            list(),          "SeedClim sqlite; TTC/TT2 code lookup; destSiteID filter (Lav/Hog/Vik)",
  "NO_Gudmedalen",       "sqlite",    "percent",   "code_lookup",        NA_character_,                            list(),          "SeedClim sqlite; TTC/TT2 code lookup; destSiteID filter (Gud/Ram/Arh)",
  "NO_Skjellingahaugen", "sqlite",    "percent",   "code_lookup",        NA_character_,                            list(),          "SeedClim sqlite; TTC/TT2 code lookup; destSiteID filter (Skj/Ves/Ovs)",

  # --- Recipe path (not a fit for split_cover_classes; see legacy_recipes.R) ---
  "US_Arizona",          "excel",     "percent",   "legacy",             "clean_recipe_US_Arizona",                list(),          "Individual counts + separate % green cover file; kept on recipe_fn"
)

# --- Site library CSV helpers ------------------------------------------------

#' Network-level site library CSVs tracked as targets file dependencies.
site_library_file_paths <- function() {
  c(
    "config/site_metadata.csv",
    "config/non_vascular.csv",
    "config/treatment_map.csv",
    "config/treatment_matrix.csv"
  )
}

load_site_metadata <- function(site_id, path = "config/site_metadata.csv") {
  tbl <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character")
  required <- c("site_id", "destSiteID", "Elevation", "Longitude", "Latitude")
  missing <- setdiff(required, names(tbl))
  if (length(missing) > 0) {
    stop("Site metadata '", path, "' is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  rows <- tbl[tbl$site_id == site_id, , drop = FALSE]
  if (nrow(rows) == 0) {
    stop("No metadata rows for site '", site_id, "' in '", path, "'", call. = FALSE)
  }
  tibble::tibble(
    destSiteID = as.character(rows$destSiteID),
    Elevation = as.numeric(rows$Elevation),
    Longitude = as.numeric(rows$Longitude),
    Latitude = as.numeric(rows$Latitude)
  )
}

load_site_non_vascular <- function(site_id, path = "config/non_vascular.csv") {
  if (!file.exists(path)) {
    return(NULL)
  }
  tbl <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character")
  required <- c("site_id", "SpeciesName")
  missing <- setdiff(required, names(tbl))
  if (length(missing) > 0) {
    stop("Non-vascular file '", path, "' is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  rows <- tbl[tbl$site_id == site_id, , drop = FALSE]
  if (nrow(rows) == 0) {
    return(NULL)
  }
  as.character(rows$SpeciesName)
}

load_site_treatment_map <- function(site_id, path = "config/treatment_map.csv") {
  if (!file.exists(path)) {
    return(NULL)
  }
  # Keys like "1"/"2" (CN_Gongga) must stay character, not integers.
  tbl <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character")
  required <- c("site_id", "key", "Treatment")
  missing <- setdiff(required, names(tbl))
  if (length(missing) > 0) {
    stop("Treatment map '", path, "' is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  rows <- tbl[tbl$site_id == site_id, , drop = FALSE]
  if (nrow(rows) == 0) {
    return(NULL)
  }
  stats::setNames(as.character(rows$Treatment), as.character(rows$key))
}

load_site_treatment_matrix <- function(site_id, path = "config/treatment_matrix.csv") {
  if (!file.exists(path)) {
    return(NULL)
  }
  tbl <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character")
  required <- c("site_id", "originSiteID", "destSiteID", "Treatment")
  missing <- setdiff(required, names(tbl))
  if (length(missing) > 0) {
    stop("Treatment matrix '", path, "' is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  rows <- tbl[tbl$site_id == site_id, , drop = FALSE]
  if (nrow(rows) == 0) {
    return(NULL)
  }
  tibble::tibble(
    originSiteID = as.character(rows$originSiteID),
    destSiteID = as.character(rows$destSiteID),
    Treatment = as.character(rows$Treatment)
  )
}

#' Attach optional CSV libraries onto a pipeline list for one site.
attach_site_libraries <- function(site_id, pipeline) {
  pipeline$meta_table <- load_site_metadata(site_id)

  non_vascular <- load_site_non_vascular(site_id)
  if (!is.null(non_vascular)) {
    pipeline$non_vascular <- non_vascular
  }

  treatment_map <- load_site_treatment_map(site_id)
  if (!is.null(treatment_map)) {
    pipeline$treatment_map <- treatment_map
  }

  treatment_matrix <- load_site_treatment_matrix(site_id)
  if (!is.null(treatment_matrix)) {
    pipeline$treatment_matrix <- treatment_matrix
  }

  pipeline
}

# --- Pipeline config for sites with recipe_fn == NA --------------------------
# Structural fields only; lookup tables come from config/*.csv (site_id column).

site_pipeline_config_base <- list(
  CH_Lavey = list(
    raw_path = "data/CH_Lavey",
    # Raw layout genuinely differs by year - reuse the existing loader.
    import_fn = "load_cover_CH_Lavey",
    id_components = c("Year", "originSiteID", "destSiteID", "plotID"),
    # Raw data already includes Other/Bare ground/... as real rows.
    add_other = FALSE,
    gradient = "CH_Lavey",
    country = "Switzerland",
    year_established = 2016,
    plot_size_m2 = 1
  ),
  US_Colorado = list(
    raw_path = "data/US_Colorado/US_Colorado_commdata/Final_IntensiveCover_2018-2023_RMBLTransplant.csv",
    id_components = c("Year", "originSiteID", "destSiteID", "destBlockID", "destPlotID"),
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
    gradient = "CN_Gongga",
    country = "China",
    year_established = 2012,
    plot_size_m2 = 0.0625
  ),
  CH_Calanda = list(
    raw_path = "data/CH_Calanda/CH_Calanda_commdata/relevee_database.csv",
    import_fn = "load_cover_CH_Calanda",
    id_components = c("Year", "originSiteID", "destSiteID", "destPlotID"),
    gradient = "CH_Calanda",
    country = "Switzerland",
    year_established = 2012,
    plot_size_m2 = 0.75
  ),
  CH_Calanda2 = list(
    raw_path = "data/CH_Calanda2/CH_Calanda2_commdata/calanda_data_TransPlantNetwork.csv",
    import_fn = "load_cover_CH_Calanda2",
    id_components = c("Year", "originSiteID", "destSiteID", "destPlotID"),
    # Cover is summed quadrant area in cm2, not 0-100 percent.
    add_other = FALSE,
    gradient = "CH_Calanda2",
    country = "Switzerland",
    year_established = 2016,
    plot_size_m2 = 1
  ),
  US_Montana = list(
    raw_path = "data/US_Montana/US_Montana_commdata/MT_transplant_relevee_200421.csv",
    import_fn = "load_cover_US_Montana",
    id_components = c("Year", "destPlotID"),
    gradient = "US_Montana",
    country = "USA",
    year_established = 2013,
    plot_size_m2 = 0.25
  ),
  SE_Abisko = list(
    raw_path = "data/SE_Abisko/SE_Abisko_commdata/Vegetation data Abisko transplantation experiment (2012 + 2013 + 2014 + 2015)_for Chelsea.xlsx",
    import_fn = "ImportCommunity_SE_Abisko",
    id_components = c("Year", "originSiteID", "destSiteID", "destPlotID"),
    gradient = "SE_Abisko",
    country = "Sweden",
    year_established = 2012,
    plot_size_m2 = 0.0177
  ),
  DE_Grainau = list(
    raw_path = "data/DE_Grainau/DE_Grainau_commdata/Vegetation 2014-17.xlsx",
    id_components = c("Year", "originSiteID", "destSiteID", "destPlotID"),
    cover_scale = "grainau_13",
    gradient = "DE_Grainau",
    country = "Germany",
    year_established = 2013,
    plot_size_m2 = 0.25
  ),
  CN_Damxung = list(
    raw_path = "data/CN_Damxung/CN_Damxung_commdata",
    import_fn = "ImportCommunity_CN_Damxung",
    id_components = c("Year", "originSiteID", "destSiteID", "destPlotID"),
    cover_scale = "damxung_10",
    gradient = "CN_Damxung",
    country = "China",
    year_established = 2013,
    plot_size_m2 = 0.25
  ),
  CN_Heibei = list(
    raw_path = "data/CN_Heibei/CN_Heibei_commdata/data to J Ecology.xlsx",
    id_components = c("Year", "destPlotID"),
    gradient = "CN_Heibei",
    country = "China",
    year_established = 2007,
    plot_size_m2 = 1
  ),
  DE_Susalps = list(
    raw_path = "data/DE_Susalps/DE_Susalps_commdata/TransPlantNet_DE_SusAlps_2016-2020.csv",
    import_fn = "load_cover_DE_Susalps",
    id_components = c("Year", "originSiteID", "destSiteID", "destPlotID"),
    gradient = "DE_Susalps",
    country = "Germany",
    year_established = 2016,
    plot_size_m2 = 0.09
  ),
  DE_TransAlps = list(
    raw_path = "data/DE_TransAlps/DE_TransAlps_commdata/TransPlantNet_DACH_TransAlps_2016-2020.csv",
    import_fn = "load_cover_DE_TransAlps",
    id_components = c("Year", "originSiteID", "destSiteID", "destPlotID"),
    gradient = "DE_TransAlps",
    country = "Germany/Switzerland",
    year_established = 2016,
    plot_size_m2 = 0.09
  ),
  IN_Kashmir = list(
    raw_path = "data/IN_Kashmir/IN_Kashmir_commdata",
    import_fn = "ImportCommunity_IN_Kashmir",
    id_components = c("destPlotID", "Year"),
    cover_scale = "kashmir_11",
    gradient = "IN_Kashmir",
    country = "India",
    year_established = 2013,
    plot_size_m2 = 0.25
  ),
  IT_MatschMazia1 = list(
    raw_path = "data/IT_MatschMazia/IT_MatschMazia_commdata/VegData10-13_corr.csv.xlsx",
    import_fn = "load_cover_IT_MatschMazia1",
    id_components = c("Year", "originSiteID", "destSiteID", "destPlotID"),
    gradient = "IT_MatschMazia1",
    country = "Italy",
    year_established = 2010,
    plot_size_m2 = 0.25
  ),
  IT_MatschMazia2 = list(
    raw_path = "data/IT_MatschMazia/IT_MatschMazia_commdata/VegData10-13_corr.csv.xlsx",
    import_fn = "load_cover_IT_MatschMazia2",
    id_components = c("Year", "originSiteID", "destSiteID", "destPlotID"),
    gradient = "IT_MatschMazia2",
    country = "Italy",
    year_established = 2010,
    plot_size_m2 = 0.25
  ),
  FR_AlpeHuez = list(
    raw_path = "data/FR_AlpeHuez/FR_AlpeHuez_commdata/2023-07_MIREN Transplant Experiment_datasheet_140405_AlpeHuezFRANCE.xlsx",
    import_fn = "ImportCommunity_FR_AlpeHuez",
    id_components = c("Year", "destPlotID"),
    gradient = "FR_AlpeHuez",
    country = "France",
    year_established = 2014,
    plot_size_m2 = 0.25
  ),
  FR_Lautaret = list(
    raw_path = "data/FR_Lautaret/FR_Lautaret_commdata",
    import_fn = "load_cover_FR_Lautaret",
    id_components = c("Year", "destPlotID"),
    gradient = "FR_Lautaret",
    country = "France",
    year_established = 2017,
    plot_size_m2 = 1
  ),
  NO_Ulvhaugen = list(
    raw_path = "data/NO_Norway/seedclim.sqlite",
    import_fn = "load_cover_NO_Norway_pipeline",
    sites = c("Ulvhaugen", "Alrust", "Fauske"),
    id_components = c("Year", "originSiteID", "destSiteID", "destPlotID"),
    gradient = "NO_Ulvhaugen",
    country = "Norway",
    year_established = 2009,
    plot_size_m2 = 0.0625
  ),
  NO_Lavisdalen = list(
    raw_path = "data/NO_Norway/seedclim.sqlite",
    import_fn = "load_cover_NO_Norway_pipeline",
    sites = c("Lavisdalen", "Hogsete", "Vikesland"),
    id_components = c("Year", "originSiteID", "destSiteID", "destPlotID"),
    gradient = "NO_Lavisdalen",
    country = "Norway",
    year_established = 2009,
    plot_size_m2 = 0.0625
  ),
  NO_Gudmedalen = list(
    raw_path = "data/NO_Norway/seedclim.sqlite",
    import_fn = "load_cover_NO_Norway_pipeline",
    sites = c("Gudmedalen", "Rambera", "Arhelleren"),
    id_components = c("Year", "originSiteID", "destSiteID", "destPlotID"),
    gradient = "NO_Gudmedalen",
    country = "Norway",
    year_established = 2009,
    plot_size_m2 = 0.0625
  ),
  NO_Skjellingahaugen = list(
    raw_path = "data/NO_Norway/seedclim.sqlite",
    import_fn = "load_cover_NO_Norway_pipeline",
    sites = c("Skjellingahaugen", "Veskre", "Ovstedal"),
    id_components = c("Year", "originSiteID", "destSiteID", "destPlotID"),
    gradient = "NO_Skjellingahaugen",
    country = "Norway",
    year_established = 2009,
    plot_size_m2 = 0.0625
  )
)

site_pipeline_config <- lapply(
  names(site_pipeline_config_base),
  function(site_id) attach_site_libraries(site_id, site_pipeline_config_base[[site_id]])
)
names(site_pipeline_config) <- names(site_pipeline_config_base)

#' Get the full config (registry row + pipeline config, if any) for one site
get_site_config <- function(site_id, registry = site_registry, pipeline_config = site_pipeline_config) {
  row <- registry[registry$site_id == site_id, ]
  if (nrow(row) != 1) stop("site_id '", site_id, "' not found (or duplicated) in site_registry", call. = FALSE)
  cfg <- as.list(row)
  # recipe_args is a list-column where each row holds an argument list for
  # recipe_fn, or an empty list (no extra args). Currently unused.
  cfg$recipe_args <- row$recipe_args[[1]]
  cfg$pipeline <- pipeline_config[[site_id]]
  cfg
}
