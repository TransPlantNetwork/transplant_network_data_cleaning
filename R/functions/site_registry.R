# Site registry: the single source of truth for what differs between sites.
#
# Every site is one row in `site_registry`. Almost all sites have
# `recipe_fn = NA` and are cleaned by the general pipeline functions in
# R/functions/pipeline/ (import_raw, standardize_columns, derive_treatment,
# build_ids, compute_rel_cover, split_cover_classes), configured via
# `site_pipeline_config` below.
#
# The one exception is US_Arizona (`recipe_fn = clean_recipe_US_Arizona`): its
# community and cover tables come from two independent raw measurements
# (individual counts vs. % green ground cover) that are not subsets of the
# same row set, so they cannot use the shared split_cover_classes() path -
# see R/functions/sites/legacy_recipes.R and the README.
#
# The original ImportClean_* scripts in R/functions/ImportData/ are kept so
# migrations can still be checked against the old cleaning output; they are
# not what the pipeline runs for migrated sites.
#
# Columns:
#   site_id        - unique site identifier, matches legacy names (e.g. "CH_Lavey")
#   raw_format     - one of "excel", "csv", "csv_delim", "sqlite", "rdata", "mixed"
#   cover_unit     - "percent" (most sites) or "biomass" (no "Other" category added)
#   treatment_rule - one of "site_pair_recode", "turfid_substring", "code_lookup",
#                    "origin_dest_matrix", "already_derived", or "legacy"
#                    (legacy = handled entirely by recipe_fn)
#   recipe_fn      - name of a wrapper in R/functions/sites/legacy_recipes.R, or NA
#                    if the site uses the general pipeline
#   recipe_args    - list-column of extra arguments to pass to recipe_fn (unused
#                    for current sites; kept for any future recipe_fn that needs args)
#   notes          - free text
#
# Per-site pipeline details (raw_path, id_components, meta_table, ...) live in
# `site_pipeline_config` below, not as tribble columns.

site_registry <- tibble::tribble(
  ~site_id,              ~raw_format, ~cover_unit, ~treatment_rule,       ~recipe_fn,                              ~recipe_args,        ~notes,

  # --- General pipeline (recipe_fn = NA; config in site_pipeline_config) ---
  "CH_Lavey",            "excel",     "percent",   "site_pair_recode",   NA_character_,                            list(),          "Excel; Other/Bare ground already in raw data (add_other = FALSE)",
  "US_Colorado",         "csv",       "percent",   "turfid_substring",   NA_character_,                            list(),          "CSV; treatment from turfID substring",
  "CN_Gongga",           "sqlite",    "percent",   "code_lookup",        NA_character_,                            list(),          "Sqlite; treatment code lookup",
  "CH_Calanda2",         "csv",       "percent",   "already_derived",    NA_character_,                            list(),          "Site x plot-number treatment logic",
  "US_Montana",          "mixed",     "percent",   "already_derived",    NA_character_,                            list(),          "Treatment/originSiteID derived in the raw loader",
  "SE_Abisko",           "excel",     "percent",   "site_pair_recode",   NA_character_,                            list(),          "Wide-format sheet pivoted to long in standardize_columns()",
  "DE_Grainau",          "excel",     "percent",   "already_derived",    NA_character_,                            list(),          "Site x code treatment + cover-class midpoint recoding",
  "CN_Damxung",          "excel",     "percent",   "already_derived",    NA_character_,                            list(),          "Site x code treatment + cover-class midpoint recoding",
  "CN_Heibei",           "excel",     "percent",   "origin_dest_matrix", NA_character_,                            list(),          "3-way origin x dest treatment matrix incl. Cold",
  "DE_Susalps",          "mixed",     "biomass",   "origin_dest_matrix", NA_character_,                            list(),          "Biomass, origin x dest treatment matrix, no Other class",
  "DE_TransAlps",        "mixed",     "biomass",   "origin_dest_matrix", NA_character_,                            list(),          "Biomass, origin x dest treatment matrix incl. Cold",
  "IN_Kashmir",          "excel",     "percent",   "already_derived",    NA_character_,                            list(),          "Site x code treatment + cover-class midpoint recoding",
  "IT_MatschMazia1",     "excel",     "percent",   "already_derived",    NA_character_,                            list(),          "Wide-format species columns + elevation/treat Treatment",
  "IT_MatschMazia2",     "excel",     "percent",   "already_derived",    NA_character_,                            list(),          "Same pattern as MatschMazia1 (1500/1950 m; originControl(s))",
  "CH_Calanda",          "csv",       "percent",   "already_derived",    NA_character_,                            list(),          "veg_away/veg_home Treatment + Cetraria islandica cover class",
  "FR_AlpeHuez",         "excel",     "percent",   "already_derived",    NA_character_,                            list(),          "Site x HIGH/LOW_TURF Treatment + Bare ground + date parsing",
  "FR_Lautaret",         "csv",       "percent",   "already_derived",    NA_character_,                            list(),          "Two raw sources (pinpoints + 2022) bound; Warm/Cold/LocalControl",
  "NO_Ulvhaugen",        "sqlite",    "percent",   "already_derived",    NA_character_,                            list(),          "SeedClim sqlite + destSiteID filter (Ulv/Alr/Fau)",
  "NO_Lavisdalen",       "sqlite",    "percent",   "already_derived",    NA_character_,                            list(),          "SeedClim sqlite + destSiteID filter (Lav/Hog/Vik)",
  "NO_Gudmedalen",       "sqlite",    "percent",   "already_derived",    NA_character_,                            list(),          "SeedClim sqlite + destSiteID filter (Gud/Ram/Arh)",
  "NO_Skjellingahaugen", "sqlite",    "percent",   "already_derived",    NA_character_,                            list(),          "SeedClim sqlite + destSiteID filter (Skj/Ves/Ovs)",

  # --- Recipe path (not a fit for split_cover_classes; see legacy_recipes.R) ---
  "US_Arizona",          "excel",     "percent",   "legacy",             "clean_recipe_US_Arizona",                list(),          "Individual counts + separate % green cover file; kept on recipe_fn"
)

# --- Pipeline config for sites with recipe_fn == NA ---
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
  CH_Calanda = list(
    raw_path = "data/CH_Calanda/CH_Calanda_commdata/relevee_database.csv",
    # Loader averages Cov_Rel1/Cov_Rel2 per plot x species x year and drops
    # NA/"NF" cells - reuse rather than reimplementing that in import_raw().
    import_fn = "load_cover_CH_Calanda",
    id_components = c("Year", "originSiteID", "destSiteID", "destPlotID"),
    # Real lichen row in the raw data (not just synthetic Other); split into
    # the cover table alongside Other.
    non_vascular = c("Cetraria islandica"),
    meta_table = tibble::tribble(
      ~destSiteID, ~Elevation, ~Longitude, ~Latitude,
      "Pea", 2800, 9.47031, 46.89326,
      "Cal", 2000, 9.48939, 46.88778,
      "Nes", 1400, 9.49013, 46.86923
    ),
    gradient = "CH_Calanda",
    country = "Switzerland",
    year_established = 2012,
    plot_size_m2 = 0.75
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
    # Origin x dest elevation matrix (includes Cold: origin moved uphill).
    # Consumed by derive_treatment_origin_dest_matrix().
    treatment_matrix = tibble::tribble(
      ~originSiteID, ~destSiteID, ~Treatment,
      "3200", "3200", "LocalControl",
      "3400", "3400", "LocalControl",
      "3800", "3800", "LocalControl",
      "3400", "3200", "Warm",
      "3800", "3200", "Warm",
      "3800", "3400", "Warm",
      "3200", "3400", "Cold",
      "3200", "3800", "Cold",
      "3400", "3800", "Cold"
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
    # Origin x dest elevation matrix; all transplants go downhill or stay
    # (BT < FE < GW < EB), so there is no Cold. Consumed by
    # derive_treatment_origin_dest_matrix().
    treatment_matrix = tibble::tribble(
      ~originSiteID, ~destSiteID, ~Treatment,
      "BT", "BT", "LocalControl",
      "FE", "FE", "LocalControl",
      "GW", "GW", "LocalControl",
      "EB", "EB", "LocalControl",
      "EB", "BT", "Warm",
      "EB", "FE", "Warm",
      "EB", "GW", "Warm",
      "FE", "BT", "Warm",
      "GW", "BT", "Warm",
      "GW", "FE", "Warm"
    ),
    gradient = "DE_Susalps",
    country = "Germany",
    year_established = 2016,
    plot_size_m2 = 0.09
  ),
  DE_TransAlps = list(
    raw_path = "data/DE_TransAlps/DE_TransAlps_commdata/TransPlantNet_DACH_TransAlps_2016-2020.csv",
    # Same reasoning as DE_Susalps: reuse the existing loader (filters to
    # "ctrl", sums biomass per plot x species x year across harvest dates).
    import_fn = "load_cover_DE_TransAlps",
    id_components = c("Year", "originSiteID", "destSiteID", "destPlotID"),
    non_vascular = c("Moss", "Dead biomass"),
    meta_table = tibble::tribble(
      ~destSiteID, ~Elevation, ~Longitude, ~Latitude,
      "BT", 300, 11.581944, 49.921111,
      "SP", 1850, 11.305278, 47.128889,
      "FP", 2440, 8.421389, 46.576667
    ),
    # Origin x dest elevation matrix including Cold (BT origin moved uphill
    # to FP/SP). Consumed by derive_treatment_origin_dest_matrix().
    treatment_matrix = tibble::tribble(
      ~originSiteID, ~destSiteID, ~Treatment,
      "BT", "BT", "LocalControl",
      "FP", "FP", "LocalControl",
      "SP", "SP", "LocalControl",
      "FP", "BT", "Warm",
      "SP", "BT", "Warm",
      "BT", "FP", "Cold",
      "BT", "SP", "Cold"
    ),
    gradient = "DE_TransAlps",
    country = "Germany/Switzerland",
    year_established = 2016,
    plot_size_m2 = 0.09
  ),
  IN_Kashmir = list(
    raw_path = "data/IN_Kashmir/IN_Kashmir_commdata",
    # Two excel files (2014, 2015) with fixed cell ranges (to work around
    # spreadsheet drag errors in the raw data) bound together - reuse the
    # existing loader rather than adding multi-file/range support to
    # import_raw().
    import_fn = "ImportCommunity_IN_Kashmir",
    # destPlotID is assembled directly in standardize_columns() as
    # originSiteID_destSiteID_destBlockID; UniqueID is destPlotID + Year
    # (note: reverse order from most other sites) to match the legacy
    # paste(destPlotID, Year) exactly.
    id_components = c("destPlotID", "Year"),
    meta_table = tibble::tribble(
      ~destSiteID, ~Elevation, ~Longitude, ~Latitude,
      "HIGH", 2684, 74.39961099, 34.050736,
      "LOW", 1951, 74.832931, 34.13218899
    ),
    gradient = "IN_Kashmir",
    country = "India",
    year_established = 2013,
    plot_size_m2 = 0.25
  ),
  IT_MatschMazia1 = list(
    raw_path = "data/IT_MatschMazia/IT_MatschMazia_commdata/VegData10-13_corr.csv.xlsx",
    # Sheet 1 of a two-sheet excel file (gradient 1 = 1000/1500 m); loader
    # also derives year from the trailing digits of `internal ID`.
    import_fn = "load_cover_IT_MatschMazia1",
    # destPlotID is extracted from the raw UniqueID (strip trailing _YY) in
    # standardize_columns(); UniqueID is then Year_origin_dest_destPlotID.
    id_components = c("Year", "originSiteID", "destSiteID", "destPlotID"),
    meta_table = tibble::tribble(
      ~destSiteID, ~Elevation, ~Longitude, ~Latitude,
      "Low", 1000, 10.5902491243, 46.6612188656,
      "High", 1500, 10.5797899, 46.6862599
    ),
    gradient = "IT_MatschMazia1",
    country = "Italy",
    year_established = 2010,
    plot_size_m2 = 0.25
  ),
  IT_MatschMazia2 = list(
    raw_path = "data/IT_MatschMazia/IT_MatschMazia_commdata/VegData10-13_corr.csv.xlsx",
    # Sheet 2 of the same file (gradient 2 = 1500/1950 m).
    import_fn = "load_cover_IT_MatschMazia2",
    id_components = c("Year", "originSiteID", "destSiteID", "destPlotID"),
    meta_table = tibble::tribble(
      ~destSiteID, ~Elevation, ~Longitude, ~Latitude,
      "Low", 1500, 10.5797899, 46.6862599,
      "High", 1950, 10.59195399, 46.6916840
    ),
    gradient = "IT_MatschMazia2",
    country = "Italy",
    year_established = 2010,
    plot_size_m2 = 0.25
  ),
  FR_AlpeHuez = list(
    raw_path = "data/FR_AlpeHuez/FR_AlpeHuez_commdata/2023-07_MIREN Transplant Experiment_datasheet_140405_AlpeHuezFRANCE.xlsx",
    import_fn = "ImportCommunity_FR_AlpeHuez",
    # destPlotID = originSiteID_destSiteID_plotID assembled in
    # standardize_columns(); UniqueID is Year + that destPlotID.
    id_components = c("Year", "destPlotID"),
    non_vascular = c("Bare ground"),
    meta_table = tibble::tribble(
      ~destSiteID, ~Elevation, ~Longitude, ~Latitude,
      "HIGH", 2072, 6.0554500, 45.0999830,
      "LOW", 1481, 6.035933, 45.08820
    ),
    gradient = "FR_AlpeHuez",
    country = "France",
    year_established = 2014,
    plot_size_m2 = 0.25
  ),
  FR_Lautaret = list(
    raw_path = "data/FR_Lautaret/FR_Lautaret_commdata",
    # Two differently shaped CSVs (pinpoint counts 2017-2021; percent cover
    # 2022) cleaned separately then bound - see load_cover_FR_Lautaret().
    import_fn = "load_cover_FR_Lautaret",
    id_components = c("Year", "destPlotID"),
    meta_table = tibble::tribble(
      ~destSiteID, ~Elevation, ~Longitude, ~Latitude,
      "G", 2450, 6.40048, 45.0543600,
      "L", 1950, 6.4190699, 45.04006
    ),
    gradient = "FR_Lautaret",
    country = "France",
    year_established = 2017,
    plot_size_m2 = 1
  ),
  # Shared SeedClim sqlite load for all four NO gradients; each filters to
  # its three destSiteIDs via pipeline$sites in standardize_columns().
  NO_Ulvhaugen = list(
    raw_path = "data/NO_Norway/seedclim.sqlite",
    import_fn = "load_cover_NO_Norway_pipeline",
    sites = c("Ulvhaugen", "Alrust", "Fauske"),
    id_components = c("Year", "originSiteID", "destSiteID", "destPlotID"),
    meta_table = tibble::tribble(
      ~destSiteID, ~Elevation, ~Longitude, ~Latitude,
      "Ulvhaugen", 1208, 8.12343, 61.0243,
      "Alrust", 815, 8.70466, 60.8203,
      "Fauske", 589, 9.07876, 61.0355
    ),
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
    meta_table = tibble::tribble(
      ~destSiteID, ~Elevation, ~Longitude, ~Latitude,
      "Lavisdalen", 1097, 7.27596, 60.8231,
      "Hogsete", 700, 7.17666, 60.876,
      "Vikesland", 474, 7.16982, 60.8803
    ),
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
    meta_table = tibble::tribble(
      ~destSiteID, ~Elevation, ~Longitude, ~Latitude,
      "Gudmedalen", 1213, 7.17561, 60.8328,
      "Rambera", 769, 6.63028, 61.0866,
      "Arhelleren", 431, 6.33738, 60.6652
    ),
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
    meta_table = tibble::tribble(
      ~destSiteID, ~Elevation, ~Longitude, ~Latitude,
      "Skjellingahaugen", 1088, 6.41504, 60.9335,
      "Veskre", 797, 6.51468, 60.5445,
      "Ovstedal", 346, 5.96487, 60.6901
    ),
    gradient = "NO_Skjellingahaugen",
    country = "Norway",
    year_established = 2009,
    plot_size_m2 = 0.0625
  )
)

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
