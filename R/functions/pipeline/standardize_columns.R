# Rename raw, site-specific column names to the canonical schema names.
# For pilot sites the small amount of site-specific renaming is done inline
# here via a `switch`, mirroring the "column_map" idea from the plan: it is
# explicit, inspectable, and isolated to this one function rather than
# scattered across a whole cleaning script.

standardize_columns <- function(raw, site_cfg) {
  switch(site_cfg$site_id,
    CH_Lavey = raw |>
      dplyr::rename(Cover = cover, Year = year, plotID = turfID) |>
      dplyr::mutate(Cover = as.numeric(Cover)) |>
      tidyr::separate(siteID, c("destSiteID", "originSiteID"), sep = "_"),
    US_Colorado = raw |>
      dplyr::select(year, turfID, species, percentCover) |>
      # 27 rows (all in 2023) have no turfID (or any other plot identifier) in
      # the raw file at all - a genuine gap in that year's raw data, not
      # something recoverable from other columns. Drop them rather than let
      # them silently collapse into one bogus "NA" plot; worth following up
      # with the data provider about what plot(s) they belong to.
      dplyr::filter(!is.na(turfID), turfID != "") |>
      dplyr::rename(SpeciesName = species, Cover = percentCover, Year = year, destPlotID = turfID) |>
      dplyr::mutate(
        Year = as.numeric(Year),
        Cover = as.numeric(Cover),
        destSiteID = substr(destPlotID, 1, 2),
        destBlockID = substr(destPlotID, 3, 3),
        treatment_code = substr(destPlotID, 7, 8),
        originSiteID = substr(destPlotID, nchar(destPlotID) - 4, nchar(destPlotID) - 3),
        originBlockID = substr(destPlotID, nchar(destPlotID) - 2, nchar(destPlotID) - 2)
      ),
    CN_Gongga = raw |>
      dplyr::filter(TTtreat != "OTC") |>
      dplyr::rename(Year = year, treatment_code = TTtreat, Cover = cover, SpeciesName = speciesName) |>
      dplyr::mutate(
        SpeciesName = dplyr::recode(SpeciesName, "Potentilla stenophylla var. emergens" = "Potentilla stenophylla")
      ) |>
      dplyr::filter(!is.na(Cover), Cover != 0),
    # Treatment depends jointly on `site` and `plot` (not a single code
    # column), so it - and the originSiteID it implies - are derived here
    # directly rather than via a treatment_rule; see
    # derive_treatment_already_derived() in derive_treatment.R. Plot numbers
    # 1/3 at "Nes" are the warmed transplants, 8/9 at "Nes" are the local
    # controls moved back to their own site; "Cal" plots are all
    # LocalControl (never moved).
    CH_Calanda2 = raw |>
      dplyr::mutate(
        originSiteID = dplyr::case_when(
          site == "Cal" ~ "Cal",
          plot %in% c(1, 3) & site == "Nes" ~ "Cal",
          plot %in% c(8, 9) & site == "Nes" ~ "Nes"
        ),
        Treatment = dplyr::case_when(
          plot %in% c(1, 3) & site == "Nes" ~ "Warm",
          plot %in% c(8, 9) & site == "Nes" ~ "LocalControl",
          site == "Cal" ~ "LocalControl"
        )
      ) |>
      dplyr::select(-plot) |>
      dplyr::rename(destSiteID = site, Cover = cover, Year = year, SpeciesName = species, destPlotID = plot_id, destBlockID = block) |>
      dplyr::mutate(Cover = as.numeric(Cover), destPlotID = as.character(destPlotID), destBlockID = as.character(destBlockID)) |>
      dplyr::filter(!is.na(Cover)),
    # Treatment/originSiteID are already derived by the bespoke raw loader
    # (load_cover_US_Montana(), reused here as site_cfg$pipeline$import_fn -
    # see the site_registry note "Most cleaning happens in the loader
    # script"); only destPlotID needs assembling here (it's
    # originSiteID_destSiteID_turfID, not just id_components pasted together
    # in the usual order - see the id_components comment in site_registry.R).
    # A few non-vascular cover-class names are inconsistently
    # cased/spelled in the raw sheet (e.g. "bareground" vs "Bare"); recode
    # them to one canonical spelling per class up front so the generic
    # split_cover_classes() ends up with one cover row per class per plot,
    # not one per raw spelling variant.
    US_Montana = raw |>
      dplyr::mutate(
        Cover = as.numeric(Cover),
        SpeciesName = dplyr::recode(SpeciesName, bareground = "Bareground", Bare = "Bareground", litter = "Litter", moss = "Moss", rock = "Rock"),
        destPlotID = paste(originSiteID, destSiteID, turfID, sep = "_")
      ) |>
      dplyr::filter(!is.na(Cover)) |>
      dplyr::select(-turfID, -Region),
    # Raw sheet is wide (one column per species) rather than one row per
    # observation, so it needs a select() (dropping ID/metadata columns and
    # the "Bare soil".."Mosses" range - a block of non-vascular columns not
    # used at this site at all, per the original code's comment about
    # dropping most moss/lichen data) and a pivot_longer() to long format
    # before it looks like every other site's raw data. Treatment is dropped
    # here too; the raw column doesn't distinguish Warm from Cold (both
    # directions of elevation transplant), so the canonical Treatment is
    # derived from the origin/destination elevation pair instead
    # (site_pair_recode rule, treatment_map in site_pipeline_config).
    SE_Abisko = raw |>
      dplyr::select(-c(El, Ori, Yr, `Spot ID`, Tag, `Bare soil`:Mosses, Treatment)) |>
      dplyr::rename(originSiteID = `Elevation of origin`, destSiteID = `Transplant elevation`, destBlockID = Block, destPlotID = `Core ID`) |>
      # Species columns aren't all the same type (readxl infers some as
      # logical when a column happens to be all-NA), so cast to character
      # before pivoting - pivot_longer() errors on mismatched column types
      # where gather() used to silently coerce.
      dplyr::mutate(dplyr::across(-c(destSiteID, originSiteID, destPlotID, destBlockID, Year), as.character)) |>
      tidyr::pivot_longer(
        cols = -c(destSiteID, originSiteID, destPlotID, destBlockID, Year),
        names_to = "SpeciesName", values_to = "Cover"
      ) |>
      dplyr::mutate(Cover = as.numeric(Cover), destPlotID = as.character(destPlotID), destBlockID = as.character(destBlockID)),
    # Treatment is a site x code combination (same pattern as CH_Calanda2/
    # US_Montana), so it - and originSiteID - are derived here directly; see
    # derive_treatment_already_derived() in derive_treatment.R. Raw Cover is
    # a 13-level cover class, not a percent, so it's converted to its
    # class-midpoint percent here (same conversion the legacy code used).
    DE_Grainau = raw |>
      dplyr::rename(
        destSiteID = site, destBlockID = block, destPlotID = plot.ID,
        Treatment = treatment, Year = year, SpeciesName = species.name, Cover = cover.class
      ) |>
      dplyr::mutate(
        originSiteID = toupper(sub("(.*)_.*", "\\1", Treatment)),
        Treatment = dplyr::case_when(
          Treatment == "low_turf" & destSiteID == "LOW" ~ "LocalControl",
          Treatment == "high_turf" & destSiteID == "LOW" ~ "Warm",
          Treatment == "high_turf" & destSiteID == "HIGH" ~ "LocalControl"
        ),
        Cover = dplyr::recode(Cover,
          `1` = 0.5, `2` = 1, `3` = 3.5, `4` = 8, `5` = 15.5, `6` = 25.5, `7` = 35.5,
          `8` = 45.5, `9` = 55.5, `10` = 65.5, `11` = 75.5, `12` = 85.5, `13` = 95.5
        ),
        destPlotID = as.character(destPlotID), destBlockID = as.character(destBlockID)
      ) |>
      dplyr::filter(!is.na(Cover)),
    # Same site x code Treatment derivation + cover-class midpoint recoding
    # pattern as DE_Grainau, just with a different (10-level) cover-class
    # scale and a multi-file (one xls/xlsx per year) raw layout - reuse the
    # existing loader (site_pipeline_config$import_fn) rather than adding
    # multi-file globbing to import_raw().
    CN_Damxung = raw |>
      dplyr::rename(
        destSiteID = SITE, destBlockID = BLOCK, destPlotID = PLOT.ID,
        Treatment = TREATMENT, Year = YEAR, SpeciesName = `Species name`, Cover = `cover class`
      ) |>
      dplyr::mutate(
        originSiteID = toupper(sub("(.*)_.*", "\\1", Treatment)),
        Treatment = dplyr::case_when(
          Treatment == "low_turf" & destSiteID == "LOW" ~ "LocalControl",
          Treatment == "high_turf" & destSiteID == "LOW" ~ "Warm",
          Treatment == "high_turf" & destSiteID == "HIGH" ~ "LocalControl"
        ),
        Cover = dplyr::recode(Cover,
          `1` = 0.5, `2` = 1, `3` = 3.5, `4` = 8, `5` = 15.5, `6` = 25.5, `7` = 35.5,
          `8` = 45.5, `9` = 55.5, `10` = 80
        ),
        destPlotID = as.character(destPlotID), destBlockID = as.character(destBlockID)
      ) |>
      dplyr::filter(!is.na(Cover)),
    # Treatment is a 3x3 origin x destination elevation matrix (unlike
    # CH_Calanda2/DE_Grainau/CN_Damxung's 2-level warm/local-control, this
    # one includes "Cold" for transplants moved to a lower/warmer site than
    # their origin) - derived here directly rather than via a treatment_rule.
    # Raw Coverage(%) is already a percent (no cover-class recoding needed).
    # destSiteID/originSiteID == 3600 (a site not used in this experiment;
    # only present as noise in a few rows) are dropped.
    CN_Heibei = raw |>
      dplyr::rename(destSiteID = away, originSiteID = home, Year = year, SpeciesName = species, Cover = `Coverage(%)`) |>
      dplyr::filter(destSiteID != 3600, originSiteID != 3600) |>
      dplyr::mutate(
        destSiteID = as.character(destSiteID), originSiteID = as.character(originSiteID),
        Treatment = dplyr::case_when(
          destSiteID == "3200" & originSiteID == "3200" ~ "LocalControl",
          destSiteID == "3400" & originSiteID == "3400" ~ "LocalControl",
          destSiteID == "3800" & originSiteID == "3800" ~ "LocalControl",
          destSiteID == "3200" & originSiteID == "3400" ~ "Warm",
          destSiteID == "3200" & originSiteID == "3800" ~ "Warm",
          destSiteID == "3400" & originSiteID == "3800" ~ "Warm",
          destSiteID == "3400" & originSiteID == "3200" ~ "Cold",
          destSiteID == "3800" & originSiteID == "3200" ~ "Cold",
          destSiteID == "3800" & originSiteID == "3400" ~ "Cold"
        ),
        destPlotID = paste(originSiteID, destSiteID, replicate, sep = "_")
      ) |>
      dplyr::select(-replicate) |>
      dplyr::filter(!is.na(Cover)) |>
      # Raw data has ~9 fully-duplicated rows (same plot/species/Cover
      # entered twice) - drop those exact duplicates before
      # collapse_duplicate_species() sums any *genuine* repeated
      # observations (e.g. a species re-identified partway through), so a
      # single duplicated row doesn't get double-counted as if it were two
      # separate observations.
      dplyr::distinct(),
    # originSiteID/destSiteID already come out of the raw loader
    # (load_cover_DE_Susalps, reused as site_pipeline_config$import_fn),
    # which also already sums biomass per plot x species x year (multiple
    # harvest dates, "ctrl" treatment only) - only Treatment needs deriving
    # here, from the same kind of origin x destination elevation matrix as
    # CN_Heibei, but 4 sites and no "Cold" (all transplants go to equal or
    # higher elevation: BT < FE < GW < EB).
    DE_Susalps = raw |>
      dplyr::rename(Cover = biomass, Year = year, destPlotID = turfID) |>
      dplyr::mutate(
        Treatment = dplyr::case_when(
          originSiteID == "BT" & destSiteID == "BT" ~ "LocalControl",
          originSiteID == "EB" & destSiteID == "BT" ~ "Warm",
          originSiteID == "EB" & destSiteID == "EB" ~ "LocalControl",
          originSiteID == "EB" & destSiteID == "FE" ~ "Warm",
          originSiteID == "EB" & destSiteID == "GW" ~ "Warm",
          originSiteID == "FE" & destSiteID == "BT" ~ "Warm",
          originSiteID == "FE" & destSiteID == "FE" ~ "LocalControl",
          originSiteID == "GW" & destSiteID == "BT" ~ "Warm",
          originSiteID == "GW" & destSiteID == "FE" ~ "Warm",
          originSiteID == "GW" & destSiteID == "GW" ~ "LocalControl"
        ),
        destPlotID = as.character(destPlotID)
      ) |>
      dplyr::filter(!is.na(Cover)),
    stop("standardize_columns(): no column mapping defined for site '", site_cfg$site_id, "'")
  )
}
