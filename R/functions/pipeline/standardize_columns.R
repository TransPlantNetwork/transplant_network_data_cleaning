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
    # Same pattern as DE_Susalps (biomass, origin x dest treatment matrix,
    # reuses the existing loader as import_fn), but 3 sites and does include
    # "Cold" (BT origin, low elevation, transplanted up to FP/SP).
    DE_TransAlps = raw |>
      dplyr::rename(Cover = biomass, Year = year, destPlotID = turfID) |>
      dplyr::mutate(
        Treatment = dplyr::case_when(
          originSiteID == "BT" & destSiteID == "BT" ~ "LocalControl",
          originSiteID == "BT" & destSiteID == "FP" ~ "Cold",
          originSiteID == "BT" & destSiteID == "SP" ~ "Cold",
          originSiteID == "FP" & destSiteID == "BT" ~ "Warm",
          originSiteID == "FP" & destSiteID == "FP" ~ "LocalControl",
          originSiteID == "SP" & destSiteID == "BT" ~ "Warm",
          originSiteID == "SP" & destSiteID == "SP" ~ "LocalControl"
        ),
        destPlotID = as.character(destPlotID)
      ) |>
      dplyr::filter(!is.na(Cover)),
    # Same site x code Treatment derivation + cover-class midpoint recoding
    # pattern as DE_Grainau/CN_Damxung, with a couple of extra site-specific
    # fixes: a handful of misspelled/inconsistent species names, and (like
    # CN_Heibei) a few exact-duplicate raw rows to distinct() out before
    # collapse_duplicate_species() sums genuine repeats. Reuses the existing
    # loader (two excel files, 2014 and 2015, with fixed cell ranges to work
    # around spreadsheet drag errors in the raw data) as import_fn.
    IN_Kashmir = raw |>
      dplyr::rename(
        destSiteID = SITE, destBlockID = BLOCK, Treatment = TREATMENT,
        Year = YEAR, SpeciesName = `Species name`, Cover = `cover class`
      ) |>
      dplyr::mutate(
        SpeciesName = dplyr::recode(SpeciesName,
          "Fragaria spp" = "Fragaria sp.", "Ranunculus spp" = "Ranunculus sp.",
          "Pinus spp" = "Pinus sp.", "CYANODON dACTYLON" = "Cyanodon dactylon",
          "Hordeum spp" = "Hordeum sp.", "Rubus spp" = "Rubus sp.",
          "Cyanodondactylon" = "Cyanodon dactylon"
        ),
        originSiteID = toupper(sub("(.*)_.*", "\\1", Treatment)),
        Treatment = dplyr::case_when(
          Treatment == "low_turf" & destSiteID == "LOW" ~ "LocalControl",
          Treatment == "high_turf" & destSiteID == "LOW" ~ "Warm",
          Treatment == "high_turf" & destSiteID == "HIGH" ~ "LocalControl"
        ),
        Cover = dplyr::recode(Cover,
          `1` = 0.5, `2` = 1, `3` = 3.5, `4` = 8, `5` = 15.5, `6` = 25.5, `7` = 35.5,
          `8` = 45.5, `9` = 55.5, `10` = 70, `11` = 90
        ),
        destBlockID = as.character(destBlockID),
        destPlotID = paste(originSiteID, destSiteID, destBlockID, sep = "_")
      ) |>
      dplyr::filter(!is.na(Cover)) |>
      dplyr::distinct(),
    # Wide-format (species as columns), like SE_Abisko. Elevation encodes
    # destSiteID (1000=Low, 1500=High); treat encodes Treatment
    # (destControl/originControls=LocalControl, warmed=Warm). originSiteID
    # depends on both. Raw UniqueID is plotID_YY - extract destPlotID by
    # stripping the trailing year token, then rebuild UniqueID via
    # id_components (Year_origin_dest_destPlotID) so it includes Year (the
    # UniqueID-without-Year bug that previously broke Rel_Cover sums is
    # documented in the legacy ImportClean script). Underscores in species
    # column names become spaces. Reuses load_cover_IT_MatschMazia1 as
    # import_fn.
    IT_MatschMazia1 = {
      id_cols <- c("UniqueID", "Elevation", "treat", "Year")
      raw |>
        dplyr::rename(Year = year, Elevation = elevation) |>
        dplyr::mutate(dplyr::across(-dplyr::all_of(id_cols), as.character)) |>
        tidyr::pivot_longer(
          cols = -dplyr::all_of(id_cols),
          names_to = "SpeciesName",
          values_to = "Cover"
        ) |>
        dplyr::mutate(Cover = as.numeric(Cover)) |>
        dplyr::filter(!is.na(Cover)) |>
        dplyr::mutate(
          SpeciesName = gsub("_", " ", SpeciesName, fixed = TRUE),
          destSiteID = dplyr::case_when(
            Elevation == 1000 ~ "Low",
            Elevation == 1500 ~ "High"
          ),
          Treatment = dplyr::case_when(
            treat == "destControl" ~ "LocalControl",
            treat == "originControls" ~ "LocalControl",
            treat == "warmed" ~ "Warm"
          ),
          originSiteID = dplyr::case_when(
            Elevation == 1000 & treat == "destControl" ~ "Low",
            Elevation == 1500 ~ "High",
            treat == "warmed" ~ "High"
          ),
          destPlotID = sub("_[^_]+$", "", UniqueID),
          destBlockID = NA_character_
        ) |>
        dplyr::select(
          Year, destSiteID, originSiteID, destPlotID, destBlockID,
          Treatment, SpeciesName, Cover
        )
    },
    # Same wide-format pattern as IT_MatschMazia1 (sheet 2 of the same
    # excel file via load_cover_IT_MatschMazia2), but elevations are
    # 1500=Low / 1950=High, and the raw treat labels are originControl /
    # originControls (both LocalControl - a known labeling inconsistency
    # in the raw data) and warmed. originSiteID is derived from the
    # already-computed Treatment (as in the legacy script), not from treat.
    IT_MatschMazia2 = {
      id_cols <- c("UniqueID", "Elevation", "treat", "Year")
      raw |>
        dplyr::rename(Year = year, Elevation = elevation) |>
        dplyr::mutate(dplyr::across(-dplyr::all_of(id_cols), as.character)) |>
        tidyr::pivot_longer(
          cols = -dplyr::all_of(id_cols),
          names_to = "SpeciesName",
          values_to = "Cover"
        ) |>
        dplyr::mutate(Cover = as.numeric(Cover)) |>
        dplyr::filter(!is.na(Cover)) |>
        dplyr::mutate(
          SpeciesName = gsub("_", " ", SpeciesName, fixed = TRUE),
          destSiteID = dplyr::case_when(
            Elevation == 1500 ~ "Low",
            Elevation == 1950 ~ "High"
          ),
          Treatment = dplyr::case_when(
            treat == "originControl" ~ "LocalControl",
            treat == "originControls" ~ "LocalControl",
            treat == "warmed" ~ "Warm"
          ),
          originSiteID = dplyr::case_when(
            Elevation == 1500 & Treatment == "LocalControl" ~ "Low",
            Elevation == 1950 ~ "High",
            Treatment == "Warm" ~ "High"
          ),
          destPlotID = sub("_[^_]+$", "", UniqueID),
          destBlockID = NA_character_
        ) |>
        dplyr::select(
          Year, destSiteID, originSiteID, destPlotID, destBlockID,
          Treatment, SpeciesName, Cover
        )
    },
    # Treatment depends jointly on raw Treatment (veg_away/veg_home) and Site
    # (Cal/Nes/Pea), so it is derived here rather than via a treatment_rule.
    # originSiteID is the raw turf_type column. Drops a bogus plot id
    # ("CalNA.NA") and exact-duplicate raw rows (same two plots noted in the
    # legacy script) via distinct() before collapse_duplicate_species().
    # Cetraria islandica is a real non-vascular row (configured in
    # non_vascular); synthetic Other is still added (default for percent).
    # Reuses load_cover_CH_Calanda as import_fn.
    CH_Calanda = raw |>
      dplyr::ungroup() |>
      dplyr::mutate(
        Treatment = dplyr::case_when(
          Treatment == "veg_away" & Site %in% c("Cal", "Nes") ~ "Warm",
          Treatment == "veg_home" & Site %in% c("Nes", "Pea", "Cal") ~ "LocalControl"
        )
      ) |>
      dplyr::rename(
        destSiteID = Site, originSiteID = turf_type, Year = year,
        SpeciesName = Species_Name, destPlotID = plot_id, destBlockID = Block
      ) |>
      dplyr::filter(Treatment %in% c("LocalControl", "Warm")) |>
      dplyr::mutate(
        destPlotID = as.character(destPlotID),
        destBlockID = as.character(destBlockID)
      ) |>
      dplyr::filter(destPlotID != "CalNA.NA") |>
      dplyr::select(
        Year, Treatment, originSiteID, destSiteID, destBlockID, destPlotID,
        SpeciesName, Cover
      ) |>
      dplyr::distinct(),
    # Same site x HIGH_TURF/LOW_TURF Treatment pattern as IN_Kashmir/DE_Grainau.
    # Year is parsed from a messy Date column (Excel serials, m/d/y, ISO).
    # Cover "+" recoded to 0.5; species names truncated to genus + epithet.
    # Bare ground is a real non-vascular class; synthetic Other still added.
    # distinct() then collapse_duplicate_species() match the legacy
    # distinct + group_by/sum. Reuses ImportCommunity_FR_AlpeHuez as import_fn.
    FR_AlpeHuez = raw |>
      dplyr::select(site:cover.class, -plot, -species.name) |>
      dplyr::rename(
        SpeciesName = `corrected name`, Cover = cover.class,
        destSiteID = site, destBlockID = block, plotID = plot.ID,
        Treatment = treatment, Date = date
      ) |>
      dplyr::mutate(
        SpeciesName = sub("^(\\S*\\s+\\S+).*", "\\1", SpeciesName)
      ) |>
      dplyr::filter(Treatment %in% c("HIGH_TURF", "LOW_TURF")) |>
      dplyr::mutate(
        Date = dplyr::case_when(
          stringr::str_detect(Date, "^\\d+$") ~ as.Date(as.numeric(Date), origin = "1899-12-30"),
          stringr::str_detect(Date, "^\\d{1,2}/\\d{1,2}/\\d{4}$") ~ lubridate::mdy(Date),
          stringr::str_detect(Date, "^\\d{1,2}/\\d{1,2}/\\d{2}$") ~ lubridate::mdy(Date),
          stringr::str_detect(Date, "^\\d{4}-\\d{2}-\\d{2}$") ~ lubridate::ymd(Date),
          TRUE ~ as.Date(NA)
        ),
        originSiteID = toupper(sub("(.*)_.*", "\\1", Treatment)),
        Treatment = dplyr::case_when(
          Treatment == "LOW_TURF" & destSiteID == "LOW" ~ "LocalControl",
          Treatment == "HIGH_TURF" & destSiteID == "LOW" ~ "Warm",
          Treatment == "HIGH_TURF" & destSiteID == "HIGH" ~ "LocalControl"
        ),
        Year = lubridate::year(Date),
        Cover = dplyr::recode(Cover, `+` = "0.5"),
        Cover = as.numeric(as.character(Cover)),
        destPlotID = paste(originSiteID, destSiteID, plotID, sep = "_"),
        destBlockID = as.character(destBlockID)
      ) |>
      dplyr::select(
        Year, originSiteID, destSiteID, destBlockID, destPlotID,
        Treatment, SpeciesName, Cover
      ) |>
      dplyr::distinct() |>
      dplyr::filter(!is.na(Cover)),
    stop("standardize_columns(): no column mapping defined for site '", site_cfg$site_id, "'")
  )
}
