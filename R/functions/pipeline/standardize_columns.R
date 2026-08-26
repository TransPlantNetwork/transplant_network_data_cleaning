# Rename raw, site-specific column names to the canonical schema names.
# For pilot sites the small amount of site-specific renaming is done inline
# here via a `switch`, mirroring the "column_map" idea from the plan: it is
# explicit, inspectable, and isolated to this one function rather than
# scattered across a whole cleaning script.

standardize_columns <- function(raw, site_cfg) {
  switch(site_cfg$site_id,
    CH_Lavey = raw %>%
      dplyr::rename(Cover = cover, Year = year, plotID = turfID) %>%
      dplyr::mutate(Cover = as.numeric(Cover)) %>%
      tidyr::separate(siteID, c("destSiteID", "originSiteID"), sep = "_"),
    US_Colorado = raw %>%
      dplyr::select(year, turfID, species, percentCover) %>%
      # 27 rows (all in 2023) have no turfID (or any other plot identifier) in
      # the raw file at all - a genuine gap in that year's raw data, not
      # something recoverable from other columns. Drop them rather than let
      # them silently collapse into one bogus "NA" plot; worth following up
      # with the data provider about what plot(s) they belong to.
      dplyr::filter(!is.na(turfID), turfID != "") %>%
      dplyr::rename(SpeciesName = species, Cover = percentCover, Year = year, destPlotID = turfID) %>%
      dplyr::mutate(
        Year = as.numeric(Year),
        Cover = as.numeric(Cover),
        destSiteID = substr(destPlotID, 1, 2),
        destBlockID = substr(destPlotID, 3, 3),
        treatment_code = substr(destPlotID, 7, 8),
        originSiteID = substr(destPlotID, nchar(destPlotID) - 4, nchar(destPlotID) - 3),
        originBlockID = substr(destPlotID, nchar(destPlotID) - 2, nchar(destPlotID) - 2)
      ),
    CN_Gongga = raw %>%
      dplyr::filter(TTtreat != "OTC") %>%
      dplyr::rename(Year = year, treatment_code = TTtreat, Cover = cover, SpeciesName = speciesName) %>%
      dplyr::mutate(
        SpeciesName = dplyr::recode(SpeciesName, "Potentilla stenophylla var. emergens" = "Potentilla stenophylla")
      ) %>%
      dplyr::filter(!is.na(Cover), Cover != 0),
    # Treatment depends jointly on `site` and `plot` (not a single code
    # column), so it - and the originSiteID it implies - are derived here
    # directly rather than via a treatment_rule; see
    # derive_treatment_already_derived() in derive_treatment.R. Plot numbers
    # 1/3 at "Nes" are the warmed transplants, 8/9 at "Nes" are the local
    # controls moved back to their own site; "Cal" plots are all
    # LocalControl (never moved).
    CH_Calanda2 = raw %>%
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
      ) %>%
      dplyr::select(-plot) %>%
      dplyr::rename(destSiteID = site, Cover = cover, Year = year, SpeciesName = species, destPlotID = plot_id, destBlockID = block) %>%
      dplyr::mutate(Cover = as.numeric(Cover), destPlotID = as.character(destPlotID), destBlockID = as.character(destBlockID)) %>%
      dplyr::filter(!is.na(Cover)),
    stop("standardize_columns(): no column mapping defined for site '", site_cfg$site_id, "'")
  )
}
