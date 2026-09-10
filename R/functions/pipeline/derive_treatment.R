# Derive the canonical `Treatment` column, dispatching on site_cfg$treatment_rule.
# These are the small set of reusable strategies that cover all sites seen
# in the network so far (see plan Section 2).

derive_treatment <- function(site_data, site_cfg) {
  switch(site_cfg$treatment_rule,
    site_pair_recode     = derive_treatment_site_pair_recode(site_data, site_cfg),
    code_lookup          = derive_treatment_code_lookup(site_data, site_cfg),
    origin_dest_matrix   = derive_treatment_origin_dest_matrix(site_data, site_cfg),
    turf_code_site       = derive_treatment_turf_code_site(site_data, site_cfg),
    already_derived      = derive_treatment_already_derived(site_data, site_cfg),
    stop("derive_treatment(): unknown treatment_rule '", site_cfg$treatment_rule, "'")
  )
}

#' Treatment derived from a lookup keyed by "destSiteID_originSiteID" (or similar
#' compound key already present in a single column before it is split).
derive_treatment_site_pair_recode <- function(site_data, site_cfg) {
  key <- paste(site_data$destSiteID, site_data$originSiteID, sep = "_")
  site_data$Treatment <- unname(site_cfg$pipeline$treatment_map[key])
  site_data
}

#' Treatment from a code -> treatment lookup (`pipeline$treatment_map`).
#' `standardize_columns()` must leave the raw code in `treatment_code`
#' (e.g. a turfID substring for US_Colorado, or TTtreat for CN_Gongga / NO_*).
derive_treatment_code_lookup <- function(site_data, site_cfg) {
  site_data$Treatment <- dplyr::recode(site_data$treatment_code, !!!site_cfg$pipeline$treatment_map)
  site_data$treatment_code <- NULL
  site_data
}

#' Treatment derived from an originSiteID x destSiteID lookup table
#' (`site_cfg$pipeline$treatment_matrix`). Used for sites whose Warm/Cold/
#' LocalControl assignment is a matrix of elevation pairs (CN_Heibei,
#' DE_Susalps, DE_TransAlps) rather than a single code column.
derive_treatment_origin_dest_matrix <- function(site_data, site_cfg) {
  treatment_matrix <- site_cfg$pipeline$treatment_matrix
  if (is.null(treatment_matrix)) {
    stop(
      "derive_treatment(): treatment_rule 'origin_dest_matrix' requires ",
      "pipeline$treatment_matrix for site '", site_cfg$site_id, "'"
    )
  }
  required <- c("originSiteID", "destSiteID", "Treatment")
  missing <- setdiff(required, names(treatment_matrix))
  if (length(missing) > 0) {
    stop(
      "derive_treatment(): pipeline$treatment_matrix for site '", site_cfg$site_id,
      "' is missing columns: ", paste(missing, collapse = ", ")
    )
  }

  site_data$Treatment <- NULL
  site_data$originSiteID <- as.character(site_data$originSiteID)
  site_data$destSiteID <- as.character(site_data$destSiteID)
  treatment_matrix <- treatment_matrix[required]
  treatment_matrix$originSiteID <- as.character(treatment_matrix$originSiteID)
  treatment_matrix$destSiteID <- as.character(treatment_matrix$destSiteID)

  dplyr::left_join(
    site_data,
    treatment_matrix,
    by = c("originSiteID", "destSiteID")
  )
}

#' Origin site encoded as the prefix of a HIGH/LOW turf code (low_turf,
#' high_turf, HIGH_TURF, LOW_TURF). Used when destPlotID must be assembled
#' in standardize_columns() before derive_treatment() runs.
origin_site_from_turf_code <- function(treatment_code) {
  toupper(sub("(.*)_.*", "\\1", as.character(treatment_code)))
}

#' Treatment derived from a HIGH/LOW turf code x destSiteID combination
#' (DE_Grainau, CN_Damxung, IN_Kashmir, FR_AlpeHuez). standardize_columns()
#' leaves the raw code in `treatment_code`; originSiteID is the code prefix
#' (LOW vs HIGH) and Treatment is LocalControl (same elevation) or Warm
#' (high turf moved to LOW). Case of the raw labels is ignored.
derive_treatment_turf_code_site <- function(site_data, site_cfg) {
  if (is.null(site_data[["treatment_code"]])) {
    stop(
      "derive_treatment(): treatment_rule 'turf_code_site' expects ",
      "standardize_columns() to have set treatment_code for site '",
      site_cfg$site_id, "'"
    )
  }
  code <- tolower(as.character(site_data$treatment_code))
  dest <- toupper(as.character(site_data$destSiteID))
  site_data$originSiteID <- origin_site_from_turf_code(site_data$treatment_code)
  site_data$Treatment <- dplyr::case_when(
    code == "low_turf" & dest == "LOW" ~ "LocalControl",
    code == "high_turf" & dest == "LOW" ~ "Warm",
    code == "high_turf" & dest == "HIGH" ~ "LocalControl"
  )
  site_data$treatment_code <- NULL
  site_data
}

#' For sites where Treatment depends on more than one raw column at once
#' (e.g. CH_Calanda2's site x plot-number combination) a single lookup key
#' isn't a clean fit for the other rules above, so standardize_columns()
#' derives Treatment (and originSiteID, if needed) directly with a small
#' site-specific case_when(). This rule is a no-op that just checks the
#' column actually got set, so a typo in standardize_columns() fails loudly
#' here instead of silently producing NA Treatments.
derive_treatment_already_derived <- function(site_data, site_cfg) {
  if (is.null(site_data[["Treatment"]])) {
    stop(
      "derive_treatment(): treatment_rule 'already_derived' expects standardize_columns() ",
      "to have already set Treatment for site '", site_cfg$site_id, "'"
    )
  }
  site_data
}
