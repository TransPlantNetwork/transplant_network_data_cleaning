# Derive the canonical `Treatment` column, dispatching on site_cfg$treatment_rule.
# These are the small set of reusable strategies that cover all sites seen
# in the network so far (see plan Section 2).

derive_treatment <- function(dat, site_cfg) {
  switch(site_cfg$treatment_rule,
    site_pair_recode = derive_treatment_site_pair_recode(dat, site_cfg),
    turfid_substring  = derive_treatment_turfid_substring(dat, site_cfg),
    code_lookup       = derive_treatment_code_lookup(dat, site_cfg),
    stop("derive_treatment(): unknown treatment_rule '", site_cfg$treatment_rule, "'")
  )
}

#' Treatment derived from a lookup keyed by "destSiteID_originSiteID" (or similar
#' compound key already present in a single column before it is split).
derive_treatment_site_pair_recode <- function(dat, site_cfg) {
  key <- paste(dat$destSiteID, dat$originSiteID, sep = "_")
  dat$Treatment <- unname(site_cfg$pipeline$treatment_map[key])
  dat
}

#' Treatment derived from a code embedded in the plot/turf ID (already
#' extracted into a `treatment_code` column by standardize_columns()).
derive_treatment_turfid_substring <- function(dat, site_cfg) {
  dat$Treatment <- dplyr::recode(dat$treatment_code, !!!site_cfg$pipeline$treatment_map)
  dat$treatment_code <- NULL
  dat
}

#' Treatment derived from a direct code -> treatment lookup table
#' (already extracted into a `treatment_code` column by standardize_columns()).
derive_treatment_code_lookup <- function(dat, site_cfg) {
  dat$Treatment <- dplyr::recode(dat$treatment_code, !!!site_cfg$pipeline$treatment_map)
  dat$treatment_code <- NULL
  dat
}
