# Validation stage v1: a small, deliberately minimal set of "obvious" checks,
# driven by config/schema.yml so they can never drift from the documented
# schema. Each check returns a tidy row (site, check, status, message)
# instead of stopping the pipeline, so failures are visible/reportable and
# more checks can be appended later without restructuring anything (plan
# Section 4).

validate_site <- function(cleaned, site_cfg) {
  schema <- load_schema()
  site_id <- site_cfg$site_id

  checks <- list(
    check_required_columns(cleaned$community, schema, "community", site_id),
    check_required_columns(cleaned$meta, schema, "meta", site_id),
    check_rel_cover_sums(cleaned$community, cleaned$cover, site_id),
    check_treatment_values(cleaned$community, schema, site_id),
    check_no_negative_cover(cleaned$community, site_id),
    check_unique_ids(cleaned$community, site_id),
    check_meta_site_match(cleaned$community, cleaned$meta, site_id)
  )

  dplyr::bind_rows(checks)
}

.result <- function(site, check, status, message) {
  tibble::tibble(site = site, check = check, status = status, message = message)
}

# --- Schema checks -----------------------------------------------------

check_required_columns <- function(df, schema, table, site_id) {
  required <- schema_required_columns(schema, table)
  missing <- setdiff(required, names(df))
  if (length(missing) == 0) {
    .result(site_id, paste0("schema_", table), "pass", "all required columns present")
  } else {
    .result(site_id, paste0("schema_", table), "fail", paste("missing columns:", paste(missing, collapse = ", ")))
  }
}

# --- Value checks --------------------------------------------------------

check_rel_cover_sums <- function(comm, cover, site_id) {
  if (is.null(comm[["Rel_Cover"]]) || is.null(comm[["UniqueID"]])) {
    return(.result(site_id, "rel_cover_sums", "skip", "Rel_Cover or UniqueID missing"))
  }
  # comm$Rel_Cover only covers vascular species: it should sum to ~1 together
  # with cover$Rel_OtherCover (non-vascular cover classes, e.g. bare ground,
  # litter, and any synthesized "Other"), not on its own - see
  # R/functions/pipeline/compute_rel_cover.R for why Rel_Cover is computed on
  # the full row set before comm/cover are split apart.
  comm_sums <- comm %>%
    dplyr::group_by(UniqueID) %>%
    dplyr::summarise(total = sum(Rel_Cover, na.rm = TRUE), .groups = "drop")
  if (!is.null(cover) && nrow(cover) > 0 && !is.null(cover[["Rel_OtherCover"]]) && !is.null(cover[["UniqueID"]])) {
    cover_sums <- cover %>%
      dplyr::group_by(UniqueID) %>%
      dplyr::summarise(other = sum(Rel_OtherCover, na.rm = TRUE), .groups = "drop")
    comm_sums <- comm_sums %>%
      dplyr::left_join(cover_sums, by = "UniqueID") %>%
      dplyr::mutate(other = tidyr::replace_na(other, 0), total = total + other)
  }
  bad <- comm_sums[abs(comm_sums$total - 1) > 0.05, ]
  if (nrow(bad) == 0) {
    .result(site_id, "rel_cover_sums", "pass", "Rel_Cover (+ Rel_OtherCover) sums to ~1 for all plots x years")
  } else {
    .result(site_id, "rel_cover_sums", "fail", paste(nrow(bad), "plot x year combinations do not sum to ~1"))
  }
}

check_treatment_values <- function(comm, schema, site_id) {
  allowed <- schema$community$Treatment$allowed_values
  if (is.null(comm[["Treatment"]])) {
    return(.result(site_id, "treatment_values", "skip", "Treatment column missing"))
  }
  bad <- setdiff(unique(comm$Treatment), allowed)
  if (length(bad) == 0) {
    .result(site_id, "treatment_values", "pass", "all Treatment values allowed")
  } else {
    .result(site_id, "treatment_values", "fail", paste("unexpected values:", paste(bad, collapse = ", ")))
  }
}

check_no_negative_cover <- function(comm, site_id) {
  if (is.null(comm[["Cover"]])) {
    return(.result(site_id, "no_negative_cover", "skip", "Cover column missing"))
  }
  n_negative <- sum(comm$Cover < 0, na.rm = TRUE)
  if (n_negative == 0) {
    .result(site_id, "no_negative_cover", "pass", "no negative Cover values")
  } else {
    .result(site_id, "no_negative_cover", "fail", paste(n_negative, "negative Cover values"))
  }
}

# --- Referential checks --------------------------------------------------

check_unique_ids <- function(comm, site_id) {
  if (is.null(comm[["UniqueID"]]) || is.null(comm[["SpeciesName"]])) {
    return(.result(site_id, "duplicate_ids", "skip", "UniqueID or SpeciesName missing"))
  }
  n_dupes <- comm %>%
    dplyr::count(UniqueID, SpeciesName) %>%
    dplyr::filter(n > 1) %>%
    nrow()
  if (n_dupes == 0) {
    .result(site_id, "duplicate_ids", "pass", "no duplicate UniqueID x SpeciesName rows")
  } else {
    .result(site_id, "duplicate_ids", "fail", paste(n_dupes, "duplicate UniqueID x SpeciesName rows"))
  }
}

check_meta_site_match <- function(comm, meta, site_id) {
  if (is.null(comm[["destSiteID"]]) || is.null(meta[["destSiteID"]])) {
    return(.result(site_id, "meta_site_match", "skip", "destSiteID missing from community or meta"))
  }
  missing <- setdiff(unique(comm$destSiteID), unique(meta$destSiteID))
  if (length(missing) == 0) {
    .result(site_id, "meta_site_match", "pass", "every destSiteID in community has a meta row")
  } else {
    .result(site_id, "meta_site_match", "fail", paste("destSiteID missing from meta:", paste(missing, collapse = ", ")))
  }
}
