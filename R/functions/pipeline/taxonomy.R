# Taxonomic name harmonization, run once on the merged dataset (not per-site).
# Uses the TNRS package (https://github.com/EnquistLab/RTNRS) as the primary
# resolver, replacing the old taxize::gnr_resolve() call in
# R/WrangleTaxaTraits/clean_taxonomy.R. Manual overrides for known
# misspellings/abbreviations live in config/taxonomy_overrides.csv so they
# remain inspectable and can grow independently of the TNRS call itself.
#
# Requires network access and the TNRS package/API; wrapped in tryCatch so
# tar_make() fails with a clear message rather than a cryptic API error if
# the service is unreachable.

#' Load manual species-name overrides from a two-column CSV (`from`, `to`).
#'
#' @param path Path to config/taxonomy_overrides.csv (or a targets file path).
#' @return Named character vector suitable for dplyr::recode().
load_taxonomy_overrides <- function(path = "config/taxonomy_overrides.csv") {
  if (!file.exists(path)) {
    stop("Taxonomy overrides file not found: '", path, "'", call. = FALSE)
  }
  overrides <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  required <- c("from", "to")
  missing <- setdiff(required, names(overrides))
  if (length(missing) > 0) {
    stop(
      "Taxonomy overrides file '", path, "' is missing columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  if (any(is.na(overrides$from) | overrides$from == "")) {
    stop("Taxonomy overrides file '", path, "' has blank 'from' values", call. = FALSE)
  }
  stats::setNames(as.character(overrides$to), as.character(overrides$from))
}

#' Apply manual overrides, then resolve unique species names via TNRS.
#'
#' @param merged_community Output of merge_comm_data(): must have a SpeciesName column.
#' @param overrides_path Path to the taxonomy overrides CSV (tracked as a
#'   targets file dependency via taxonomy_plan.R).
#' @return tibble with SpeciesName (original), submitted_name (after manual
#'   overrides), Accepted_name, Taxonomic_status, Overall_score (from TNRS).
harmonize_taxonomy <- function(merged_community,
                               overrides_path = "config/taxonomy_overrides.csv") {
  taxa <- unique(merged_community$SpeciesName)
  taxa <- taxa[!is.na(taxa)]

  overrides <- load_taxonomy_overrides(overrides_path)
  submitted_name <- dplyr::recode(taxa, !!!overrides)

  if (!requireNamespace("TNRS", quietly = TRUE)) {
    stop(
      "The TNRS package is required for taxonomy harmonization but is not installed.\n",
      "Install it with remotes::install_github('EnquistLab/RTNRS').",
      call. = FALSE
    )
  }

  resolved <- tryCatch(
    TNRS::TNRS(taxonomic_names = submitted_name),
    error = function(e) {
      stop("TNRS lookup failed (is there network access to the TNRS API?): ", conditionMessage(e), call. = FALSE)
    }
  )

  tibble::tibble(SpeciesName = taxa, submitted_name = submitted_name) |>
    dplyr::left_join(
      resolved |>
        dplyr::select(
          submitted_name = Name_submitted,
          Accepted_name = Accepted_name,
          Taxonomic_status = Taxonomic_status,
          Overall_score = Overall_score
        ),
      by = "submitted_name"
    )
}

#' Per-site (Region) share of community rows/species whose SpeciesName TNRS
#' could not confidently resolve to an accepted name (Accepted_name missing
#' or blank, or no Overall_score returned at all) - a quick "how much of
#' this site's data is unidentified/misspelled species" signal, independent
#' of manually checking individual names. Feeds into the validation report
#' (R/functions/validation_report.R) as an extra per-site metric, not a
#' pass/fail check - a high % isn't necessarily wrong (small/rare taxa and
#' genuinely field-unidentifiable specimens are expected), just worth eyeballing.
#'
#' @param merged_community_harmonized The `merged_community_harmonized`
#'   target: merged community data (has a Region column identifying the
#'   site) left-joined with `taxonomy_lookup` (Accepted_name, Overall_score, ...).
compute_taxonomy_resolution <- function(merged_community_harmonized) {
  merged_community_harmonized |>
    dplyr::mutate(
      unresolved = is.na(Accepted_name) | Accepted_name == "" | is.na(Overall_score)
    ) |>
    dplyr::group_by(site = Region) |>
    dplyr::summarise(
      n_species = dplyr::n_distinct(SpeciesName),
      n_unresolved_species = dplyr::n_distinct(SpeciesName[unresolved]),
      pct_unresolved_species = round(100 * n_unresolved_species / n_species, 1),
      n_rows = dplyr::n(),
      n_unresolved_rows = sum(unresolved),
      pct_unresolved_rows = round(100 * n_unresolved_rows / n_rows, 1),
      .groups = "drop"
    )
}
