# Taxonomic name harmonization, run once on the merged dataset (not per-site).
# Uses the TNRS package (https://github.com/EnquistLab/RTNRS) as the primary
# resolver, replacing the old taxize::gnr_resolve() call in
# R/WrangleTaxaTraits/clean_taxonomy.R. Manual overrides for known
# misspellings/abbreviations are kept as a small lookup table so they remain
# inspectable and can grow independently of the TNRS call itself.
#
# Requires network access and the TNRS package/API; wrapped in tryCatch so
# tar_make() fails with a clear message rather than a cryptic API error if
# the service is unreachable.

manual_taxonomy_overrides <- c(
  "Stellaria_wide_leaf" = "Stellaria umbellata",
  "Entire Meconopsis" = "Meconopsis",
  "Leuc. Vulg." = "Leucanthemum vulgare",
  "unknown aster serrated" = "Aster",
  "Unknown aster serrated" = "Aster",
  "Haemarocalus fulva" = "Hemerocallis fulva",
  "Dracophaleum" = "Dracocephalum",
  "Antoxanthum alpinum" = "Anthoxanthum odoratum nipponicum",
  "Anthoxanthum alpinum" = "Anthoxanthum odoratum nipponicum",
  "Vaccinium gaultherioides" = "Vaccinium uliginosum",
  "Listera ovata" = "Neottia ovata",
  "Gentiana tenella" = "Gentianella tenella",
  "Nigritella nigra" = "Gymnadenia nigra",
  "Hieracium lactucela" = "Pilosella lactucella",
  "Agrostis schraderiana" = "Agrostis agrostiflora",
  "Festuca pratense" = "Festuca pratensis",
  "Ran. acris subsp. Friesianus" = "Ranunculus acris subsp. friesianus",
  "Symphyothricum_sp." = "Symphyotrichum",
  "Orchidacea spec" = "Orchidaceae",
  "Carex biggelowii" = "Carex bigelowii",
  "Carex spec" = "Carex",
  "Hol.lan" = "Holcus lanatus",
  "Dia.med" = "Dianthus deltoides"
)

#' Apply manual overrides, then resolve unique species names via TNRS.
#'
#' @param merged_community Output of merge_comm_data(): must have a SpeciesName column.
#' @return tibble with SpeciesName (original), submitted_name (after manual
#'   overrides), Accepted_name, Taxonomic_status, Overall_score (from TNRS).
harmonize_taxonomy <- function(merged_community) {
  taxa <- unique(merged_community$SpeciesName)
  taxa <- taxa[!is.na(taxa)]

  submitted_name <- dplyr::recode(taxa, !!!manual_taxonomy_overrides)

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

  tibble::tibble(SpeciesName = taxa, submitted_name = submitted_name) %>%
    dplyr::left_join(
      resolved %>%
        dplyr::select(
          submitted_name = Name_submitted,
          Accepted_name = Accepted_name,
          Taxonomic_status = Taxonomic_status,
          Overall_score = Overall_score
        ),
      by = "submitted_name"
    )
}
