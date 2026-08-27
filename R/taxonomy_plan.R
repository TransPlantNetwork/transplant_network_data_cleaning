# Taxonomy plan: taxonomic name resolution via TNRS, run once after the
# community data is cleaned and merged (see R/pipeline/taxonomy.R for the
# resolution logic and manual overrides). Kept separate from
# harmonization_plan.R / validation_plan.R so taxonomy concerns (fuzzy
# matching, synonymy, unresolved-name overrides) stay independently
# testable from the community-data cleaning logic.

taxonomy_plan <- list(
  tar_target(
    name = taxonomy_lookup,
    command = harmonize_taxonomy(merged_community)
  ),
  tar_target(
    name = merged_community_harmonized,
    command = merged_community |>
      dplyr::left_join(taxonomy_lookup, by = "SpeciesName")
  ),
  # Per-site % of species/rows TNRS couldn't confidently resolve - see
  # compute_taxonomy_resolution() for what counts as "unresolved". Surfaced
  # in the validation report (R/functions/validation_report.R) as an extra
  # per-site metric.
  tar_target(
    name = taxonomy_resolution_summary,
    command = compute_taxonomy_resolution(merged_community_harmonized)
  )
)
