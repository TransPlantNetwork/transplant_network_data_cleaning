# Site plan: one branch of targets per site, generated from `site_registry`
# with tarchetypes::tar_map() (static branching) instead of 19 hand-written
# targets. Each site gets its own inspectable, individually rerunnable set of
# targets (e.g. `cleaned_CH_Lavey`, `validated_CH_Lavey`), visible in
# tar_visnetwork()/tar_manifest().
#
# Adding a new site = add a row to site_registry (Section 1 of the plan),
# not write a new ~80-line script.
#
# Wrapped in a function (rather than executed at source-time) so it does not
# depend on file-sourcing order: R/functions/site_registry.R must be sourced before
# this runs, which build_site_plan() guarantees is true by the time
# _targets.R calls it (after tar_source() has sourced every R/ file).

build_site_plan <- function() {
  shared_library_plan <- list(
    tar_target(
      name = cover_scales_file,
      command = "config/cover_scales.csv",
      format = "file"
    ),
    tar_target(
      name = species_recode_file,
      command = "config/species_recode.csv",
      format = "file"
    )
  )

  site_plan <- tarchetypes::tar_map(
    values = list(site_id = site_registry$site_id),
    names = site_id,
    # Track config/sites/<site_id>/*.csv so library edits invalidate that site.
    tar_target(site_library_files, site_library_paths(site_id), format = "file"),
    tar_target(cleaned, {
      site_library_files
      cover_scales_file
      species_recode_file
      clean_site(get_site_config(site_id))
    }),
    tar_target(validated, validate_site(cleaned, get_site_config(site_id)))
  )

  # Collect every site's `cleaned` output into one named list (named by
  # site_id, matching legacy `Region` naming) and every site's `validated`
  # report into one combined validation report.
  site_collect_plan <- list(
    tarchetypes::tar_combine(
      all_sites_cleaned,
      site_plan[["cleaned"]],
      command = rlang::set_names(list(!!!.x), site_registry$site_id)
    ),
    tarchetypes::tar_combine(
      all_sites_validated,
      site_plan[["validated"]],
      command = dplyr::bind_rows(!!!.x)
    )
  )

  c(shared_library_plan, site_plan, site_collect_plan)
}
