# Validation plan: v1 checks on each cleaned site (see R/pipeline/validate_site.R),
# combined into one report target. Per-site validation targets themselves are
# generated in R/site_plan.R (one `validated_<site_id>` target per site, via
# tar_map), so a single site's checks can be inspected/rerun independently.
#
# This list only adds a target that fails loudly if any check across any site
# failed, and (optionally) writes the full report to disk for inspection.
# More checks can be added to validate_site() over time without touching
# this file or restructuring the pipeline.

validation_plan <- list(
  tar_target(
    name = validation_summary,
    command = {
      failed <- dplyr::filter(all_sites_validated, status == "fail")
      if (nrow(failed) > 0) {
        warning(
          nrow(failed), " validation check(s) failed:\n",
          paste(capture.output(print(failed, n = Inf)), collapse = "\n"),
          call. = FALSE
        )
      }
      all_sites_validated
    }
  ),
  # Human-readable, per-site (per-gradient) markdown report - see
  # R/functions/validation_report.R. Depends on validation_summary (not
  # all_sites_validated directly) purely for ordering: it's the same data,
  # but this way the "any checks failed" warning above always surfaces
  # before/alongside the report in tar_make() output.
  tar_target(
    name = validation_report,
    command = {
      validation_summary
      render_validation_report(all_sites_cleaned, all_sites_validated, out_path = "docs/validation_report.md")
    },
    format = "file"
  )
)
