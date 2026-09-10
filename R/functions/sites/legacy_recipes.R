# Recipe wrappers for sites that are not on the general pipeline.
#
# Almost every site is cleaned by R/functions/pipeline/ (see site_registry.R).
# US_Arizona is the exception: its `community` table comes from individual
# counts (one excel sheet), while its `cover` table comes from a separate
# "% green ground cover" measurement (another file). Those are not two
# subsets of the same rows (unlike every other site, where cover classes are
# split out of the same long table by split_cover_classes()). Forcing Arizona
# into import_raw → standardize → add_other → compute_rel_cover → split would
# mean special-casing almost every shared step, so it stays on this thin
# wrapper around ImportClean_US_Arizona() instead.
#
# The original ImportClean_* scripts under R/functions/ImportData/ are still
# kept for *all* sites (including migrated ones) so new pipeline output can
# be compared against the old cleaning code during review. Only US_Arizona
# is still *run* via a recipe_fn from the targets pipeline.
#
# Returns the same contract as clean_site(): list(meta=, community=, cover=, taxa=).

clean_recipe_US_Arizona <- function() ImportClean_US_Arizona()
