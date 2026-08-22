# Compute Total_Cover and Rel_Cover per plot x year, on the FULL row set
# (vascular species + non-vascular cover classes + any synthetic "Other"),
# BEFORE splitting into comm/cover (see clean_site.R for the exact order -
# this matters: it's what the legacy per-site scripts do, and doing it after
# splitting would inflate Rel_Cover for sites that add an "Other" category).

compute_rel_cover <- function(dat, site_cfg) {
  # Group by UniqueID only (one plot x year per group) - see add_other_category()
  # in split_cover_classes.R for why grouping by every remaining column is
  # wrong for sites with per-row columns like a raw species code or QA flag.
  dat %>%
    dplyr::group_by(UniqueID) %>%
    dplyr::mutate(Total_Cover = sum(Cover, na.rm = TRUE), Rel_Cover = Cover / Total_Cover) %>%
    dplyr::ungroup()
}
