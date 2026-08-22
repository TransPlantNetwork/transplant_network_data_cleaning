# Compute Total_Cover and Rel_Cover per plot x year, on the FULL row set
# (vascular species + non-vascular cover classes + any synthetic "Other"),
# BEFORE splitting into comm/cover (see clean_site.R for the exact order -
# this matters: it's what the legacy per-site scripts do, and doing it after
# splitting would inflate Rel_Cover for sites that add an "Other" category).

compute_rel_cover <- function(dat, site_cfg) {
  id_cols <- setdiff(names(dat), c("SpeciesName", "Cover"))
  dat %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(id_cols))) %>%
    dplyr::mutate(Total_Cover = sum(Cover, na.rm = TRUE), Rel_Cover = Cover / Total_Cover) %>%
    dplyr::ungroup()
}
