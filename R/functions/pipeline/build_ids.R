# Assemble UniqueID (and destPlotID, where not already present) from the
# per-site list of id components declared in the registry.

build_ids <- function(dat, site_cfg) {
  components <- site_cfg$pipeline$id_components

  if (is.null(dat[["destPlotID"]])) {
    dat$destPlotID <- do.call(paste, c(as.list(dat[intersect(components, names(dat))]), sep = "_"))
  }
  dat$destPlotID <- as.character(dat$destPlotID)
  if (is.null(dat[["destBlockID"]])) dat$destBlockID <- NA_character_
  dat$destBlockID <- as.character(dat$destBlockID)

  dat$UniqueID <- do.call(paste, c(as.list(dat[intersect(components, names(dat))]), sep = "_"))
  dat
}
