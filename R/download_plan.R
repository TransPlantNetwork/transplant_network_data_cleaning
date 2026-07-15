# Download community database from OSF (node f3knq).
#
# Uses dataDownloader::need_update() and get_file() (Between-the-Fjords/dataDownloader).
# tar_change() mirrors the old Drake trigger: checks for updates each run and
# re-downloads when need_update() returns TRUE.

community_osf <- list(
  node = "f3knq",
  remote_path = "Community",
  file = "transplant.sqlite",
  path = "data"
)

download_plan <- tarchetypes::tar_change(
  name = community_download,
  change = dataDownloader::need_update(
    node = community_osf$node,
    remote_path = community_osf$remote_path,
    file = community_osf$file,
    path = community_osf$path
  ),
  command = dataDownloader::get_file(
    node = community_osf$node,
    remote_path = community_osf$remote_path,
    file = community_osf$file,
    path = community_osf$path
  ),
  format = "file"
)
