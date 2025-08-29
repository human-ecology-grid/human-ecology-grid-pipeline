# remote sources
remotes <- c(
  "10m/physical/ne_10m_land.zip",
  "10m/physical/ne_10m_coastline.zip",
  "10m/cultural/ne_10m_railroads.zip",
  "10m/cultural/ne_10m_railroads_north_america.zip",
  "10m/cultural/ne_10m_airports.zip",
  "10m/cultural/ne_10m_ports.zip",
  "10m/cultural/ne_10m_admin_0_countries.zip"
)
remotes <- paste0("https://naciscdn.org/naturalearth/", remotes)

for (remote in remotes) {
  uri <- httr2::url_parse(remote)$path |> fs::path_file()
  fetch_remote_file(remote, uri, .postprocess = list(
    label = "Extracting",
    fun = function(zipfile, .) {
      suppressMessages(archive::archive_extract(zipfile, fs::path_ext_remove(uri)))
    }
  ))
}
