remote <- "https://data.hydrosheds.org/file/hydrolakes/HydroLAKES_polys_v10.gdb.zip"

fetch_remote_file(remote, "HydroLAKES_polys_v10.gdb.zip", .postprocess = list(
  label = "Extracting",
  fun = function(zipfile, .) {
    suppressMessages(archive::archive_extract(zipfile))
  }
))
