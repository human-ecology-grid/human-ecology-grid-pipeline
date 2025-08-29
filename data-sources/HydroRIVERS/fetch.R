remote <- "https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10.gdb.zip"

fetch_remote_file(remote, "HydroRIVERS_v10.gdb.zip", .postprocess = list(
  label = "Extracting",
  fun = function(zipfile, .) {
    suppressMessages(archive::archive_extract(zipfile))
  }
))
