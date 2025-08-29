remote <- "https://dataportaal.pbl.nl/downloads/GRIP4/GRIP4_global_vector_fgdb.zip"

fetch_remote_file(remote, "GRIP4_global_vector_fgdb.zip", .postprocess = list(
  label = "Extracting",
  fun = function(zipfile, .) {
    suppressMessages(archive::archive_extract(zipfile))
  }
))
