remote <- "https://figshare.com/ndownloader/articles/22262545/versions/8"

fetch_remote_file(remote, "simulated_NPP_VIIRS_V2.zip", .postprocess = list(
  label = "Extracting",
  fun = function(zipfile, .) {
    suppressMessages(archive::archive_extract(zipfile, files = PIPELINE_INFO$digest$uri))
  }
))
