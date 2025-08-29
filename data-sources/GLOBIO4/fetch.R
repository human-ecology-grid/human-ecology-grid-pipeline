remote <- "https://dataportaal.pbl.nl/downloads/GLOBIO/Schipper_etal_2020/2015/Globio4_TerrestrialMSA_10sec_2015.zip"

fetch_remote_file(remote, "Globio4_TerrestrialMSA_10sec_2015.zip", .postprocess = list(
  label = "Extracting",
  fun = function(zipfile, .) {
    suppressMessages(archive::archive_extract(zipfile, files = "TerrestrialMSA_2015_World.tif"))
  }
))
