token <- PIPELINE_INFO$credentials$NativeLand$token

!is_null(token) || error(
  "missing API token for Native Land Digital API",
  i = "token can be obtained from {.url https://api-docs.native-land.ca}",
  i = "place the token in the {.file config.toml}",
  i = "",
  i = "[[credentials.NativeLand]]",
  i = "token = \"<token>\""
)

remote <- sprintf("https://native-land.ca/api/polygons/geojson/languages?key=%s", token)
fetch_remote_file(remote, "nativeland_languages.geojson.gz", .postprocess = list(
  label = "Compressing",
  fun = gzip_file
))
