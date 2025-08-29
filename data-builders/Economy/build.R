# load the data
g_econ <- local({
  zipdir <- local_tempdir()
  archive::archive_extract(data_uri("G-ECON", "spatialecon-gecon-v4-xls.zip"), zipdir)

  fs::path(zipdir, "Original data/gecon-v4.xls") |>
  readxl::read_excel(sheet = "GEcon40")
})

# the original data is dissagregated by country, collapse them
# also move the temporal dimension into the long format and convert everything to USD
g_econ <- summarize(g_econ,
  across(starts_with("MER") | starts_with("PPP"), function(x) sum(x, na.rm = TRUE)*1e6),
  QUALITY = min(QUALITY),
  .by = c(LONGITUDE, LAT)
) |>
pivot_longer(
  -c(LONGITUDE, LAT, QUALITY),
  names_pattern = "(MER|PPP)([0-9]+)_40",
  names_to = c(".value", "year")
) |>
mutate(year = as.integer(year))

# helper to convert g-econ grid into a raster
# note: the G-Econ data grid coordinates point the the south-west corner
make_raster <- function(lat, lon, var) {
  rast <- terra::rast(extent = terra::ext(-180, 180, -90, 90), resolution = 1, crs = "EPSG:4326")
  gg <- tibble(lat = lat, lon = lon, var = var)
  pp <- terra::vect(gg, geom = c("lon", "lat"), crs = "EPSG:4326")

  terra::rasterize(pp, rast, field = "var")
}

g_econ_grid <- reframe(g_econ,
  cell_id = grid$cell_id,
  mer = {
    make_raster(LAT, LONGITUDE, MER) |>
    mc_raster_extract(grid$cell_geometry, fun = "sum") |>
    pull("sum")
  },
  mer = if_else(mer == 0, NA, mer),
  ppp = {
    make_raster(LAT, LONGITUDE, PPP) |>
    mc_raster_extract(grid$cell_geometry, fun = "sum") |>
    pull("sum")
  },
  ppp = if_else(ppp == 0, NA, ppp),
  # data quality as percentage of cells with QUALITY == 1
  qlty = {
    make_raster(LAT, LONGITUDE, QUALITY == 1) |>
    mc_raster_extract(grid$cell_geometry, fun = "mean") |>
    pull("mean")
  },
  qlty = if_else(is.na(qlty) | (is.na(ppp) & is.na(mer)), NA, qlty),
  .by = year
) |>
select(
  cell_id,
  year,
  gdp_mer_usd = mer,
  gdp_ppp_usd = ppp,
  gdp_reliability = qlty
)  |>
# remove empty rows
filter(!if_all(!cell_id & !year, is.na))

write_artifact(g_econ_grid)
