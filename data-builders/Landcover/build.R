# Calculate cropland coverage
raster <- data_uri("HYDE", "cropland.nc") |> terra::rast()

cropland_grid_stats <- purrr::map(YEARS, function(year) {
  progress_message("Calculating cropland landcover for year {year}")
  calculate_landcover_stats(raster, year)
}) |> bind_rows() |> select(
  cell_id,
  year,
  land_crop_area_km2 = area_km2,
  land_crop_dispersion = dispersion
)

# Calculate grazing land coverage
raster <- data_uri("HYDE", "grazing_land.nc") |> terra::rast()

grazing_grid_stats <- purrr::map(YEARS, function(year) {
  progress_message("Calculating grazing landcover for year {year}")
  calculate_landcover_stats(raster, year)
}) |> bind_rows() |> select(
  cell_id,
  year,
  land_grazing_area_km2 = area_km2,
  land_grazing_dispersion = dispersion
)

# Calculate urban (built-up) land coverage
raster <- data_uri("HYDE", "urban_area.nc") |> terra::rast()

urban_grid_stats <- map(YEARS, function(year) {
  progress_message("Calculating urban landcover for year {year}")
  calculate_landcover_stats(raster, year)
}) |> bind_rows() |> select(
  cell_id,
  year,
  land_urban_area_km2 = area_km2,
  land_urban_dispersion = dispersion
)

# combine all data
landcover_grid <- cropland_grid_stats |>
  full_join(grazing_grid_stats, join_by(cell_id, year)) |>
  full_join(urban_grid_stats, join_by(cell_id, year)) |>
  relocate(cell_id, year)

write_artifact(landcover_grid)
