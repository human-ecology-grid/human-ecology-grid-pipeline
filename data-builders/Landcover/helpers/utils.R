# auxiliary function calculating the landcover statistics from a single raster layer
#
# - total landcover per cell
# - landcover dispersion across the subcells (Gini coefficient)
calculate_landcover_stats <- function(rasters, year) {
  # find the raster layer
  idx <- which(lubridate::year(terra::time(rasters)) == year)
  (length(idx) == 1) || error("invalid year {year}")

  # copy the raster(this speeds up the computation)
  rast <- terra::app(rasters[[idx]], "sum", cores = parallelly::availableCores())

  # total landcover (zonal sum)
  grid_stats <- select(grid, cell_id, cell_geometry) |>
    mutate(mc_raster_extract(rast, cell_geometry, fun = c("sum", "count"))) |>
    # exclude cells with zero coverage
    filter(count > 0) |>
    select(cell_id, area_km2 = sum)

  # subcell stats (dispersion)
  grid_dispersion <- subgrid |>
    mutate(mc_raster_extract(rast, subcell_geometry, fun = c("sum", "count"))) |>
    # exclude cells with zero coverage
    filter(count > 0) |>
    # feature dispersion over the cell as Gini index of feature density
    summarize(
      dispersion = gini(sum/subcell_land_area_km2),
      .by = cell_id
    ) |>
    filter(!is.na(dispersion))

  # combine the statistics and add the temporal dimension
  left_join(grid_stats, grid_dispersion, join_by(cell_id)) |>
    mutate(year = year, .after = cell_id)
}
