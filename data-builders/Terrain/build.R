progress_message("Calculating terrain roughness index")

# load ETOPO data (we ignore the EGM2008 component to simplify calculations)
elev <- terra::rast(data_uri("ETOPO", "ETOPO_2022_v1_30s_N90W180_surface.tif"))
terra::crs(elev) <- "EPSG:4326"

# Roughnes index (RRI) at 5x5 window
# see Trevisani et al. 20023
suppressWarnings(land_rri <- SurfRough::RRI(elev)) # bug in terra, workaround
terra::crs(land_rri) <- terra::crs(elev)

# land mask for only cnsidering values on land
# note: we use conservative rasterization for mask generation
elev_land_mask <- {
  data_uri("NaturalEarth", "ne_10m_land") |>
  st_read(quiet = TRUE) |>
  exactextractr::rasterize_polygons(elev, min_coverage = 1)
}

elev[is.na(elev_land_mask)] <- NA
land_rri[is.na(elev_land_mask)] <- NA

progress_message("Calculating elevation summaries")

# main estimations
terrain_grid_stats <- mutate(grid,
  # elevation
  mc_raster_extract(
    elev,
    cell_geometry,
    fun = c("count", "mean", "min", "max", "stdev")
  ) |>
  select(
    count,
    land_elevation_mean = mean,
    land_elevation_min = min,
    land_elevation_max = max,
    land_elevation_sd = stdev
  ),
  # roughness
  mc_raster_extract(
    land_rri,
    cell_geometry,
    fun = c("mean", "min", "max", "stdev")
  ) |>
  select(
    land_roughness_mean = mean,
    land_roughness_min = min,
    land_roughness_max = max,
    land_roughness_sd = stdev
  )
) |>
filter(count > 0) |>
select(
  cell_id,
  starts_with("land_elevation_"),
  starts_with("land_roughness_")
)

# add the land area, keeping the data coverage from etopo
grid_stats <- mutate(grid, cell_land_area_prop = cell_land_area_km2/cell_area_km2) |>
  select(cell_id, land_area_km2 = cell_land_area_km2, land_area_prop = cell_land_area_prop)

terrain_grid_stats <- terrain_grid_stats |>
  left_join(grid_stats, join_by(cell_id)) |>
  relocate(cell_id, land_area_km2, land_area_prop, everything())

progress_message()

# save the artifact
write_artifact(terrain_grid_stats)
