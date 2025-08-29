# process a single year
resample_nightlights <- function(year) {
  # unpack the tiff file
  progress_message("Uncompressing nightlight data from year {year}")
  zipfile <- data_uri("SimulatedVIIRSNightlights", sprintf("SRUNet_NPP_VIIRS_V2_Like_%s.zip", year))
  tmp <- withr::local_tempdir()
  archive::archive_extract(zipfile, dir = tmp)

  progress_message("Sampling nightlight data from year {year}")
  rast <- fs::path(tmp, sprintf("SRUNet_NPP_VIIRS_V2_Like_%s.tif", year)) |>
    terra::rast() |>
    # downsample to 30-arc-seconds
    terra::aggregate(2L, fun = "mean", na.rm = TRUE, cores = n_available_cores())

  # sample and aggregate into relevant variables
  # we also convert the radiance to the more standard W/m2/sr measure
  grid_stats <- select(grid, cell_id, cell_geometry) |>
    # extract zonal statistics
    mutate(mc_raster_extract(
      rast,
      cell_geometry,
      fun = c("weighted_sum", "mean", "quantile", "count"),
      weights = "area",
      quantiles = c(0.1, 0.25, 0.5, 0.75, 0.9, 0.95)
    )) |>
    # exclude cells with zero coverage
    filter(count > 0) |>
    # rescale from original nW/sm2/sr to W/m2/sr
    # scaling factor: 1e-9 (nw to W) / 1e-4 (m2 to cm2) = 1e-5
    select(cell_id, weighted_sum, mean, starts_with("q")) |>
    mutate(across(!cell_id, function(x) x * 1e-5)) |>
    select(
      cell_id,
      # radiant intensity is the area-weighted sum of measured radiance
      ntl_intensity_watt_sr = weighted_sum,
      # mean radiance
      ntl_radiance_mean = mean,
      # percentiles
      ntl_radiance_p10 = q10,
      ntl_radiance_p25 = q25,
      ntl_radiance_p50 = q50,
      ntl_radiance_p75 = q75,
      ntl_radiance_p90 = q90,
      ntl_radiance_p95 = q95
    )

  # calculate radiant intensity for the within the cells
  # we sample the subgrid and aggregade per grid cell using the Gini index
  grid_dispersion <- mutate(subgrid,
    mc_raster_extract(
      rast,
      subcell_geometry,
      fun = c("weighted_sum", "count"),
      weights = "area"
    )) |>
    # drop geometry data
    select(!subcell_geometry) |>
    # exclude cells with zero coverage
    filter(count > 0) |>
    # rescale from original nW/sm2/sr to W/m2/sr
    # scaling factor: 1e-9 (nw to W) / 1e-4 (m2 to cm2) = 1e-5
    mutate(radiant_intensity = weighted_sum * 1e-5) |>
    # aggregate by call using Gini index
    summarize(
      ntl_intensity_dispersion = gini(radiant_intensity),
      .by = cell_id
    )

  # add the year and grid coverage
  left_join(grid_stats, grid_dispersion, join_by(cell_id)) |>
    mutate(year = year, .after = cell_id)
}

# process all data
ntl_data <- bind_rows(map(YEARS, resample_nightlights))

write_artifact(ntl_data)
