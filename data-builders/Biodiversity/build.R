t0 <- bench::hires_time()

progress_message("Loading GLOBIO4 World")

stopifnot(length(YEARS) == 1L)

rast <- data_uri("GLOBIO4", sprintf("TerrestrialMSA_%s_World.tif", YEARS)) |>
  terra::rast() |>
  # downsample to 30-arc-seconds
  terra::aggregate(3L, fun = "mean", na.rm = TRUE, cores = n_available_cores())

progress_message("Aggregating GLOBIO4 World")

msa_grid <- tibble(
  cell_id = grid$cell_id,
  mc_raster_extract(
    rast,
    grid$cell_geometry,
    fun = c("mean", "count")
  )
) |>
# drop empty cells
filter(
  count > 0
) |>
mutate(
  msa_mean = mean
) |>
select(
  cell_id,
  msa_mean
)

msa_grid_dispersion <- tibble(
  cell_id = subgrid$cell_id,
  mc_raster_extract(
    rast,
    subgrid$subcell_geometry,
    fun = c("mean", "count")
  )
) |>
# drop empty cells
filter(
  count > 0
) |>
# msa dispersion (Gini index)
summarize(
  msa_dispersion = gini(mean),
  .by = cell_id
)

msa_grid <- left_join(msa_grid, msa_grid_dispersion, join_by(cell_id)) |>
 mutate(year = YEARS, .after = cell_id)

write_artifact(msa_grid)
