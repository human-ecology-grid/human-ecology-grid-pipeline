# aggregate climate variables
climate_data <- imap(climvars, function(id, var) {
  progress_message("Processing CHELSA climactic variable {.var {id}} [1981-2010] ")

  uri <- data_uri("CHELSA", sprintf("CHELSA_%s_1981-2010_V.2.1.tif", id))
  rast <- terra::rast(uri)

  tibble(
    cell_id = grid$cell_id,
    mc_raster_extract(rast, grid$cell_geometry, fun = c("count", "mean"))
  ) |>
  filter(count > 0L) |>
  select(cell_id, !!var := mean)
})

# combine the data
climate_grid <- reduce(climate_data, function(x, y) {
  full_join(x, y, by = "cell_id")
})

write_artifact(climate_grid)
