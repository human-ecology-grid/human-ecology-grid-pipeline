
# total population
rasters <- data_uri("HYDE", "population.nc") |> terra::rast()
pop_grid_stats <- map(YEARS, function(year) {
  progress_message("Aggregating population for year {year}")
  calculate_population_grid_stats(rasters, year)
}) |> bind_rows() |>
rename(
  population_total_num = population,
  population_total_density = population_density,
  population_total_dispersion = dispersion
)

# urban population
rasters <- data_uri("HYDE", "urban_population.nc") |> terra::rast()
urban_pop_grid_stats <- map(YEARS, function(year) {
  progress_message("Aggregating urban population for year {year}")
  calculate_population_grid_stats(rasters, year)
}) |> bind_rows() |>
rename(
  population_urban_num = population,
  population_urban_density = population_density,
  population_urban_dispersion = dispersion
)

# rural population
rasters <- data_uri("HYDE", "rural_population.nc") |> terra::rast()
rural_pop_grid_stats <- map(YEARS, function(year) {
    progress_message("Aggregating rural population for year {year}")
    calculate_population_grid_stats(rasters, year)
}) |> bind_rows() |>
rename(
  population_rural_num = population,
  population_rural_density = population_density,
  population_rural_dispersion = dispersion
)

# combine the data
pop_stats  <- pop_grid_stats |>
  full_join(urban_pop_grid_stats, join_by(cell_id, year)) |>
  full_join(rural_pop_grid_stats, join_by(cell_id, year))

write_artifact(pop_stats)
