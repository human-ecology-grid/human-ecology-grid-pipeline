# NaturalEarth landmas data as a single polygon, used to calculate land coverage
landmass_data_uri <- data_uri("NaturalEarth", "ne_10m_land")
countries_data_uri <- data_uri("NaturalEarth", "ne_10m_admin_0_countries")


landmass <- cached({
  st_read(landmass_data_uri, quiet = TRUE) |>
  s2_union_agg()
})

landmass_sans_antarctica <- cached({
  antarctica <- st_read(countries_data_uri, quiet = TRUE) |>
    as_tibble() |>
    filter(CONTINENT == "Antarctica") |>
    pull(geometry) |>
    s2_union_agg()

  s2_difference(landmass, antarctica)
}, landmass)
