progress_message("Pre-processing the country list")

countries <- cached({
  data_uri("CShapes", "CShapes-2.0.csv.gz") |>
  readr::read_csv(show_col_types = FALSE) |>
  # discard all data before YEARS coverage
  filter(gweyear >= min(YEARS)) |>
  mutate(gwsyear = pmax(gwsyear, min(YEARS))) |>
  # parse the polygons
  mutate(the_geom = {
    s2_geog_from_text(the_geom, check = FALSE) |>
    s2_union(options = s2_options(dimensions = "polygon"))
  }) |>
  mutate(
    capital_location = s2_geog_point(caplong, caplat)
  )
})

# merge with isocode data
countries <- left_join(countries, country_codes, join_by(gwcode))

# Gleditsch & Ward use the same code for both Soviet Union and Russia
# make sure that Soviet union gets the proper isocode
#
# Russia became an independent country in December 1991
countries <- local({
  ussr_cuttoff_date <- filter(countries, gwcode == 365, gwsyear == 1991)$gwsdate |> max()
  ussr_at <- which(with_data(countries, gwcode == 365 & gwsdate < ussr_cuttoff_date))

  mutate(countries,
    name = vec_assign(name, ussr_at, "Union of Soviet Socialist Republics"),
    isocode = vec_assign(isocode, ussr_at, "SUN")
  )
})

# track the country state at the end of the year
countries <- countries |>
# take the last change for the given year
filter(
  gwsdate == max(gwsdate),
  .by = c(isocode, gwsyear)
) |>
# crop the end year (set the last year to a year before the next change to avoid overlaps)
mutate(
  gweyear = if_else((gweyear %in% gwsyear) & (gweyear != gwsyear), gweyear - 1L, gweyear),
  .by = isocode
) |>
mutate(
  year = map2(gwsyear, gweyear, seq.int)
) |>
select(
  name, isocode, gwcode, year, capital_location, geometry = the_geom
)

progress_message("Mapping the countries to grid")

countries_grid <- local({
  loc <- mc_s2_locate_intersections(grid$cell_geometry, countries$geometry)

  tibble(
    cell_id = grid$cell_id[loc[[1L]]],
    cell_geometry = grid$cell_geometry[loc[[1L]]],
    vec_slice(countries, loc[[2L]])
  ) |>
  select(
    cell_id,
    year,
    country_name = name,
    country_isocode = isocode,
    country_gwcode = gwcode,
    capital_location,
    cell_geometry,
    country_geometry = geometry
  )
}) |>
mutate(
  country_geometry = mc_s2_intersection(cell_geometry, country_geometry),
  country_area_km2 = s2_area(country_geometry) / 1e6,
  country_capital_dist_km = s2_distance(cell_geometry, capital_location)/1e3
) |>
select(!cell_geometry & !capital_location) |>
relocate(country_geometry, .after = last_col())

write_artifact(countries_grid, "CountriesGridGeometry")

progress_message("Aggregating countries")

countries_aggregated_grid <- select(countries_grid,
  !country_geometry
) |>
unnest(
  year
) |>
nest(
  countries = starts_with("country_")
) |>
mutate(
  countries_num = list_sizes(countries),
  # largest country in the cell, defined as having at least
  # the same or higher area than all other countries in the same cell
  map_vec(countries, function(X) {
    if (nrow(X) == 1L) return(X)

    areas <- X$country_area_km2
    idx <- which.max(areas)
    idx <- if_else(areas[idx] >= sum(areas[-idx]), idx, NA_integer_)

    vec_slice(X, idx)
  }) |> rename_with(str_replace, everything(), "^country_", "country_largest_"),
  .before = countries
)


write_artifact(countries_aggregated_grid)
