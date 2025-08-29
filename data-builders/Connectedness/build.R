progress_message("Calculating road density (GRIP4)")

roads_grid_stats <- calculate_pathway_grid_stats("roads", st_read(
  data_uri("GRIP4", "GRIP4_GlobalRoads.gdb"),
  # major roads (highways, primary, and secondary importance), open
  query = "SELECT GP_RTP, GP_RAV, GP_RRG from GRIP4_GlobalRoads where GP_RTP <= 3 AND GP_REX = 1",
  quiet = TRUE
))

progress_message("Calculating railroad density (NaturalEarth)")

railways_grid_stats <- calculate_pathway_grid_stats("railways", c(
  read_ne_geometry("ne_10m_railroads"),
  read_ne_geometry("ne_10m_railroads_north_america")
))


progress_message("Estimating navigable rivers (HydroRIVERS)")

rivers_grid_stat <- calculate_pathway_grid_stats("rivers_min_st6", st_read(
  data_uri("HydroRIVERS", "HydroRIVERS_v10.gdb"),
  # as proxy for navigability we take the rivers with Strahler number 6 or higher
  # this results in a global river network of ~ 700'000km, close to the 623'000 estiamted by
  # OECD 2018 report (Beyer 2018)
  query = "SELECT ORD_STRA, ORD_CLAS, ORD_FLOW from HydroRIVERS_v10 where ORD_STRA >= 6",
  quiet = TRUE
))

progress_message("Estimating major lake shorelines (HydroLAKES)")
lakes_grid_stat <- calculate_pathway_grid_stats("lakes_min_10km2_shore", local({
  st_read(
    data_uri("HydroLAKES", "HydroLAKES_polys_v10.gdb"),
    # as we are interested in lakes that connect communities, we take lakes with area of 10km2 or larger
    query = "SELECT Lake_area, Shore_len, Depth_avg from HydroLAKES_polys_v10 where Lake_area >= 10",
    quiet = TRUE
  ) |>
  make_valid_s2_geography() |>
  s2_boundary()
}))

progress_message("Estimating coastline")

coastline_grid_stat <- calculate_pathway_grid_stats("coastline", local({
  st_read(
    data_uri("NaturalEarth", "ne_10m_land"),
    quiet = TRUE
  ) |>
  make_valid_s2_geography() |>
  s2_boundary()
}))

progress_message("Aggregating travel and trade infrastructure")

infrastructure_grid_stats <- local({
  # sea and airports (NaturalEArth)
  seaports <- read_ne_geometry("ne_10m_ports")
  airports <- read_ne_geometry("ne_10m_airports")

  # maritime chockepoints
  chokepoints <- data_uri("Chokepoints", "Chokepoints.csv") |>
    read_csv(show_col_types = FALSE) |>
    with_data(s2_geog_point(lon, lat))

  # construct the summaries
  tibble(
    cell_id = grid$cell_id,
    airports_num = mc_map_vec(grid$cell_geometry, function(g) {
      s2_intersects_matrix(g, airports) |> list_sizes()
    }),
    seaports_num = mc_map_vec(grid$cell_geometry, function(g) {
      s2_intersects_matrix(g, seaports) |> list_sizes()
    }),
    chokepoint_nearest_dist_km = mc_map_vec(grid$cell_centroid, function(centroid) {
      closest_idx <- s2_closest_feature(centroid, chokepoints)
      s2_distance(centroid, chokepoints[closest_idx])
    })
  )
})

# combine the data
connectedness_grid_stats <- roads_grid_stats |>
  full_join(railways_grid_stats, join_by(cell_id)) |>
  full_join(rivers_grid_stat, join_by(cell_id)) |>
  full_join(lakes_grid_stat, join_by(cell_id)) |>
  full_join(coastline_grid_stat, join_by(cell_id)) |>
  full_join(infrastructure_grid_stats, join_by(cell_id))

write_artifact(connectedness_grid_stats)
