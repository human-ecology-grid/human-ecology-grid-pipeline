# utility to read NaturalEearth geometry
read_ne_geometry <- function(dataset) {
  data_uri("NaturalEarth", dataset) |>
    st_read(quiet = TRUE) |>
    as_s2_geography()
}


calculate_pathway_grid_stats <- function(prefix, pathways) {
  # we need to compute the pathway geometry overlapping each cell grid
  #
  # as the pathway data is compiled from various regional datasets we first smooth
  # out the polygonal chains using a filter of 50 meters; this helps to harmonize
  # segments collected using different spatial resolutions
  #
  # pathways vector be rather large, so we will chunk it and reduce in steps
  # chunk size of 100000L seems to work well enough for machines with 4GB per core at the
  # tested grid resolution
  progress_message("({prefix}) Simplifying geometry")

  pathways <- {
    pathways |>
    as_s2_geography(check = FALSE) |>
    s2_rebuild(options = s2_options(
      snap_radius = 50/s2_earth_radius_meters(),
      simplify_edge_chains = TRUE,
      dimensions = "polyline"
    ))
  }

  pathways <- vec_chunk(pathways, .chunk_size = 100000L)
  pathways_per_cell <- NULL

  chunk_i <- 0L

  # process the chunks and reduce
  for (pathways_chunk in pathways) {
    chunk_i <- chunk_i + 1L
    if (length(pathways) > 1L) progress_message("({prefix}) Processing chunk {chunk_i}/{length(pathways)}")

    chunk_overlaps_per_cell <- mc_vec_apply(grid$cell_geometry, function(g) {
      # locate overlapping elements and aggregate
      overlaps <- s2_intersects_matrix(g, pathways_chunk)
      overlaps <- map2(g, overlaps, function(g, ii) {
        s2_intersection(g, pathways_chunk[[ii]]) |>
        s2_union_agg(options = s2_options(dimensions = "polyline"))
      })

      # work around the issue with concatenating s2 vectors
      s2:::new_s2_geography(unlist(overlaps))
    })

    # reduce with previous results
    pathways_per_cell <- if (!is_null(pathways_per_cell)) {
      mc_s2_union(pathways_per_cell, chunk_overlaps_per_cell, options = s2_options(dimensions = "polyline"))
    } else {
      chunk_overlaps_per_cell
    }
  }

  # calculate the stats
  grid_pathway_stats <- mutate(grid,
    pathways_geo = pathways_per_cell,
    # total pathway length
    length_km = s2_length(pathways_geo) / 1e3,
    # pathway density (relative to land area), km/km2
    density = length_km / cell_land_area_km2,
    # dispersion as Gini coefficient of subcell pathway densities
    dispersion = map2_vec(pathways_geo, subcells, function(g, cells)  {
      gini(
        s2_length(s2_intersection(g, cells$subcell_geometry)) /
        cells$subcell_land_area_km2
      )
    })
   ) |>
  select(
    cell_id,
    length_km,
    density,
    dispersion
  ) |>
  rename_with(
    .cols = !cell_id,
    function(var) paste0(prefix, "_", var)
  )

  grid_pathway_stats
}
