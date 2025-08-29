generate_ivea_grid <- function(
  level,
  adjacency = TRUE,
  subzones = 0,
  refinement = 10,
  tesselate = FALSE
) {
  model <- "IVEA3H"
  rlang::is_scalar_logical(adjacency) || rlang::abort("'adjacency' must be a boolean scalar")
  rlang::is_scalar_integerish(subzones) || rlang::abort("'subzones' must be an integer scalar")
  rlang::is_scalar_integerish(refinement) || rlang::abort("'refinement' must be an integer scalar")
  rlang::is_scalar_integerish(level) || rlang::abort("'level' must be an integer scalar")

  # invoke the tool
  json_file <- withr::local_tempfile(fileext = ".jsonl")
  status <- system2("ivea-grid", c(
    paste("--type", model),
    paste("--level", level),
    paste("--refinement", refinement),
    paste("--subzones", subzones),
    if (isFALSE(adjacency)) "--no-adjacency",
    if (isTRUE(tesselate)) "--tesselate"
  ), stdout = json_file)

  (status == 0L) || error("error invoking `generate-grid`")

  TBL_CLS <- c("tbl_df", "tbl", "data.frame")

  grid <- yyjsonr::read_ndjson_file(json_file) |>
    as_tibble() |>
    select(
      cell_id = id,
      cell_shape = shape,
      cell_centroid = centroid,
      adj_cells = any_of("neighbors"),
      subcells = any_of("subzones"),
      subtriangles = any_of("subtriangles"),
      cell_geometry = geometry
    )

  # centroids are specified as [lon, lat]
  grid$cell_centroid <- local({
    xy <- unlist(grid$cell_centroid)
    n <- length(xy)

    # bulk-init centroid points
    s2_geog_point(xy[seq.int(1L, n, by = 2L)], xy[seq.int(2L, n, by = 2L)])
  })

  # edges are specified as [lon0, lat0, lon1, lat1]
  if (has_name(grid, "adj_cells")) grid$adj_cells <- mc_map(grid$adj_cells, function(X) {
    names(X) <- c("adj_cell_id", "adj_boundary")
    class(X) <- TBL_CLS

    xy <- unlist(X$adj_boundary)
    n <- length(xy)

    X$adj_boundary <- s2_make_line(
      xy[seq.int(1L, n, by = 2L)],
      xy[seq.int(2L, n, by = 2L)],
      feature_id = rep(seq_len(nrow(X)), each = 2L)
    )

    X
  })

  # convert subcells to tibbles so that unnest does the right thing
  if (has_name(grid, "subcells")) grid$subcells <- mc_map(grid$subcells, function(x) {
    new_data_frame(list(subcell_id = x), class = TBL_CLS)
  })

  if (has_name(grid, "subtriangles")) grid$subtriangles <- map(seq_along(grid$subtriangles), function(i) {
    xy <- grid$subtriangles[[i]]
    n_triangles <- nrow(xy) %/% 3
    tryCatch(s2_make_polygon(xy$lon, xy$lat, feature_id = rep(seq_len(n_triangles), each = 3L)), error = function(cnd) {
      xy <- mutate(xy, id = rep(seq_len(n_triangles), each = 3L)) |> as_tibble()
      print(xy, n = Inf)
      print(cnd)
      print(as_tibble(grid$cell_geometry[[i]]))
      abort("error!")
    })
  })


  # geometry is specified as df(lon, lat)
  grid$cell_geometry <- mc_vec_apply(grid$cell_geometry, function(X) {
    sizes <- list_sizes(X)
    xy <- list_unchop(X)

    # bulk-create cell polygons
    s2_make_polygon(xy$lon, xy$lat, feature_id = rep.int(seq_along(sizes), sizes))
  })

  grid
}
