mc_raster_extract <- function(
  raster,
  geometry,
  ...,
  .chunk_size = 100000L,
  .n_cores = n_available_cores()
) {
  # split the data into chunks which we will process sequentilly
  # overloading the spawned processes sometimes locks up, and limiting
  # helps prevent this
  chunks <- vec_chunk(geometry, .chunk_size = .chunk_size)
  out <- vector("list", length(chunks))

  for (i in seq_along(chunks)) {
    out[[i]] <- mc_vec_apply(chunks[[i]], .n_cores = .n_cores, function(g) {
      exactextractr::exact_extract(
        raster,
        g,
        ...,
        force_df = TRUE,
        progress = FALSE
      ) |> as_tibble()
    })
  }

  list_unchop(out)
}

mc_s2_intersection <- function(
  x,
  y,
  options = s2_options(),
  ...,
  .chunk_size = NULL,
  .n_cores = n_available_cores()
) {
  xy <- vec_recycle_common(x, y)
  x <- xy[[1]]
  y <- xy[[2]]
  n <- length(x)

  mc_vec_apply(seq_along(x), .chunk_size = .chunk_size, .n_cores = .n_cores, function(i) {
    s2_intersection(x[i], y[i], options = options)
  })
}

mc_s2_intersection_area <- function(
  x,
  y,
  options = s2_options(),
  ...,
  .chunk_size = NULL,
  .n_cores = n_available_cores()
) {
  xy <- vec_recycle_common(x, y)
  x <- xy[[1]]
  y <- xy[[2]]
  n <- length(x)

  mc_vec_apply(seq_along(x), .chunk_size = .chunk_size, .n_cores = .n_cores, function(i) {
    s2_area(s2_intersection(x[i], y[i], options = options))
  })
}

mc_s2_intersects <- function(
  x,
  y,
  options = s2_options(),
  ...,
  .chunk_size = NULL,
  .n_cores = n_available_cores()
) {
  xy <- vec_recycle_common(x, y)
  x <- xy[[1]]
  y <- xy[[2]]
  n <- length(x)

  mc_vec_apply(seq_len(n), .chunk_size = .chunk_size, .n_cores = .n_cores, function(i) {
    s2_intersects(x[i], y[i], options = options)
  })
}

mc_s2_union <- function(
  x,
  y,
  options = s2_options(),
  ...,
  .chunk_size = NULL,
  .n_cores = n_available_cores()
) {
  if (missing(y)) {
    mc_vec_apply(x, .chunk_size = .chunk_size, .n_cores = .n_cores, function(x) {
      s2_union(x, options = options)
    })
  } else {
    xy <- vec_recycle_common(x, y)
    x <- xy[[1]]
    y <- xy[[2]]
    n <- length(x)

    mc_vec_apply(seq_len(n), .chunk_size = .chunk_size, .n_cores = .n_cores, function(i) {
      s2_union(x[i], y[i], options = options)
    })
  }
}

mc_s2_locate_intersections <- function(
    x,
    y,
    options = s2_options(),
    ...,
    .unnest = TRUE,
    .chunk_size = NULL,
    .n_cores = n_available_cores()
) {
  y <- force(y)

  mc_vec_apply(vec_seq_along(x), .chunk_size = .chunk_size, .n_cores = .n_cores, function(i) {
    loc <- s2_intersects_matrix(vec_slice(x, i), y, options = options)

    if (is_true(.unnest)) {
      tibble(x = rep(i, list_sizes(loc)), y = unlist(loc))
    } else {
      loc
    }
  })
}

s2_as_sfc <- function(geometry, ..., .crop = 1e-5) {
  opts <- s2_options(dimensions = "polygon")
  crop <- s2_world_plate_carree(.crop, .crop)

  geometry |>
    mc_s2_intersection(crop, options = opts) |>
    st_as_sfc()
}


raster_fits_in_memory <- function(raster) {
  terra::mem_info(raster, print = FALSE)[["fits_mem"]] != 0.0
}

make_valid_s2_geography <- function(geo, ..., dimensions = "polygon", simplify_dist = NULL) {
  s2_opts <- s2_options(
    simplify_edge_chains = TRUE,
    snap_radius = if (!is_null(simplify_dist)) tolerance/s2_earth_radius_meters() else -1,
    split_crossing_edges = TRUE,
    duplicate_edges = FALSE,
    validate = FALSE,
    edge_type = "directed",
    dimensions = dimensions
  )

  suppressWarnings(geo <- as_s2_geography(geo, check = FALSE))

  mc_vec_apply(geo, function(g) {
    g |>
    # these steps are nessesary to fix the orientation of polygons
    # see https://github.com/r-spatial/s2/issues/267
    s2_rebuild(options = s2_opts) |>
    s2_as_binary() |>
    as_s2_geography() |>
    s2_rebuild(options = s2_options(validate = TRUE, dimensions = dimensions))
  })
}


segmentize_linestring <- function(sf) {
  UseMethod("segmentize_linestring")
}

segmentize_linestring.default <- function(sf) {
  stop("line geometry expected")
}

segmentize_linestring.GEOMETRYCOLLECTION <- function(sf) {
  if (length(sf) == 0L) return(s2_geography())
  stop("line geometry expected")
}

segmentize_linestring.LINESTRING <- function(sf) {
  if ((nrow(sf) %||% 0L) == 0L) return(s2_geography())

  x <- sf[, 1L]
  y <- sf[, 2L]

  # duplicate vertices to split the segments
  x <- vec_interleave(x, c(x[-1], x[[1]]))
  y <- vec_interleave(y, c(y[-1], y[[1]]))

  s2_make_line(x, y, rep(seq_len(nrow(sf)), each = 2L))
}

segmentize_linestring.MULTILINESTRING <- function(sf) {
  s2:::new_s2_geography(unlist(map(unclass(sf), segmentize_linestring.LINESTRING)))
}
