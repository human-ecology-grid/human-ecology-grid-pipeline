#' Locate the data root for the Human Ecology Grid database
#'
#' @param resolution target grid resolution (optional)
#' @return string path to the data directory
#'
#' This function attempts to auto-detect the root directory based on
#' the path to the support script. To set up the root manually, set
#' `option(human.ecology.grid.root = <path>)`
human_ecology_grid_root <- local({
  nzchar(system.file(package = "rlang")) || stop("package `rlang` is required")
  rlang::check_installed("tibble")
  rlang::check_installed("arrow")
  rlang::check_installed("sf")

  # TODO: better check
  check_db_root <- function(path) {
    path <- file.path(path, "scripts", "human-ecology-grid.R")

    file.exists(path) # add hash check
  }

  auto_db_root <- NULL

  # retrieve the path to the current script by matching the argument source(file)
  for (frame_idx in rev(seq_len(sys.nframe()))) {
    call_fn <- sys.function(frame_idx)
    if (!identical(call_fn, base::source) && !identical(call_fn, base::sys.source)) next

    # match the call and evaluate the file argument
    parent_env <- sys.frame(sys.parents()[frame_idx])
    call <- match.call(call = sys.call(frame_idx), definition = call_fn, envir = parent_env)
    path <- eval(call$file, parent_env)
    path <- tryCatch(normalizePath(path), error = function(...) NULL)

    # check the path
    path <- dirname(dirname(path))
    if (!check_db_root(path)) next

    auto_db_root <- path
    break
  }

  function(..., resolution) {
    rlang::check_dots_empty()
    root <- getOption("human.ecology.grid.root", auto_db_root)

    if (!check_db_root(root)) {
      if (!is.null(getOption("human.ecology.grid.root"))) rlang::abort(c(
        "invalid HumanEcologyGrid data root",
        i = sprintf("`options(human.ecology.grid.root = \"%s\")` is invalid", root)
      )) else rlang::abort(c(
        "unable to auto-detect HumanEcologyGrid data root",
        i = "please set `options(human.ecology.grid.root = \"...\")`"
      ))
    }

    if (missing(resolution)) return(root)
    rlang::is_integerish(resolution, n = 1L, finite = TRUE) || rlang::abort(c(
      "`resolution` must be an integer grid resolution in km",
      i = sprintf("got `%s`", rlang::as_label(resolution))
    ))


    root <- file.path(root, sprintf("grid-%skm", resolution))
    dir.exists(root) || rlang::abort(c(
      sprintf("unable to locate HumanEcologyGrid data for `resolution=%s`", resolution),
      i = sprintf("path \"%s\" does not exist", root)
    ))

    root
  }
})

#' Load the Human Ecology Grid cell geometry
#'
#' @param resolution grid resolution
#' @param result_type type of the result (SimpleFeatures or tibble)
#' @param split_meridian longitude to split the grid by (optional, for plotting or subsequent processing)
#' @result a SimpleFeatures object (`result_type = "sf"`) or a tibble (`result_type = "tbl"`)
human_ecology_grid_cells <- function(resolution, ..., split_meridian = 180, result_type = c("sf", "tbl")) {
  # check the arguments
  root <- human_ecology_grid_root(resolution = resolution)
  result_type <- rlang::arg_match(result_type)
  is.null(split_meridian) ||
  (rlang::is_bare_numeric(split_meridian, 1L) && (abs(split_meridian <= 180))) || rlang::abort(c(
    "`split_meridian` should be a longitude between -180.0 and 180.0",
    i = sprintf("got `%s`", rlang::as_label(split_meridian))
  ))

  is.null(split_meridian) || rlang::is_installed("s2") || rlang::abort(
    "package `s2` is required to split by meridian"
  )

  # load the file
  geojson_path <- file.path(root, "grid.geojson.xz")
  cells <- tryCatch({
    text <- if (rlang::is_installed("readr")) {
      readr::read_file(geojson_path)
    } else {
      readLines(geojson_path, warn = FALSE, encoding = "UTF-8")
    }

    # parse the file
    if (rlang::is_installed("yyjsonr")) {
      opts <- yyjsonr::opts_read_geojson(property_promotion = "list")
      yyjsonr::read_geojson_str(text, opts = opts)
    } else {
      sf::st_read(dsn = text, drivers == "GeoJSON")
    }
  }, error = function(cnd) {
   rlang::abort(c(
     sprintf("unable to load GeoJSON at \"%s\"", geojson_path),
     i = cnd$message
   ))
  })

  # split the meridian
  if (!is.null(split_meridian)) {
    # build a thin polygon representing the split meridian
    tol <- 0.01
    meridian_geometry <- s2::s2_make_polygon(
      c(split_meridian + tol, split_meridian - tol, split_meridian - tol, split_meridian + tol),
      c(90.0, 90.0, -90.0, -90.0),
      oriented = TRUE
    )
    opts <- s2::s2_options(dimensions = "polygon")

    cell_geometry <- s2::as_s2_geography(cells$geometry)
    cells$geometry <- sf::st_as_sfc(s2::s2_difference(cell_geometry, meridian_geometry, options = opts))
  }

  if (result_type == "tbl") {
    cells <- tibble::as_tibble(cells)
  }

  cells
}

#' Load the Human Ecology Grid data
#'
#' @param resolution grid reslution
#' @param year data year ("latest" for most recent cross-sectional dat, "all" for full panel data)
#' @param result_type type of the result (tibble or Arrow Dataset)
#' @return A tibble (`result_type = "tbl"`) or an Arrow Dataset (`result_type = "arrow"`)
human_ecology_grid_data <- function(resolution, ..., year = c("latest", "all"), result_type = c("tbl", "arrow")) {
  root <- human_ecology_grid_root(resolution = resolution)
  year <- rlang::arg_match(year)
  result_type <-  rlang::arg_match(result_type)

  # load the relevant dataset
  data_uri <- switch(year,
    latest = file.path(root, "cross-section-latest.parquet"),
    all = file.path(root, "yearly")
  )
  data <- arrow::open_dataset(data_uri)

  # postprocess
  if (result_type == "tbl") data <- tibble::as_tibble(data)

  data
}

#' Load the Human Ecology Grid cell adjacency matrix
#'
#' @param resolution grid reslution
#' @param data data frame with a column `cell_id` (optional, to subset the matrix)
#' @return A SparseMatrix matrix
human_ecology_grid_adjacency_matrix <- function(resolution, ..., data) {
  require(Matrix, quietly = TRUE)

  root <- human_ecology_grid_root(resolution = resolution)
  missing(data) || is.data.frame(data) || rlang::abort("`data` must be a table")
  missing(data) || rlang::has_name(data, "cell_id") || rlang::abort("`data` must have a column `cell_id`")


  mat <- readRDS(file.path(root, "extra", "grid-adjacency-matrix.rds"))

  if (!missing(data)) {
    mat <- mat[match(data$cell_id, rownames(mat)), match(data$cell_id, colnames(mat))]

    # ensure that the order matches
    stopifnot(identical(data$cell_id, rownames(mat)))
    stopifnot(identical(data$cell_id, colnames(mat)))
  }

  mat
}

invisible(NULL)
