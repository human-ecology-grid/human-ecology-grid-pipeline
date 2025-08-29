build_outputs <- function(pipeline) {
  # auxiliary functions
  load_artifact <- function(theme) {
    # verify artifact checksums
    manifest <- read_stage_manifest(pipeline, list(name = theme))
    artifact_hash <- hash_files(locate_stage_artifacts(pipeline, theme))

    identical(manifest$artifact, artifact_hash) || error(
      "Artifact fingerprint mismatch for {.field {theme}}",
      i = "please re-run the pipeline"
    )

    check_errors(data <- qs2::qs_read(get_artifact_path(pipeline, theme))) || error(
      "Unable to load required output artifact {.field {theme}}"
    )

    # no observation should be partial
    !vec_any_missing(select(data, -any_of(c("cell_id", "year")))) || error(
      "artifact {.field {theme}} contains missing data rows"
    )

    # all columns should be unnamed
    is_unnamed <- map_lgl(unclass(data), function(col) is_null(names(col)))
    all(is_unnamed) || error(
      "artifact {.field {theme}} contains named columns",
      i = "{.field {names(data)[!is_unnamed]}}"
    )

    # validate the data agains the schema (protects against partial runs)
    if (theme == "Grid") return(data)

    schema <- filter(pipeline$data_builders, name == theme)$schema[[1]]
    checked_table_data(data, schema, .error_msg = format_inline("error validating artifact {.var {theme}}"))
  }

  build_grid_adjacency_matrix <- function(grid) {
    # build the adjacency index (representing a triangle in a symmetric matrix)
    adj <- select(grid, cell_id, adj_cells) |>
      unnest(adj_cells) |>
      select(cell_id, adj_cell_id) |>
      mutate(i = match(cell_id, grid$cell_id), j = match(adj_cell_id, grid$cell_id)) |>
      filter(i < j)

    # convert the adjacency index into a sparse adjacency matrix
    n <- nrow(grid)
    id <- grid$cell_id
    Matrix::sparseMatrix(
      adj$i,
      adj$j,
      x=1.0,
      dims = c(n, n),
      dimnames = list(id, id),
      symmetric = TRUE
    )
  }

  calc_outputs_fingerprint <- function() {
    # main artifacts
    artifact_files <- fs::dir_ls(fs::path(pipeline$staging_dir, "artifacts"), type = "file", glob = "*.qs2")
    product_files <- if (fs::dir_exists(outputs_dir)) {
      fs::dir_ls(outputs_dir, type = "file", recurse = TRUE, all = FALSE)
    } else {
      character()
    }
    product_files <- setdiff(product_files, fingerprint_path)

    hash_files(sort(c(artifact_files, product_files)))
  }

  # configure the output locations
  outputs_dir <- fs::path(pipeline$staging_dir, "outputs")
  fingerprint_path <- fs::path(outputs_dir, "fingerprint.txt")
  progress_context(prefix = cli::col_green(sprintf("[%skm] ", pipeline$config$grid$resolution)))

  # check the fingerprint to determine if rebuild is needed
  fingerprint <- tryCatch(readr::read_file(fingerprint_path), error = function(...) NULL)
  if (identical(fingerprint, calc_outputs_fingerprint())) {
    progress_message()
    cli_alert_success("{cli::col_green(sprintf('[%skm]', pipeline$config$grid$resolution))} Output is up to date")

    return(fingerprint)
  }

  t0 <- bench::hires_time()

  # otherwise, rebuild is needed
  check_errors(fs::dir_delete(outputs_dir))
  fs::dir_create(outputs_dir)
  fs::dir_create(fs::path(outputs_dir, "extra"))
  fs::dir_create(fs::path(outputs_dir, "yearly"))

  grid_file <- fs::path(outputs_dir, glue("grid.geojson.xz"))
  grid_adj_matrix_file <- fs::path(outputs_dir, "extra", glue("grid-adjacency-matrix.rds"))
  data_latest_file <- fs::path(outputs_dir, glue("cross-section-latest.parquet"))
  data_yearly_dir <- fs::path(outputs_dir, "yearly")

  # load all artifact files, in order
  progress_message("Loading artifacts")
  artifacts <- map(pipeline$config$theme_order, load_artifact)

  # write the grid data
  progress_message("Writing grid data")
  grid <- load_artifact("Grid")

  select(grid, cell_id, cell_is_ocean_ice, adj_cells, cell_geometry) |>
    mutate(adj_cells = map(adj_cells, function(X) select(X, adj_cell_id, adj_boundary_ocean))) |>
    sf::st_as_sf() |>
    yyjsonr::write_geojson_str() |>
    write_file(grid_file)

  build_grid_adjacency_matrix(grid) |>
    saveRDS(grid_adj_matrix_file)

  progress_message("Writing annual dataset")
  # determine the longitudinal coverage
  year_range <- artifacts |>
    map(function(X) unique(X[["year"]]) %||% integer()) |>
    reduce(vec_set_union)
  year_range <- seq.int(min(year_range), max(year_range))

  # build the longitudinal dataset
  yearly_data <- artifacts |>
  map(function(X) {
    if (!has_name(X, "year")) {
      expand_grid(X, year = year_range) |> relocate(year, .after = cell_id)
    } else {
      X
    }
  }) |>
  reduce(function(x, y) {
    full_join(x, y, by = c("cell_id", "year"))
  })

  # write out the files using Hive partitioning
  for (y in year_range) {
    p <- fs::path(data_yearly_dir, sprintf("year=%s", y))
    fs::dir_create(p)

    filter(yearly_data, year == y) |>
    arrange(cell_id) |>
    arrow::write_parquet(fs::path(p, "data.parquet"))
  }
  yearly_data <- NULL

  progress_message("Writing cross-sectional (latest) dataset")

  artifacts |>
  map(function(X) {
    if (has_name(X, "year")) {
      filter(X, year == max(year)) |> select(!year)
    } else {
      X
    }
  }) |>
  reduce(function(x, y) {
    full_join(x, y, by = c("cell_id"))
  }) |>
  arrow::write_parquet(data_latest_file)

  # # calculate cell coverage
  # map(artifacts, function(X) {
  #   group <- if(has_name(X, "year")) "year"

  #   summarize(X,
  #     across(!cell_id, function(x) {
  #       sum(!vec_detect_missing(x))/nrow(grid)
  #     }),
  #     .by = !!group
  #   ) |>
  #   select(
  #     !any_of("year")
  #   ) |>
  #   summarize(
  #     across(everything(), mean)
  #   ) |>
  #   pivot_longer(
  #     everything(),
  #     names_to = "var",
  #     values_to = "cell_coverage"
  #   )
  # }) |>
  # bind_rows() |>
  # write_csv(data_cell_coverage_file)

  # write the fingerprint
  fingerprint <- calc_outputs_fingerprint()
  readr::write_file(fingerprint, fingerprint_path)

  progress_message()
  cli_alert_success("{cli::col_green(sprintf('[%skm]', pipeline$config$grid$resolution))} Assembled output {format_elapsed(from = t0)}")

  fingerprint
}

get_pipeline_fingerprint <- function(pipeline) {
  list(
    data = {
      pipeline$data_sources |>
      arrange(name) |>
      pull(fingerprint) |>
      paste0(collapse = "#") |>
      hash()
    },
    code = {
      pipeline$data_builders |>
      arrange(name) |>
      pull(fingerprint) |>
      paste0(collapse = "#") |>
      hash()
    }
  )
}

write_product_metadata <- local({
  # openxlsx worksheet helper — forces all columns to be formatted
  add_xlsx_worksheet <- function(wb, name, df, ..., .header_style) {
    # collect the column specs
    cols <- list2(...)
    identical(names(cols), names(df)) || error(
      "invalid column specification",
      i = "expected {.field {names(df)}}",
      i = "got {.field {names(cols)}}"
    )

    .header_style <- exec(openxlsx::createStyle, !!!.header_style)

    # write the worksheet
    openxlsx::addWorksheet(wb, sheetName = name, gridLines = FALSE)
    openxlsx::writeDataTable(wb, sheet = name, x = df, colNames = TRUE, tableStyle = "none", headerStyle = .header_style)

    # format the columns
    for (idx in seq_along(cols)) {
      # split the style spec
      spec <- cols[[idx]]
      width <- spec[["width"]] %||% 25
      spec$width <- NULL

      # create column style
      style <- exec(openxlsx::createStyle, !!!spec)
      openxlsx::addStyle(wb, sheet = name, cols = idx, rows = seq_len(nrow(df)) + 1, style = style)

      # set column width
      openxlsx::setColWidths(wb, sheet = name, cols = idx, widths = width)
    }
  }

  # format text value
  fmt_cell_text <- function(x) {
    str_trim(gsub("\\\\n", "\n", x))
  }

  build_variable_metadata <- function(pipeline) {
    # select in theme order
    pipeline$data_builders |>
    filter(name != "Grid") |>
    arrange(match(name, pipeline$config$theme_order)) |>
    # format the field info
    select(theme = name, schema) |>
    unnest(schema) |>
    filter(!is.na(description_short)) |>
    mutate(data_source = map_chr(data_source, paste0, collapse = ", ")) |>
    mutate(description_short = fmt_cell_text(description_short)) |>
    mutate(description_temporal = fmt_cell_text(description_temporal)) |>
    mutate(description_detail = fmt_cell_text(description_detail)) |>
    select(
      Variable = field,
      Theme = theme,
      Description = description_short,
      `Temporal Coverage` = description_temporal,
      Units = units,
      Aggregation = aggregation,
      `Data Source` = data_source,
      Details = description_detail
    )
  }

  build_data_sources_metadata <- function(pipeline) {
    pipeline$data_sources |>
    mutate(
      description = fmt_cell_text(description),
      BibTeX = map_chr(cite, function(cite) {
        filter(pipeline$references, key %in% cite) |>
        pull(bibentry) |>
        reduce(c) |>
        toBibtex() |>
        paste(collapse = "\n")
      }),
      Citation = map_chr(cite, function(cite) {
        filter(pipeline$references, key %in% cite) |>
        pull(bibentry) |>
        reduce(c) |>
        format() |>
        paste(collapse = "\n\n")
      }),
      Citekey = map_chr(cite, function(x) {
        paste0("@", x, collapse = ", ")
      })
    ) |>
    select(
      `Data Source` = name,
      Description = description,
      License = license,
      Citekey,
      Citation,
      BibTeX
    )
  }

  # write_product_metadata
  function(pipeline, out_path) {
    # common style
    style0 <- list(
      border = "TopBottomLeftRight",
      borderStyle = "thin",
      borderColour = "#B3B3B3",
      wrapText = TRUE,
      valign = "top"
    )

    # header style
    style_hdr <- list(textDecoration = "Bold", fgFill = "#DEDEDE")

    # build the excel file
    wb <- openxlsx::createWorkbook()
    openxlsx::modifyBaseFont(wb, fontSize = 10, fontName = "Helvetica Neue")

    # write the variable metadata
    variables <- build_variable_metadata(pipeline)
    add_xlsx_worksheet(wb, "Variables", select(variables, !Details),
      .header_style       = list_assign(style0, !!!style_hdr),
      Variable            = list_assign(style0, !!!style_hdr, width = 40),
      Theme               = list_assign(style0, width = 15),
      Description         = list_assign(style0, width = 50, wrapText = TRUE),
      `Temporal Coverage` = list_assign(style0, width = 35, wrapText = TRUE),
      Units               = list_assign(style0, width = 10),
      Aggregation         = list_assign(style0, width = 15),
      `Data Source`       = list_assign(style0, width = 20, wrapText = TRUE)
    )

    # add details as comments
    for (i in seq_len(nrow(variables))) {
      if (is_na(detail <- variables$Details[[i]])) next

      comment <- openxlsx::createComment(comment = detail, author = "Details", visible = FALSE)
      openxlsx::writeComment(wb, sheet = "Variables", match("Description", names(variables)), i + 1, comment)
    }

    # write data source metadata
    add_xlsx_worksheet(wb, "Data Sources", build_data_sources_metadata(pipeline),
      .header_style       = list_assign(style0, !!!style_hdr),
      `Data Source`       = list_assign(style0, !!!style_hdr, width = 20),
      Description         = list_assign(style0, width = 45, wrapText = TRUE),
      License             = list_assign(style0, width = 15, wrapText = TRUE),
      Citekey             = list_assign(style0, width = 20, wrapText = TRUE),
      Citation            = list_assign(style0, width = 25, wrapText = FALSE),
      BibTeX              = list_assign(style0, width = 25, wrapText = FALSE)
    )

    openxlsx::saveWorkbook(wb, out_path, overwrite = TRUE)
  }
})

build_product <- function(pipeline) {
  progress_context()
  progress_message("Building the product", .persist = TRUE)

  # clean the current product
  check_errors(fs::dir_delete("/data/product"))

  # set up a temporary product output directory
  product_dir <- tempfile(pattern = "human_ecology_grid_")
  check_errors(fs::dir_delete(product_dir))
  fs::dir_create(product_dir)

  # copy the files
  product_fingerprint <- map(grid_config[pipeline$config$grid], function(grid_info) {
    grid_label <- sprintf("grid-%skm", grid_info$resolution)
    outputs_dir <- fs::path("/data/build/",  grid_label, "outputs")
    product_dir <- fs::path(product_dir, grid_label)

    # TODO: check fingerprint!
    fs::dir_exists(outputs_dir) || error(
      "unable to locate outputs for {.file {grid_label}}",
      i = "{.file {outputs_dir}} does not exist"
    )

    # copy all files
    filelist <- with_dir(outputs_dir, fs::dir_ls(recurse = TRUE, all = FALSE, type = "file")) |>
      setdiff("fingerprint.txt")

    fs::dir_create(fs::path_dir(fs::path(product_dir, filelist)))
    fs::file_copy(fs::path(outputs_dir, filelist), fs::path(product_dir, filelist))

    read_file(fs::path(outputs_dir, "fingerprint.txt"))
  })

  # output the metadata
  write_product_metadata(pipeline, fs::path(product_dir, "variables.xlsx"))

  # copy the references
  fs::file_copy("/pipeline/references.bib", fs::path(product_dir, "references.bib"))

  # copy all other assets
  assets <- with_dir("/pipeline/product-assets", fs::dir_ls(recurse = TRUE, all = FALSE, type = "file"))
  fs::dir_create(fs::path_dir(fs::path(product_dir, assets)))
  fs::file_copy(fs::path("/pipeline/product-assets", assets), fs::path(product_dir, assets))

  # combined ginderprint
  fingerprint <- get_pipeline_fingerprint(pipeline)
  fingerprint$output <- hash(product_fingerprint)
  fingerprint <- fingerprint[c("data", "code", "output")]

  version <- hash(fingerprint)
  version_list <- tryCatch(RcppTOML::parseTOML("/pipeline/versions.toml"), error = function(...) list())
  if (has_name(version_list, version)) {
    fingerprint <- c(list(version = version_list[[version]]), fingerprint)
    version <- sprintf("v%s (%s)", version_list[[version]], version)
  }

  yyjsonr::write_json_file(
    fingerprint,
    fs::path(product_dir, "version.json"),
    opts = yyjsonr::opts_write_json(pretty = TRUE, auto_unbox = TRUE)
  )

  progress_message("Running stage 0 product tests", .persist = TRUE)
  progress_message()
  run_product_stage0_tests(pipeline, product_dir)

  progress_message("Running stage 1 product tests", .persist = TRUE)
  progress_message()
  run_product_stage1_tests(pipeline, product_dir)

  # copy the final product
  fs::dir_copy(product_dir, "/data/product", overwrite = TRUE)

  progress_message()
  cli::cli_alert_success("Written product {.field {version}}")
}


run_product_stage0_tests <- function(pipeline, product_dir) {
  make_tests <- function(GRID_RESOLUTION) {
    test_label <- function(label) {
      sprintf("%skm %s", GRID_RESOLUTION, label)
    }

    tests <- exprs(
      data_latest_tbl = test_that(!!test_label("cross-section data can be loaded as a tibble"), {
        expect_no_error(data <- human_ecology_grid_data(!!GRID_RESOLUTION, year = "latest", result_type = "tbl"))
        expect_s3_class(data, "tbl_df")
      }),
      data_latest_arrow = test_that(!!test_label("cross-section data can be loaded as an arrow dataset"), {
        expect_no_error(data <- human_ecology_grid_data(!!GRID_RESOLUTION, year = "latest", result_type = "arrow"))
        expect_s3_class(data, "Dataset")
      }),
      data_yearly_tbl = test_that(!!test_label("yearly data can be loaded as a tibble"), {
        expect_no_error(data <- human_ecology_grid_data(!!GRID_RESOLUTION, year = "all", result_type = "tbl"))
        expect_s3_class(data, "tbl_df")
      }),
      data_yearly_arrow = test_that(!!test_label("yearly data can be loaded as an arrow dataset"), {
        expect_no_error(data <- human_ecology_grid_data(!!GRID_RESOLUTION, year = "all", result_type = "arrow"))
        expect_s3_class(data, "Dataset")
      }),
      geojson_sf = test_that(!!test_label("cells can be loaded as a sf object"), {
        expect_no_error(grid <- human_ecology_grid_cells(!!GRID_RESOLUTION, result_type = "sf"))
        expect_s3_class(grid, "sf")
        expect_s3_class(grid$geometry, "sfc")
      }),
      geojson_tbl = test_that(!!test_label("cells can be loaded as a tibble"), {
        expect_no_error(grid <- human_ecology_grid_cells(!!GRID_RESOLUTION, result_type = "tbl"))
        expect_s3_class(grid, "tbl_df")
        expect_s3_class(grid$geometry, "sfc")
      }),
      cell_ids = test_that(!!test_label("cell IDs are correct"), {
        data <- human_ecology_grid_data(!!GRID_RESOLUTION, year = "all", result_type = "arrow")
        cell_ids <-  pull(data, cell_id, as_vector = TRUE)
        grid <- human_ecology_grid_cells(!!GRID_RESOLUTION, result_type = "tbl")

        expect_false(anyNA(cell_ids))
        expect_contains(grid$cell_id, cell_ids)
      }),
      year_range = test_that(!!test_label("year range is correct"), {
        data <- human_ecology_grid_data(!!GRID_RESOLUTION, year = "all", result_type = "arrow")
        year <- pull(data, year, as_vector = TRUE)

        expect_false(anyNA(year))
        expect_true(all(year >= 1981))
        expect_true(all(year <= 2024))
      }),
      adj_matrix = test_that(!!test_label("cell adjacency matrix can be loaded"), {
        expect_no_error(mat <- human_ecology_grid_adjacency_matrix(!!GRID_RESOLUTION))
        grid <- human_ecology_grid_cells(!!GRID_RESOLUTION, result_type = "tbl")

        expect_s4_class(mat, "dsCMatrix")
        expect_equal(rownames(mat), grid$cell_id)
        expect_equal(colnames(mat), grid$cell_id)
      })
    )

    preamble <- exprs(
      library(rlang),
      library(tidyverse),
      library(vctrs),
      source(!!unclass(fs::path(product_dir, "scripts", "human-ecology-grid.R"))),
    )

    # individual tests as code listings
    map(tests, function(test) {
      c(
        list_unchop(map(preamble, deparse, width.cutoff = 500L)),
        "",
        deparse(test, width.cutoff = 500L)
      )
    })
  }

  # generate tests
  grid_resolutions <- unname(map_int(grid_config[pipeline$config$grid], "resolution"))
  tests <- map(grid_resolutions, function(resolution) {
    tests <- make_tests(resolution)
    names(tests) <- sprintf("%skm-%s", resolution, names(tests))
    tests
  })
  tests <- vec_c(!!!tests)

  # run tests
  run_tests(tests)
}


run_product_stage1_tests <- function(pipeline, product_dir) {
  get_ptype_expr <- function(ptype) {
    parse(text = deparse(ptype))[[1L]]
  }

  make_tests <- function(GRID_RESOLUTION, YEAR) {
    tests <- pmap(variables, function(year_coverage, field, ptype) {
      if (is.list(ptype)) return(NULL)

      # variable extraction
      field_parts <- unlist(strsplit(field, "$", fixed = TRUE))
      stopifnot(length(field_parts) %in% c(1L, 2L))

      var_sym <- sym(last(field_parts))
      var_expr <- if (length(field_parts) == 2L) {
        expr(select(data, !!sym(first(field_parts))) |> unnest(!!sym(first(field_parts))) |> pull(!!var_sym))
      } else {
        expr(pull(data, !!var_sym))
      }

      make_test_description <- function(desc) {
        sprintf("[%skm YEAR=%s] `%s` %s", GRID_RESOLUTION, YEAR, field, desc)
      }

      # different tests depending on whether the data is defined for the year or not
      is_covered <- length(year_coverage) == 0L || YEAR %in% year_coverage
      if (is_covered) {
        description <- make_test_description("contains valid data")
        coverage_tests <- exprs(
          expect_false(all(vec_detect_missing(!!var_sym)))
        )
      } else {
        description <- make_test_description("contains no data data")
        coverage_tests <- exprs(
          expect_true(all(vec_detect_missing(!!var_sym)))
        )
      }

      # built the tests
      list(
        expr(test_that(!!description, {
          !!var_sym <- !!var_expr

          expect_vector(!!var_sym, !!get_ptype_expr(ptype))
          !!!coverage_tests
        })),
        if (is.numeric(ptype)) expr(test_that(!!make_test_description("contains no NaNs"), {
          !!var_sym <- !!var_expr

          expect_false(any(is.nan(!!var_sym)))
        }))
      )
    })
    tests <- list_unchop(tests) |> compact()

    preamble <- exprs(
      library(rlang),
      library(tidyverse),
      library(vctrs),
      source(!!unclass(fs::path(product_dir, "scripts", "human-ecology-grid.R"))),
      data <- human_ecology_grid_data(!!GRID_RESOLUTION, year = "all", result_type = "arrow") |>
        filter(year == !!YEAR) |>
        collect()
    )

    # return the test code as text
    c(
      list_unchop(map(preamble, deparse, width.cutoff = 500L)),
      list_unchop(map(tests, function(expr) {

        c("", deparse(expr, width.cutoff = 500L))
      }))
    )
  }


  # enumerate all variables
  variables <- {
    pipeline$data_builders |>
    filter(name != "Grid") |>
    arrange(match(name, pipeline$config$theme_order)) |>
    select(year_coverage, schema) |>
    unnest(schema) |>
    filter(!is.na(description_short)) |>
    select(year_coverage, field, ptype)
  }

  # generate tests
  test_params <- expand_grid(
    GRID_RESOLUTION = map_int(grid_config[pipeline$config$grid], "resolution"),
    YEAR = sort(unique(unlist(variables$year_coverage)))
  )

  tests <- pmap(test_params, make_tests)

  names(tests) <- pmap(test_params, function(GRID_RESOLUTION, YEAR) {
    sprintf("%skm-%s", GRID_RESOLUTION, YEAR)
  })

  run_tests(tests)
}
