run_data_builders <- local({
  make_builder_step_info <- function(..., target, script, pipeline) {
    check_dots_empty()

    list(
      stage = "builder",
      target = target,
      script = script,
      grid = pipeline$config$grid,
      production = pipeline$production
    )
  }

  make_builder_function <- function(stage, pipeline) {
    code <- load_pipeline_code(stage$build_script)

    # locations
    data_dir <- pipeline$data_dir
    artifact_dir <- fs::path(pipeline$staging_dir, "artifacts")
    cache_dir <- fs::path(pipeline$staging_dir, "cache")
    grid_prefix <- cli::col_green(sprintf("[%skm]", pipeline$config$grid$resolution))

    # grid load hook
    grid_load_hook <- if (stage$name != "Grid") exprs(
      delayedAssign("grid", read_artifact("Grid", "Grid")),
      delayedAssign("subgrid", read_artifact("Grid", "Subgrid")),
      delayedAssign("grid_no_ocean_ice", read_artifact("Grid", "GridNoOceanIce")),
      delayedAssign("subgrid_no_ocean_ice", read_artifact("Grid", "SubgridNoOceanIce")),
      delayedAssign("grid_sfc", read_artifact("Grid", "Grid_sfc")),
      delayedAssign("subgrid_sfc", read_artifact("Grid", "Subgrid_sfc")),
      delayedAssign("grid_no_ocean_ice_sfc", read_artifact("Grid", "GridNoOceanIce_sfc")),
      delayedAssign("subgrid_no_ocean_ice_sfc", read_artifact("Grid", "SubgridNoOceanIce_sfc"))
    )

    # the builder prologue loads the packages and installs I/O functions
    #
    # - with_cache(id, fingerprint, expr) is used to cache local computations
    # - data_uri(data_source, uri) is used to find data sources (and also tracks data dependencies)
    # - write_artifact(name, data) is used to write a pipeline artifact for the stage
    # - read_artifact(stage, name) is used to read a pipeline artifact from a dependency stage
    builder_prologue <- expr({
      .state$dependencies <- character()
      .state$artifacts <- character()
      .state$cached_objects <- character()

      .depends_on_stages <- !!(unlist(stage$depends_on))

      # temporal coverage
      YEARS <- !!stage$year_coverage

      library(sf)
      library(s2)
      terra::terraOptions(memfrac = 0.7)

      source("/pipeline/lib/geotools.R")
      source("/pipeline/lib/stat.R")

      !!!grid_load_hook

      cached <- function(expr, ...) {
        # combined hash
        hash <- rlang::hash(c(list2(...), list(substitute(expr))))
        hash <- paste0(!!paste0(stage$name, "_"), hash)

        # mark the hash
        .state$cached_objects <- c(.state$cached_objects, hash)

        # load from disk if available
        path <- fs::path(!!cache_dir, hash)
        if (fs::file_exists(path)) return(qs2::qs_read(path))

        # cache the expression
        output <- expr
        qs2::qs_save(output, path)

        output
      }

      # clear caches on exit
      on.exit({
        with_dir(!!cache_dir, local({
          cached_objects <- fs::dir_ls(type = "file", regexp = !!sprintf("^%s_.+$", stage$name))
          to_remove <- setdiff(cached_objects, .state$cached_objects)
          fs::file_delete(to_remove)
        }))
      })

      write_artifact <- function(data, name) {
        if (missing(name)) name <- !!stage$name

        is_string(name) && (name != "") || error(
          "invalid artifact name {.val {name}}",
          i = "artifact name must be a non-empty string"
        )

        suffix <- paste0("_", rlang::hash(c(!!stage$name, name)))
        artifact_path <- fs::path(!!artifact_dir, paste0(!!stage$name, suffix, ".qs2"))

        .state$artifacts <- unique(c(.state$artifacts, fs::path_file(artifact_path)))
        qs2::qs_save(data, artifact_path)
      }

      read_artifact <- function(stage, name) {
        (stage %in% .depends_on_stages) || error(
          !!sprintf("{.file {stage}} is not a dependency of the current pipeline {.file {%s}}", stage$name),
          i = "can only read artifacts from dependencies"
        )

        is_string(name) && (name != "") || error(
          "invalid artifact name {.val {name}}",
          i = "artifact name must be a non-empty string"
        )
        suffix <- paste0("_", rlang::hash(c(stage, name)))
        artifact_path <- fs::path(!!artifact_dir, paste0(stage, suffix, ".qs2"))

        fs::file_exists(artifact_path) || error(
          "unable to locate artifact {.val {name}} for {.file {stage}}"
        )
        .state$dependencies <- unique(c(.state$dependencies, fs::path_file(artifact_path)))

        qs2::qs_read(artifact_path)
      }

      data_uri <- function(dataset, ...) {
        data_dir <- fs::path(!!data_dir, dataset)
        fs::dir_exists(data_dir) || error(
          "unknown data source {.file {dataset}}"
        )

        uri <- fs::path(data_dir, ...)
        fs::file_exists(uri) || error(
          "unable to locate uri {.file {fs::path(...)}} for data source {.file {dataset}}"
        )

        .state$dependencies <- unique(c(.state$dependencies, dataset))
        uri
      }

      progress_context(prefix = !!format_inline("{grid_prefix} {.file {stage$name}} "))
    })

    builder_epilogue <- expr({
      as.list(.state)
    })

    make_pipeline_function(
      code = code,
      .prologue = builder_prologue,
      .epilogue = builder_epilogue
    )
  }

  check_dependency_fingerprints <- function(fingerprints, pipeline) {
    # get current fingerprints and compare
    current_fingerprints <- get_dependency_fingerprints(pipeline, names(fingerprints))

    identical(current_fingerprints, fingerprints)
  }

  verify_builder_output <- function(stage, pipeline) {
    artifact_path <- get_artifact_path(pipeline, stage$name)

    fs::file_exists(artifact_path) || error(
      "missing required artifact {.var {stage$name}}",
      i = "did you forget {.code write_artifact(<data>)}",
      i = "in {.file {fs::path(stage$name, '__build__.R')}}?"
    )

    # TODO: validate grid
    if (stage$name == "Grid") return()

    data <- qs2::qs_read(artifact_path)
    data <- checked_table_data(
      data,
      stage$schema,
      .error_msg = format_inline("error validating artifact {.var {stage$name}}")
    )

    # validate cell ids
    all(data$cell_id %in% pipeline$grid$cell_ids) || error(
      "error validating artifact {.var {stage$name}}",
      i = "invalid cell id{?s}: {.q {setdiff(data$cell_id, pipeline$grid$cell_ids}}"
    )

    # validate longitudinal coverage
    is_null(stage$year_coverage) || setequal(data$year, stage$year_coverage) || error(
      "error validating artifact {.var {stage$name}}",
      i = "invalid year coverage",
      if (length(x <- setdiff(stage$year_coverage, data$year))) "missing coverage for {.field {sort(x)}}",
      if (length(x <- setdiff(data$year, stage$year_coverage))) "unexpected coverage for {.field {sort(x)}}"
    )

    # check that there are no empty rows
    vec_is_empty(missing_rows <- filter(data, if_all(-any_of(c("cell_id", "year")), is.na))) || error(
      "error validating artifact {.var {stage$name}}",
      i = "empty rows detected",
      .footer = cli::cli_format_method({
        print(missing_rows)
      })
    )

    # check that columsn are unnamed
    for (i in seq_along(data)) {
      is_null(names(data[[i]])) || error(
        "error validating artifact {.var {stage$name}}",
        i = "columns {.field {names(data)[[i]]}} is named",
      )
    }

    qs2::qs_save(data, artifact_path)
  }

  # result: 0 - up to date, 1 - code or output changed, 2 - dependency changed
  check_stage_status <- function(stage, pipeline) {
    # read the cache manifest
    manifest <- read_stage_manifest(pipeline, stage)

    # absent or corrupted manifest, needs rerun
    if (is_null(manifest)) return(1L)

    # check dependencies
    if (!check_dependency_fingerprints(manifest$dependencies, pipeline)) return(2L)

    # check code and artifact fingerprints
    artifact_fingerprint <- hash_files(locate_stage_artifacts(pipeline, stage$name))
    if (!all(manifest$code == stage$fingerprint, manifest$artifact == artifact_fingerprint)) return (1L)

    # up to date
    0L
  }

  check_and_run_build_stage <- function(stage, pipeline) {
    grid_prefix <- cli::col_green(sprintf("[%skm]", pipeline$config$grid$resolution))

    # do nothing if the output is up to date
    status <- check_stage_status(stage, pipeline)
    if (status == 0L) {
      cli_alert_success("{grid_prefix} {.file {stage$name}} is up to date")
      return(FALSE)
    }

    t0 <- bench::hires_time()
    progress_context(prefix = format_inline("{grid_prefix} {.file {stage$name}} "))
    # clear the caches if dependencies have changed
    if (status == 2L) with_dir(fs::path(pipeline$staging_dir, "cache"), {
      fs::dir_ls(type = "file", regexp = sprintf("^%s_.+$", stage$name)) |>
      fs::file_delete()
    })

    # remove all artifacts
    fs::file_delete(locate_stage_artifacts(pipeline, stage$name))

    # run the builder
    builder_fn <- make_builder_function(stage, pipeline)
    pipeline_step_info <- make_builder_step_info(
      target = stage$name,
      script = stage$build_script,
      pipeline = pipeline
    )

    # TODO: clean the cache if the sources have changed!
    local_dir(fs::path_dir(stage$build_script))
    result <- run_pipeline_function(
      builder_fn,
      list(PIPELINE_INFO = pipeline_step_info)
    )

    progress_message("Validating artifacts")

    verify_builder_output(stage, pipeline)
    write_stage_manifest(pipeline, stage, result$dependencies)

    progress_message()
    cli_alert_success("{grid_prefix} {.file {stage$name}} built...{format_elapsed(from = t0)}")

    TRUE
  }

  # run_data_builders
  function(pipeline, ..., .force) {
    fs::dir_create(fs::path(pipeline$staging_dir, "cache"))
    fs::dir_create(fs::path(pipeline$staging_dir, "artifacts"))

    stages <- pipeline$data_builders

    # order the stages by their dependencies
    dependencies <- set_names(map(stages$depends_on, unlist), stages$name)

    run_order <- toposort::topological_sort(dependencies, dependency_type = "follows")
    stages <- vec_slice(stages, match(run_order, stages$name))

    # forced re-runs
    if (!missing(.force)) {
      stages <- filter(stages, name %in% c("Grid", .force))
      for (stage in .force) fs::file_delete(locate_stage_artifacts(pipeline, stage))
    }

    # split the stages into rows
    stages <- pmap(stages, list)

    # run the grid
    (stages[[1]]$name == "Grid") || error(
      "(internal) invalid pipeline execution order"
    )
    check_and_run_build_stage(stages[[1]], pipeline)

    # load grid ids for validation
    pipeline$grid$cell_ids <- qs2::qs_read(get_artifact_path(pipeline, "Grid"))$cell_id

    # run the rest of the pipeline
    for (stage in stages[-1]) {
      check_and_run_build_stage(stage, pipeline)
    }
  }
})
