load_pipeline_info <- local({
  parse_toml <- RcppTOML::parseTOML

  #  data source definitions
  parse_data_source_descriptor <- function(descriptor_root, references) {
    # TODO: parse/check the metadata
    fs::dir_exists(descriptor_root) || error(
      "unable to locate directory {.file {descriptor_root}}"
    )

    # basic info
    dataset <- fs::path_file(descriptor_root)
    metadata_path <- fs::path(descriptor_root, "metadata.toml")
    digest_path <- fs::path(descriptor_root, "filelist.txt")
    fetch_script_path <- fs::path(descriptor_root, "fetch.R")

    # common error header
    error_header <- "invalid data source descriptor {.var {dataset}}"

    # check and load the metadata
    fs::file_exists(metadata_path) || error(
      !!!error_header,
      i = "missing metadata file {.file {metadata_path}}"
    )

    check_errors(toml <- parse_toml(metadata_path)) || error(
      !!!error_header,
      i = "in metadata file {.file {metadata_path}}",
      i = "{(.last_error$message)}"
    )

    # check and load the file digest
    fs::file_exists(digest_path) || error(
      !!!error_header,
      i = "missing file list {.file {digest_path}}"
    )

    check_errors(digest <- digest_read(digest_path)) || error(
      !!!error_header,
      i = "in file list {.file {digest_path}}",
      i = "{(.last_error$message)}"
    )

    # check if the fetch script exists
    if (!fs::file_exists(fetch_script_path)) {
      fetch_script_path <- NA_character_

      # filelist must contain urls
      has_name(digest, "remote") && !anyNA(digest$remote) || error(
        !!!error_header,
        i = "either {.file {fetch.R}} needs to be provided or {.file {filelist.txt}} should contain remote URLs",
      )
    }

    # citations
    is_bare_character(cite <- toml[["cite"]]) && all(cite %in% references$key) || error(
      !!!error_header,
      i = if (is_bare_character(cite)) {
        "expected character {.field {cite}}"
      }  else  {
        "unknown citation{?s} {.q {setdiff(cite, references$key)}}"
      }
    )

    # check metadata
    req_metadata <- c("name", "description", "license")
    req_metadata <- req_metadata[!map_lgl(toml[req_metadata], is_bare_string)]
    is_empty(req_metadata) || error(
      !!!error_header,
      i = "expected text metadata {.field {req_metadata}}"
    )

    tibble(
      name = dataset,
      longname = toml$name,
      descriptor_root = descriptor_root,
      digest = list(digest),
      fetch_script = fetch_script_path,
      metadata = list(toml[c("description", "cite", "license")]),
      fingerprint = digest_fingerprint(digest[[1]]),
      cite = list(cite),
      description = toml$description,
      license = toml$license
    )
  }

  load_data_sources_pipeline_info <- function(root, references) {
    # list the datasets
    fs::dir_exists(root) || return(NULL)
    data_sources <- with_dir(root, fs::dir_ls(type = "directory"))

    # parse the dataset definitions
    data_sources <- fs::path(root, data_sources) |>
      map_vec(parse_data_source_descriptor, references = references)

    (nrow(data_sources) > 0) || error(
      "no data sources defined in the pipeline"
    )

    data_sources
  }

  #  data builder definitions (modules)
  parse_data_builder_descriptor <- function(descriptor_root) {
    fs::dir_exists(descriptor_root) || error(
      "unable to locate directory {.file {descriptor_root}}"
    )

    builder <- fs::path_file(descriptor_root)
    metadata_path <- fs::path(descriptor_root, "metadata.toml")
    build_script_path <- fs::path(descriptor_root, "build.R")

    # common error header
    error_header <- "invalid data builder descriptor {.var {builder}}"

    # check and load the metadata
    fs::file_exists(metadata_path) || error(
      !!!error_header,
      i = "missing metadata file {.file {metadata_path}}"
    )

    check_errors(toml <- parse_toml(metadata_path)) || error(
      !!!error_header,
      i = "in metadata file {.file {metadata_path}}",
      i = "{(.last_error$message)}"
    )

    # check and load the file digest
    fs::file_exists(build_script_path) || error(
      !!!error_header,
      i = "missing builder script {.file {build_script_path}}"
    )

    # builder dependencies
    depends_on <- c("Grid", as.character(unclass(toml$depends_on))) |>
     unique() |>
     setdiff(builder)

    # parse the table schema
    check_errors(schema <- parse_dataset_schema(toml$variable)) || {
      error(
        !!!error_header,
        i = "in metadata file {.file {metadata_path}}",
        i = .last_error$message
    )}


    # check the longitudinal coverage metadata
    if (builder == "Grid") toml$longitudinal <- FALSE
    is_false(years <- toml$longitudinal) || is_bare_integer(years) || error(
      !!!error_header,
      i = "in metadata file {.file {metadata_path}}",
      i = "invalid {.field longitudinal} = {.q {years}}",
      i = "expected {.q false} or a list of years for which the data has coverage"
    )
    if (is_false(years)) years <- NULL

    # extend the schema
    schema <- bind_rows(
      tibble(field = "cell_id", ptype = list(character())),
      if (!is_null(years)) tibble(field = "year", ptype = list(integer())),
      schema
    )

    # TODO: validate metadata
    tibble(
      name = builder,
      descriptor_root = descriptor_root,
      build_script = build_script_path,
      depends_on = list(depends_on),
      metadata = list(toml),
      schema = list(schema),
      year_coverage = list(years),
      fingerprint = paste0(
        pipeline_code_fingerprint(descriptor_root),
        hash(select(schema[[1]], field, ptype))
      )
    )
  }

  load_data_builders_pipeline_info <- function(root) {
    # list the datasets
    fs::dir_exists(root) || return(NULL)
    data_builders <- with_dir(root, fs::dir_ls(type = "directory"))

    # parse the builder definitions
    data_builders <- fs::path(root, data_builders) |>
      map_vec(parse_data_builder_descriptor)

    (nrow(data_builders) > 0) || error(
      "no builder pipeline steps defined in the pipeline"
    )

    data_builders
  }

  # pipeline configuration
  load_config_file <- function(path) {
    if (!fs::file_exists(path)) return (list())

    check_errors(config <- parse_toml(path)) || error(
      "in file {.file config.toml}",
      i = "{(.last_error$message)}"
    )

    config
  }

  load_pipeline_config <- function() {
   # core configuration
   core_config <- load_config_file("/pipeline/config.toml")
   local_config <-  load_config_file("/data/config.toml")

   # combine configurations (local config has precedence)
   list_modify(core_config, !!!local_config)
  }

  # load_pipeline_info
  function() {
    local_error_call(expr(load_pipeline_info()))

    # load the references
    check_errors(references <- bibtex::read.bib("/pipeline/references.bib")) || error(
      "in file {.file references.bib}",
      i = "{(.last_error$message)}"
    )
    references <- tibble(
      key = unlist(references$key),
      bibentry = map(seq_along(references), function(i) references[[i]])
    )

    # collect pipeline info
    pipeline <- list(
      config = load_pipeline_config(),
      references = references,
      data_sources = load_data_sources_pipeline_info("/pipeline/data-sources", references),
      data_builders = load_data_builders_pipeline_info("/pipeline/data-builders"),
      data_dir = "/data/data-sources",
      product_dir = "/data/products",
      production = !ENV_DEVELOPMENT
    )

    # check the config
    is_bare_character(pipeline$config$theme_order) || error (
      "configuration error: invalid {.field theme_order} = {.q {pipeline$config$theme_order}}",
      i = "expected an ordered list of data themes"
    )

    all_themes <- setdiff(pipeline$data_builders$name, "Grid")
    setequal(pipeline$config$theme_order, all_themes) || error(
      "configuration error: {.field theme_order} must match the data themes",
      i = if(length(x <- setdiff(pipeline$config$theme_order, all_themes))) "unknown: {.field {x}}",
      i = if(length(x <- setdiff(all_themes, pipeline$config$theme_order))) "missing: {.field {x}}",
    )

    is_bare_character(pipeline$config$grid) && all(pipeline$config$grid %in% names(grid_config)) || error(
      "configuration error: invalid {.field grid} = {.q {pipeline$config$grid}}",
      i = "expected a list of valid grid identifiers"
    )

    all(pipeline$config$grid %in% names(grid_config)) || error(
      "configuration error: invalid {.field grid} = {.q {pipeline$config$grid}}",
      i = "unknown grid {.q {setdiff(pipeline$config$grid), names(grid_config)}}"
    )

    pipeline
  }
})
