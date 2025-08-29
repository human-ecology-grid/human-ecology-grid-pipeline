fetch_data_sources <- local({
  make_fetch_step_info <- function(..., target, data_dir, script, digest, pipeline) {
    check_dots_empty()

    list(
      stage = "fetch",
      target = target,
      data_dir = data_dir,
      script = script,
      digest = select(digest, uri, hash),
      credentials = pipeline$config$credentials %||% list()
    )
  }

  check_and_fetch_data_source <- function(data_source, pipeline) {
    force <- is_true(pipeline$options$force_fetch) || is_true(data_source$name %in% pipeline$options$force_fetch)
    has_custom_fetch <- !is_na(data_source$fetch_script)

    data_source$digest <- data_source$digest[[1]] # digest is boxed

    # set the data directory
    data_dir <- fs::path(pipeline$data_dir, data_source$name)
    if (force && fs::dir_exists(data_dir)) fs::dir_delete(data_dir)
    fs::dir_create(data_dir)
    local_dir(data_dir)

    # check the local data
    t0 <- bench::hires_time()
    progress_message("Checking {.file {data_source$name}}...")
    diff <- digest_diff_local(data_source$digest)
    status <- summary(diff)
    progress_message()

    # nothing to do, data is cached
    if (status == "ok") {
      cli_alert_success("Checking {.file {data_source$name}}... {format_elapsed(from = t0)}")
      return(FALSE)
    }

    # nothing to do, data is cached
    if (status == "ok") return(FALSE)

    # remove non-matching files (if we have a custom script we remove everything)
    to_remove <- filter(diff, has_custom_fetch | status != "ok") |> filter(status != "missing")
    fs::file_delete(to_remove$uri)

    # fetch the data
    t0 <- bench::hires_time()
    progress_context(prefix = format_inline("{.file {data_source$name}} "))
    if (has_custom_fetch) {
      # run the custom fetch step
      fetch_fn <- make_pipeline_function(
        data_source$fetch_script,
        .prologue = expr({
          source("/pipeline/lib/fetch.R")
          progress_context(prefix = !!format_inline("{.file {data_source$name}} "))
        })
      )

      pipeline_step_info <- make_fetch_step_info(
        target = data_source$name,
        script = data_source$fetch_script,
        data_dir = data_dir,
        digest = data_source$digest,
        pipeline = pipeline
      )

      run_pipeline_function(
        fetch_fn,
        list(PIPELINE_INFO = pipeline_step_info)
      )
    } else {
      # fetch the files using the digest
      to_fetch <- filter(diff, status %in% c("missing", "mismatch")) |>
        left_join(data_source$digest, join_by(uri))

      digest_fetch_remote(to_fetch)
    }

    # re-check the data
    diff <- digest_diff_local(data_source$digest)
    status <- summary(diff)

    (status == "ok") || error(
      "Error fetching dataset {.file {data_source$name}}",
      i = "data checksum does not match",
      .footer = c("", format(diff, indent = 4L))
    )

    progress_message()
    cli_alert_success("Fetched {.file {data_source$name}}... {format_elapsed(from = t0)}")

    # fetched
    TRUE
  }

  # fetch_data_sources
  function(pipeline) {
    n_fetched <- 0L

    # process all data sources
    data_sources <- vec_chop(pipeline$data_sources)
    for (data_source in data_sources) {
      # inject trick for cleaner error traces
      n_fetched <- n_fetched + check_and_fetch_data_source(as.list(data_source), pipeline)
    }

    # status report
    cli::cli_text("")
    if (n_fetched == 0L) {
      cli::cli_alert_success("All datasets up to date")
    } else {
      cli::cli_alert_success("Fetched {n_fetched} dataset{?s} ({length(data_sources) - n_fetched} up to date)")
    }
  }
})
