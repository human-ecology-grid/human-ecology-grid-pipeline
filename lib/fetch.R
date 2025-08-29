make_download_progress_bar <- function(uri) {
  withr::local_options(cli.progress_show_after = 0)
  prev_total <- 0L

  # init the progress bar
  pb <- cli::cli_progress_bar(
    format = "Fetching {.file {uri}} {cli::pb_spin}",
    total = prev_total,
    type = "download",
    clear = TRUE,
    auto_terminate = FALSE,
    .auto_close = FALSE
  )

  withr::defer(if (!is_null(pb)) cli::cli_process_done(id = pb))
  time0 <- bench::hires_time()

  # methods
  list(
    update = function(down, total) {
      # total is now known, need to reset the bar!
      if (total > 0 && prev_total == 0) {
        time_elapsed <- bench::hires_time() - time0
        withr::local_options(cli.progress_show_after = max(0, 2 - time_elapsed))
        cli::cli_progress_done(id = pb, result = "clear")
        pb <<- cli::cli_progress_bar(
          format = "Fetching {.file {uri}} {cli::pb_percent} {cli::pb_bar} [{cli::pb_eta}]",
          total = total,
          type = "download",
          clear = TRUE,
          auto_terminate = FALSE,
          .auto_close = FALSE
        )
        cli::cli_progress_update(id = pb, set = down)
      } else {
        cli::cli_progress_update(id = pb, set = down, total = total)
      }

      prev_total <<- total
    },
    done = function() {
      cli::cli_progress_done(id = pb, result = "clear")
      pb <<- NULL
    }
  )
}

fetch_remote_file <- function(
  remote,
  local,
  ...,
  .attempts = 3L,
  .name = local,
  .postprocess = NULL
) {
  # parse .postprocess
  if (!is_null(.postprocess)) {
    if (is_function(.postprocess)) {
      postprocess_fun <- .postprocess
      postprocess_label <- "Processing"
    } else
    if (!is_null(.postprocess) && is_bare_list(.postprocess)) {
      postprocess_fun <- .postprocess$fun %||% error("{.code .postprocess$fun} must be a function")
      postprocess_label <- .postprocess$label %||% "Processing"
    } else {
      error("invalid {.code .postprocess} specification")
    }
  }

  # httr progress messages are not forwarded to parent process, we
  # work around it by capturing the progress asynchronously
  stats <- new_environment()
  curl_xferinfofunction <- function(down, up) {
    stats$download <- down
    TRUE
  }

  update_progress <- function() {
    pb$update(stats$download[[2]], stats$download[[1]])
  }

  # transformer requires a temporary download
  local_dst <- if (is_null(.postprocess)) {
    local
  } else {
    ext <- fs::path_ext(httr2::url_parse(remote)$path)
    if (ext != "") ext <- paste0(".", ext)
    local_tempfile(fileext = ext)
  }

  # fetch the data
  while (.attempts > 0) {
    .attempts <- .attempts - 1L

    # init progress
    stats$download <- c(0, 0)
    pb <- make_download_progress_bar(.name)

    # fetch the file asynchronously
    promise <- httr2::request(remote) |>
      httr2::req_options(followlocation = TRUE) |>
      httr2::req_user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64)") |>
      httr2::req_options(xferinfofunction = curl_xferinfofunction) |>
      httr2::req_perform_promise(path = local_dst)

    # wait for the promise while updating the progress bar
    success <- check_errors(block_on(promise, .tick_fn = update_progress))

    # check the download status
    pb$done()
    if (success) break

    # another attempt (if any left)
    if (.attempts == 0L) error("unable to {.file {(.name)}}", .parent = .last_error)
    sleep_msg(5, "error fetching {.file {(.name)}}, trying again in {cli::pb_extra$remaining}s")
  }

  # run the transformer, if needed
  if (!is_null(.postprocess)) {
    progress_message(sprintf("%s {.file {(.name)}}", postprocess_label))
    postprocess_fun(local_dst, local)
    progress_message()
  }

  invisible(local)
}

gzip_file <- function(src, dst) {
  R.utils::compressFile(src, dst, "gz", gzfile, overwrite = TRUE, remove = FALSE)
}
