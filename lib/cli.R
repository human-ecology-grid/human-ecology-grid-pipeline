format_elapsed <- function(elapsed, ..., from) {
  if (!missing(from)) elapsed <- bench::hires_time() - from
  cli::col_silver(sprintf("[%s]", prettyunits::pretty_sec(elapsed)))
}

progress_context <- function(..., prefix = "", start_time = bench::hires_time(), envir = caller_env()) {
  defer({
    check_errors(cli::cli_progress_done(environment(progress_message)$msg_id))
    environment(progress_message)$context <- NULL
  }, envir)
  environment(progress_message)$context <- list(prefix = prefix, start_time = start_time)
}

progress_message <- local({
  msg_id <- NULL
  context <- NULL

  function(msg, ..., .persist = FALSE) {
    check_errors(cli::cli_progress_done(msg_id))
    if (missing(msg)) return(invisible(NULL))

    elapsed <- if (!is_null(context)) format_elapsed(from = context$start_time) else ""
    prefix <- context$prefix %||% ""
    msg <- sprintf("{.alert-info {}}%s%s %s", prefix, msg, elapsed)

    msg_id <<- cli::cli_progress_bar(format = msg, type = "custom", .envir = caller_env(), .auto_close = FALSE, clear = !.persist)
    cli::cli_progress_update(id = msg_id, force = TRUE, .envir = caller_env())

    invisible(msg_id)
  }
})
