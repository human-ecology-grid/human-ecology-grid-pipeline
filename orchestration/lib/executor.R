

make_data_pipeline_info <- function(..., target, script) {
  check_dots_empty()
  config <- get_pipeline_config()

  list(
    stage = "data",
    target = target,
    script = script
  )
}

run_pipeline_function <- function(fn, args, ...) {
  check_dots_empty()

  # additional definitions for the development mode: debug printing etc.
  extras <- if (ENV_DEVELOPMENT) expr({
    .GlobalEnv$print <- function(...) {
      out <- capture.output(base::print(...))
      cli::cli_verbatim(out)
    }
  })

  # launch a new r session
  init_block <- expr({
    options(cli.message_class = "callr_message")
    options(warn = 2)

    # read the system profile, since it does not seem processed
    source(fs::path(Sys.getenv("R_HOME"), "etc/Rprofile.site"))

    library(stats)
    library(rlang)
    library(vctrs)
    library(withr)
    library(cli)
    library(tidyverse)

    # patch for s2
    c.s2_geography <- function(...) {
      s2:::new_s2_geography(unlist(lapply(list(...), s2::as_s2_geography)))
    }
    registerS3method("c", "s2_geography", c.s2_geography)

    source("/pipeline/lib/errors.R")
    source("/pipeline/lib/eval.R")
    source("/pipeline/lib/async.R")
    source("/pipeline/lib/parallel.R")
    source("/pipeline/lib/cli.R")

    !!!extras
  })

  # attempt to start the session
  start_session <- function() {
    callr::r_session$new(wait = TRUE, callr::r_session_options(load_hook = init_block))
  }

  check_errors(r_session <- start_session()) ||
  check_errors(r_session <- start_session()) ||
  check_errors(r_session <- start_session()) || error(
    "unable to start R session",
    i = !.last_error$message
  )
  on.exit(r_session$close())

  # execute the function
  result <- r_session$run_with_output(fn, args)

  # error check
  if (!is_null(result$error)) {
    error <- result$error$parent %||% result$error
    cnd_signal(error)
  }

  result$result
}
