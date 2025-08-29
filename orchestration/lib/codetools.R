load_script_code <- function(path) {
  tryCatch({
    code <- parse(file = path, keep.source = TRUE)
  },
  error = function(cnd) {
    inject(
      delayedAssign("code", rlang::abort(!!cnd$message)),
      env = parent.env(environment())
    )
  })

  as.list(code)
}

load_pipeline_code <- function(pipeline_script, ...) {
  # load the main script
  code <- load_script_code(pipeline_script)

  # load additional scripts
  sort_pipeline_scripts <- function(paths) {
    names <- fs::path_file(paths)
    paths[order(tolower(names) != "setup.r", names)]
  }

  helpers_code <- {
    fs::dir_ls(fs::path_dir(pipeline_script), glob = "*.R", recurse = TRUE) |>
    setdiff(pipeline_script) |>
    sort_pipeline_scripts() |>
    map(load_script_code) |>
    list_unchop()
  }

  c(helpers_code, code)
}

safe_hash_file <- function(path, ..., .missing = NA_character_) {
  out <- character(length(path))
  is_file <- fs::file_exists(path)
  out[is_file] <- mc_map_vec(path[is_file], rlang::hash_file)

  out
}

hash_files <- function(..., .simplify = FALSE) {
  hash <- sort(vec_c(...)) |> safe_hash_file(.missing = "-") |> paste0(collapse = "")
  if (is_true(.simplify)) hash <- hash(hash)
  hash
}

pipeline_code_fingerprint <- function(path) {
  fs::dir_ls(
    path,
    recurse = TRUE,
    type = "file",
    glob  = "**/metadata.toml",
    invert = TRUE
   ) |>
   hash_files()
}


make_pipeline_function <- function(
  pipeline_script,
  ...,
  code,
  .args = "PIPELINE_INFO",
  .prologue = NULL,
  .epilogue = NULL
) {
  check_dots_empty()
  check_exclusive(pipeline_script, code)

  # load the code if needed
  if (missing(code)) code <-  load_pipeline_code(pipeline_script)

  # function arguments
  args <- unique(c("...", .args))
  args <- rep(list(sym("")), length(args)) |> set_names(args)

  # build the function
  new_function(args, env = .GlobalEnv, body = expr({
    .state <- rlang::new_environment(list(
      env = NULL,
      start_time = bench::hires_time(),
      # progress message
      progress_prefix = "",
      progress_id = NULL
    ))

    !!!.prologue

    # progress_message <- function(msg) {
    #   # clear previous message
    #   check_errors(cli_progress_done(id = .state$progress_id))

    #   # nothing to show - exit
    #   if (missing(msg)) return()

    #   elapsed <- prettyunits::pretty_sec(bench::hires_time() - .state$start_time)
    #   elapsed <- cli::col_grey(paste0("[", elapsed, "]"))
    #   msg <- cli::format_inline(msg, .envir = caller_env())
    #   format <- sprintf("%s%s %s", .state$progress_prefix, msg, elapsed)

    #   .state$progress_id <- cli::cli_progress_bar(format = format, type = "custom", clear = TRUE, .auto_close = FALSE)
    #   cli::cli_progress_update(id = .state$progress_id, force = TRUE)

    #   invisible(.state$progress_id)
    # }

    # .GlobalEnv$progress_message <- progress_message

    lockEnvironment(rlang::current_env(), TRUE)

    withCallingHandlers((function() {
      options(rlang_trace_top_env = environment())
      on.exit(.state$env <- environment())
      # .state$start_time <- bench::hires_time()

      !!!code
    })(), error = rlang::entrace)

    progress_message()

    !!!.epilogue
  }))
}
