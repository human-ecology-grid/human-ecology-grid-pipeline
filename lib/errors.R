# Signal an error
error <- function(..., .footer = NULL, .parent = NULL) {
  msg <- vapply(list2(...), cli::format_inline, "", .envir = caller_env())
  trace <- trace_back(bottom = caller_env())
  rlang::abort(
    message = msg,
    call = caller_env(),
    use_cli_format = FALSE,
    trace = trace,
    parent = .parent,
    footer = .footer
  )
}

# Mechanism for sequential error processing
#
# Use it like this:
#
# check_error(expr) || do_something(rlang::last_error())
check_errors <- function(expr) {
  no_error <- TRUE

  tryCatch(expr, error = function(cnd) {
    if (!inherits(cnd, "rlang_error")) {
      cnd <- error_cnd(message = conditionMessage(cnd) %||% "", trace = trace_back())
    }
    rlang:::poke_last_error(cnd)
    no_error <<- FALSE
  })

  invisible(no_error)
}

env_bind_active(current_env(), .last_error  = rlang::last_error)
