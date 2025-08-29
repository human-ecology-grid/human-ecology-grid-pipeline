# import promise operations
then <- promises::then
catch <- promises::catch
finally <- promises::finally

# A promise executing asynchronously on a separate thread
#
# The intended use is launching tasks that should not block the main
# loop. The number of concurrent tasks should be kept low. This function
# should not be used for parallel computation.
spawn_task <- function(expr) {
  p <- parallel:::mcfork()
  if (inherits(p, "masterProcess")) {
    parallel:::mcinteractive(FALSE)
    parallel:::mc.set.stream()
    parallel:::closeStdout(TRUE)
    parallel:::closeStderr(TRUE)

    withCallingHandlers(
      result <- expr,
      error = function(cnd) {
        parallel:::sendMaster(list(payload = cnd, type = "error"))
        parallel:::mcexit(1L)
      },
      condition = function(cnd) {
        parallel:::sendMaster(list(payload = cnd, type = "condition"))
      }
    )
    parallel:::mcexit(0L, list(payload = result, type = "result"))
  }
  pid <- p$pid
  poll_interval <- 0.05


  promises::promise(function(resolve, reject) {
    poll <- function() {
      running <- TRUE
      on.exit(if (running) later::later(poll, poll_interval) else parallel:::rmChild(pid))

      # poll all messages
      while (running && identical(parallel:::selectChildren(pid), pid)) {
        msg <- parallel:::readChild(pid)
        is.raw(msg) || fatal_error(
          "internal: unexpected communication from child process"
        )

        # decode the message
        msg <- unserialize(msg)
        is_list(msg) && is_string(msg$type) || fatal_error(
          "internal: unexpected communication from child process"
        )

        # handle the message
        running <- switch(msg$type,
          error = {
            reject(msg$payload)
            FALSE
          },
          condition = {
           # print(msg$payload)
            if (inherits(msg$payload, "cli_message")) {
              cli:::cli__message_emit(msg$payload)
            } else {
              signalCondition(msg$payload)
            }
            TRUE
          },
          result = {
            resolve(msg$payload)
            FALSE
          },
          # unknown message
          fatal_error("internal: unexpected communication from child process")
        )
      }
    }

    later::later(poll, poll_interval)
  })
}

# synchronize a promise
block_on <- function(.p, ..., .tick_fn = function() {}, .cli_progress = NULL) {
  check_dots_empty()
  .env <- caller_env()
  .tick_fn <- as_function(.tick_fn, .env)
  .pb <- if (!is_null(.cli_progress)) {
    cli::cli_progress_step(.cli_progress, spinner = TRUE, .envir = .env)
  }

  running <- TRUE
  result <- NULL

  then(.p, onFulfilled = function(value) {
    running <<- FALSE
    result <<- value

    if (!is.null(.pb)) cli::cli_progress_done(id = .pb, result = "done")
  }, onRejected = function(reason) {
    running <<- FALSE

    env_bind_lazy(env_parent(), result = rlang::abort(!!cnd_message(reason)))

    if (!is.null(.pb)) cli::cli_progress_done(id = .pb, result = "failed")
  })

  timeout <- 0.05

  while (running) {
    # cli::cli_text("ops is {later::next_op_secs()}")
    # fire update
    .tick_fn()
    if (!is.null(.pb)) cli::cli_progress_update(id = .pb, force = FALSE)

    # geometric backoff
    later::run_now(timeout)
    # timeout <- min(timeout * 2.0, 0.01)
  }

  result
}

sleep_msg <- function(time, msg = "waiting for {cli::pb_extra$remaining}s") {
  pb <- cli::cli_progress_bar(
    format = paste("{cli::pb_spin}", msg),
    clear = TRUE,
    extra = list(remaining = ceiling(time)),
    .auto_close = FALSE,
    .envir = caller_env()
  )

  t1 <- bench::hires_time() + time
  while ((remaining <- (t1 - bench::hires_time())) > 0) {
    Sys.sleep(min(0.05, remaining))
    cli::cli_progress_update(
      id = pb,
      force = TRUE,
      extra = list(remaining = ceiling(remaining)),
      .envir = caller_env()
    )
  }
  cli::cli_progress_done(id = pb, result = "clear")
}
