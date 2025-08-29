# core detection
n_available_cores <- function() {
  parallelly::availableCores(constraints = "multicore", logical = FALSE)
}

# thin wrapper over mclapply()
mc_map <- function(.x, .f, ..., .n_cores = n_available_cores()) {
  .f <- as_function(.f, caller_env())
  if (...length() != 0) .f <- partial(.f, ...)

  if (.n_cores <= 1L) {
    .x <- lapply(.x, .f)
  } else {
    parallel::mclapply(.x, .f, mc.silent = TRUE, mc.cores = .n_cores)
  }
}

mc_map2 <- function(.x, .y, .f, ..., .n_cores = n_available_cores()) {
  xy <- vec_recycle_common(.x, .y)

  .f <- as_function(.f, caller_env())
  if (...length() != 0) .f <- partial(.f, ...)

  if (.n_cores <= 1L) {
    .x <- exec(mapply, .f, !!!xy, SIMPLIFY = FALSE)
  } else {
    exec(parallel::mcmapply, .f, !!!xy, mc.silent = TRUE, SIMPLIFY = FALSE, mc.cores = .n_cores)
  }
}

# thin wrapper over mclapply() with common vector coercion
mc_map_vec <- function(.x, .f, ..., .ptype = NULL, .n_cores = n_available_cores()) {
  .f <- as_function(.f, caller_env())
  if (...length() != 0) .f <- partial(.f, ...)

  if (.n_cores <= 1L) {
    .x <- lapply(.x, .f)
  } else {
    .x <- parallel::mclapply(.x, .f, mc.silent = TRUE, mc.cores = .n_cores)
  }
  .ptype <- .ptype %||% vec_ptype_common(!!!.x)

  # fast path for s2_geography
  if (inherits(.ptype, "s2_geography")) {
    s2:::new_s2_geography(unlist(.x))
  } else {
    list_unchop(.x, ptype = .ptype)
  }
}

mc_map2_vec <- function(.x, .y, .f, ..., .ptype = NULL, .n_cores = n_available_cores()) {
  xy <- vec_recycle_common(.x, .y)

  .f <- as_function(.f, caller_env())
  if (...length() != 0) .f <- partial(.f, ...)

  if (.n_cores <= 1L) {
    .x <- exec(mapply, .f, !!!xy, SIMPLIFY = FALSE)
  } else {
    .x <- exec(parallel::mcmapply, .f, !!!xy, mc.silent = TRUE, SIMPLIFY = FALSE, mc.cores = .n_cores)
  }
  .ptype <- .ptype %||% vec_ptype_common(!!!.x)

  # fast path for s2_geography
  if (inherits(.ptype, "s2_geography")) {
    s2:::new_s2_geography(unlist(.x))
  } else {
    list_unchop(.x, ptype = .ptype)
  }
}

# thin wrapper over pvec()
mc_vec_apply <- function(
  .x,
  .f,
  ...,
  .ptype = NULL,
  .chunk_size = NULL,
  .n_cores = n_available_cores()
) {
  .f <- as_function(.f, caller_env())
  if (...length() != 0) .f <- partial(.f, ...)

  # processing chunks
  N <- vec_size(.x)
  max_chunk_size <- as.integer(ceiling(N / .n_cores))
  if (is_null(.chunk_size)) .chunk_size <- max_chunk_size
  .chunk_size <- min(.chunk_size, max_chunk_size)
  sizes <- c(rep(.chunk_size, N %/% .chunk_size), if (r <- N %% .chunk_size) r)

  if (.n_cores <= 1L) {
    .x <- list(.f(.x))
  } else {
    .x <- parallel::mclapply(
      vec_chop(.x, sizes = sizes),
      .f,
      mc.silent = TRUE,
      mc.cores = .n_cores,
      mc.preschedule = FALSE
    )
    if (length(.x) != length(sizes)) error(
      "(internal) sent {length(sizes)} jobs, received {length(.x)} results"
    )
  }
  .ptype <- .ptype %||% vec_ptype_common(!!!.x)

  # fast path for s2_geography
  if (inherits(.ptype, "s2_geography")) {
    s2:::new_s2_geography(unlist(.x))
  } else {
    list_unchop(.x, ptype = .ptype)
  }
}

vec_chunk <- function(.x, ..., .chunk_size) {
  N <- vec_size(.x)
  sizes <- c(rep(.chunk_size, N %/% .chunk_size), if (r <- N %% .chunk_size) r)

  vec_chop(.x, sizes = sizes)
}
