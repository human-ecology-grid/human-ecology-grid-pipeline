# Routines for working with file digests
#
# File digests are used to verify the dataset integrity and versions across
# builds. The digest is provided as a part of the dataset descriptor and will
# be compared to the data on the disk.
#
# The file digest is based on the GNU checksum format. It uses a simple space-separated
# column layout with up to three elements
#
# - file hash (XXH128)
# - file path (percent-encoded)
# - [optional] file remote URL (percent-encoded)
#
# Escaping is not supported. Line comments are supported (#)
digest_read <- function(path) {
  # the filelist is in a simple space-separated columnar format
  # there is no escaping and no spaces in the data
  # comments are supported
  text <- readr::read_file(path)

  # shrink multi-space alignment
  text <- gsub(" +", " ", text)
  tbl <- suppressWarnings(readr::read_delim(
    I(text),
    comment = "#",
    delim = " ",
    col_names = FALSE,
    show_col_types = FALSE,
    trim_ws = TRUE
  ))

  # empty columns are not allowed
  !any(sapply(tbl, function(x) all(is.na(x)))) || error("invalid digest {.file {path}}")

  # set the names
  (ncol(tbl) >= 2) && (ncol(tbl) <= 3) || error("invalid digest {.file {path}}")
  names(tbl) <- c("hash", "uri", "remote")[seq_len(ncol(tbl))]

  tbl$uri <- URLdecode(tbl$uri)

  tbl
}

digest_write <- function(tbl, path) {
  is_tibble(tbl) && identical(names(tbl), c("hash", "uri", "remote")[seq_len(ncol(tbl))]) || error(
    "invalid digest object"
  )

  tbl$uri <- URLencode(tbl$uri)

  # format the columns and print them in an aligned way
  lines <- as.list(tbl) |>
    map(function(x) ifelse(is.na(x), "", x)) |>
    map(function(x) format(x, justify = "left", width = nchar(x))) |>
    reduce(paste) |>
    str_trim()

  readr::write_lines(lines, path)
}

digest_empty <- function() {
  tibble(hash = character(), uri = character())
}

digest_compute <- function(paths) {
  is_file <- fs::file_exists(paths)
  hash <- rep(NA_character_, length(paths))

  hash[is_file] <- mc_map_vec(paths[is_file], hash_file)
  tibble(hash = hash, uri = paths)
}

digest_diff <- function(expected, observed) {
  diff <- full_join(expected, observed, by = "uri") |>
    mutate(hash.x = hash.x %|% "<missing>", hash.y = hash.y %|% "<missing>") |>
    mutate(status = case_when(
      hash.x == hash.y ~ "ok",
      hash.x == "<missing>" ~ "unknown",
      hash.y == "<missing>" ~ "missing",
      TRUE ~ "mismatch"
    )) |>
    select(uri, status)


  class(diff) <- c("digest_diff_tbl", class(diff))
  diff
}

digest_diff_local <- function(digest) {
  digest_diff(digest, digest_compute(fs::dir_ls(type = "file", recurse = TRUE)))
}

format.digest_diff_tbl <- function(x, ..., max_results = 5L, indent = 0L) {
  n_total <- nrow(x)
  if (n_total > max_results) x <- vec_slice(x, seq_len(max_results - 2))

  # build the columns
  cols <- map(unclass(x), as.character)
  names(cols) <- names(x)

  # highlight the results
  if (has_name(cols, "result")) {
    is_ok <- cols$status %in% "ok"
    cols$result <- paste0("<", cols$status, ">")
    cols$result <- ifelse(is_ok, col_green(cols$status), col_red(cols$status))
  }

  # add the headers
  cols <- imap(cols, function(col, name) c(style_bold(name), "-----", col))

  # align the headers
  cols <- map(cols, function(col) ansi_align(col, width = max(ansi_nchar(col)) + 2))

  # paste the columns together
  lines <- inject(paste0(!!!unname(cols)))

  # add the suffix
  if (n_total > nrow(x)) {
    lines <- c(lines, " ...", col_blue(format_inline("<and {n_total - max_results} more>")))
  }

  # indent
  if (indent > 0L) {
    lines <- paste0(format("", width = indent), lines)
  }

  lines
}

summary.digest_diff_tbl <- function(diff, ...) {
  status <- unique(diff$status)
  if_else(length(status) == 1L, status[[1]], "mismatch")
}

digest_fingerprint <- function(digest) {
  select(digest, uri, hash) |>
    arrange(uri) |>
    hash()
}

digest_fetch_remote <- function(digest) {
  has_name(digest, "remote") && !anyNA(digest$remote) || error(
    "unable to fetch file digest - missing remote URLs"
  )

  postprocess_fun <- function(uri, remote) {
    if (!endsWith(uri, ".gz") || endsWith(remote, ".gz")) return(NULL)

    list(fun = gzip_file, label = "Compressing")
  }


  for (i in seq_len(nrow(digest))) {
    uri <- digest$uri[[i]]
    remote <- digest$remote[[i]]
    hash <- digest$hash[[i]]

    # fetch the file
    fetch_remote_file(
      remote,
      uri,
      .postprocess = postprocess_fun(uri, remote)
    )

    # check the hash
    fetched_hash <- hash_file(uri)
    identical(fetched_hash, hash) || error(
      "hash mismatch in {.file {uri}}",
      i = "file hash is {.value {fetched_hash}}",
      i = "expected hash {.value {hash}}"
    )
  }

  digest
}
