checked_table_data <- local({
  # performs data validation, throws errors on schema mismatch
  check_data <- function(data, schema, ..., .error_msg = "error validating dataset") {
    # enumerate data fields in order of occurence, including nested fields
    #
    # we align expected and observed fields, inserting gaps where needed
    # out of order fields are also indicated by gaps
    fields <- tibble(expected = character(0L), observed = character(0L))

    observed <- enumerate_table_fields(data)
    expected <- schema$field
    matched_fields <- vec_set_intersect(observed, expected)

    while (length(expected) & length(observed)) {
      # expected and observed match, emit it
      if (identical(expected[[1L]], observed[[1L]])) {
        fields <- vec_c(fields, tibble(expected = expected[[1L]], observed = observed[[1L]]))
        expected <- expected[-1L]
        observed <- observed[-1L]
      } else
      # expected is not matched or observed will be matched later, emit expected
      if (!expected[[1L]] %in% matched_fields || observed[[1]] %in% expected) {
        fields <- vec_c(fields, tibble(expected = expected[[1L]], observed = NA_character_))
        expected <- expected[-1L]
      } else
      # emit observed
      {
        fields <- vec_c(fields, tibble(expected = NA_character_, observed = observed[[1L]]))
        observed <- observed[-1L]
      }
    }

    # emit the remaining expected followed by observed
    fields <- vec_rbind(
      fields,
      tibble(expected = expected, observed = NA_character_),
      tibble(expected = NA_character_, observed = observed)
    )

    # field
    fields <- fields |> mutate(
      expected_type = map(schema$ptype[vec_match(expected, schema$field)], function(ptype) {
        # weaked list of data frame fields for the purpose of type checking
        if (obj_is_list(ptype) && is.data.frame(attr(ptype, "ptype"))) ptype <- list_of_tbl_ptype

        ptype
      }),
      observed_type = map2(observed, expected_type[vec_match(observed, expected)], function(field, ptype) {
        if (is.na(field)) return (NULL)

        get_field_ptype(data, field, ptype)
      })
    ) |>
    # detect errors
    mutate(
      error_field_missing = !is.na(expected) & !(expected %in% observed),
      error_field_unknown = !is.na(observed) & !(observed %in% expected),
      error_invalid_order = ((expected != observed) %|% TRUE) & !(error_field_missing | error_field_unknown),
      error_type_expected = map2_lgl(expected_type, observed_type[vec_match(expected, observed)], function(...) {
        if (is_null(..1) || is_null(..2)) return(NA)

        !identical(...)
      }),
      error_type_observed = error_type_expected[vec_match(observed, expected)],
      ok = !if_any(starts_with("error_"), identity)
    )

    # abort of no errors detected
    if (all(fields$ok)) return()

    # build the report
    report <- fields |> mutate(
      # format fields (highlight errors and perfect matches)
      expected = case_when(
        ok                                        ~ cli::col_green(expected),
        is.na(expected)                            ~ "",
        error_type_expected | error_field_missing ~ cli::col_red(expected),
        TRUE                                      ~ expected
      ),
      observed = case_when(
        ok                                        ~ cli::col_green(observed),
        is.na(observed)                           ~ "",
        error_field_unknown | error_type_observed ~ cli::col_red(observed),
        TRUE                                      ~ observed
      ),
      # spacer column, highlighting out of order items
      spacer = local({
        # indices of matching out of order fields
        i0 <- which(fields$error_invalid_order & !is.na(fields$observed))
        i1 <- vec_match(fields$observed[i0], fields$expected)


        # interval indices
        ii <- map2(i0, i1, seq.int) |> unlist() |> unique()

        # compose the column, setting markers for out of order fields
        rowid <- seq_along(expected)

        paste0(
          if_else(rowid %in% i0, " ··", "   "),
          if_else(rowid %in% ii, "·", " "),
          if_else(rowid %in% i1, "·· ", "   ")
        )
      }),
      # error details
      details = case_when(
        error_type_expected ~ cli::col_red(sprintf(
          "! expected <%s>, found <%s>",
          map_chr(expected_type, vec_ptype_abbr),
          map_chr(observed_type[vec_match(expected, observed)], vec_ptype_abbr)
        )),
        TRUE ~ ""
      )
    ) |>
    # format the columns
    reframe(
      col1 = format_column(observed, "data"),
      col2 = format_column(spacer, "", delim = " "),
      col3 = format_column(expected, "expected"),
      col4 = format_column(details, "", delim = " "),
      lines = paste(col1, col2, col3, col4)
    )


    error(.error_msg, .footer = cli::cli_format_method({
      cli::cli_text("")
      cli::cli_verbatim(paste0("  ", report$lines))
      cli::cli_text("")
      cli::cli_text("")
   }))
  }

  # list all table fields, included nested data
  # only nesting of level 1 is supported
  enumerate_table_fields <- function(tbl) {
    stopifnot(is.data.frame(tbl))
    tbl <- unclass(tbl)

    unlist(map(seq_along(tbl), function(i) {
      is_nested <- (
        vec_is_list(tbl[[i]]) &&
        !is_named(tbl[[i]]) &&
        every(tbl[[i]], function(x) is_null(x) || is_na(x) || is.data.frame(x))
      )

      c(
        # field
        names(tbl)[[i]],
        # nested fields (if any)
        if (is_nested) {
          paste0(names(tbl)[[i]], "$", vec_unique(unlist(map(tbl[[i]], names))))
        }
      )
    }))
  }

  # combine vectors using indices
  vec_insert <- function(x, y, after) {
    y <- vec_cast(y, x)
    stopifnot(is_integerish(after) && all(after >= 0L))
    after <- pmin(after, vec_size(x))

    recycled <- vec_recycle_common(y, after)
    y <- recycled[[1]]
    after <- recycled[[2]]

    if (vec_size(y) == 0L) return(x)

    # group by insertion point
    to_insert <- vec_locate_sorted_groups(after)

    # N insertion points partition x into N+1 intervals
    # calculate the partition sizes so that we can split x accordingly
    sizes <- c(to_insert$key, vec_size(x)) - c(0L, to_insert$key)

    # combine the vectors
    out <- vector("list", 2*length(sizes) - 1L)
    out[seq.int(1L, length(out), by = 2L)] <- vec_chop(x, sizes = sizes)
    out[seq.int(2L, length(out), by = 2L)] <- vec_chop(y, to_insert$loc)

    list_unchop(out)
  }

  # field type detection
  get_field_ptype <- function(data, field, expected_ptype = NULL) {
    data_chunks <- get_field_data(data, field)

    # NULL chunks indicate incomplete nested data
    if (some(data_chunks, is_null)) return(incomplete_type)

    # try casting the data to expected ptype
    if (!is_null(expected_ptype) && check_errors(x <- list_unchop(data_chunks, ptype = expected_ptype))) {
      # check for NANs
      if (is.numeric(x) && any(is.nan(x))) return(incomplete_type)
      if (all(vec_detect_missing(x))) return(incomplete_type)

      expected_ptype
    } else
    # casting has failed, infer the common data type
    if (check_errors(ptype <- vec_ptype_common(!!!data_chunks))) {
      # check for list of tibbles
      if (obj_is_list(ptype) && check_errors(check_is_list_of_tbl(ptype))) {
        ptype <- list_of_tbl_ptype
      }
      ptype
    } else {
      mismatched_type
    }
  }

  # output: list of data chunks extracted from the (potentially nested) field
  #         NULL signals that the field does not exist in the nested data
  get_field_data <- function(data, field) {
    field <- unlist(strsplit(field, "$", fixed = TRUE))

    if (length(field) == 1) {
      list(data[[field]])
    } else
    if (length(field) == 2) {
      prefix <- field[[1L]]
      suffix <- field[[2L]]
      fn <- inject(function(X) `$`(unclass(X), !!suffix))

      # extract table data
      data <- data[[prefix]]
      data <- data[map_lgl(data, is.data.frame)]

      mc_map(data, fn)
    } else {
      error("invalid selector {field}")
    }
  }

  # formatting utilities
  format_column <- function(coldata, header, ..., delim = "-", pad = 0L) {
    colwidth <- max(ansi_nchar(c(header, coldata), "width")) + 2L*pad

    # build the header
    hdr <- ansi_align(c(header, str_pad("", colwidth, pad = delim)), colwidth, align = "center")

    # combine and align
    col <- c(hdr, paste0(str_pad("", pad), coldata))
    ansi_align(col, colwidth, "left")
  }

  # validated_data()
  function(data, schema, ..., .error_msg = "error validating dataset") {
    check_data(data, schema, .error_msg = .error_msg)

    # convert schema to prototype proper
    schema <- filter(schema, !str_detect(field, fixed("$")))
    ptype <- new_tibble(set_names(schema$ptype, schema$field))

    # convert the data
    data <- vec_cast(data, ptype)

    data
  }
})
