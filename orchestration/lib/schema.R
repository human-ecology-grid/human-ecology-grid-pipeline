schema_field_types <- list(
  numeric = list(ptype = double()),
  quantity = list(ptype = double()),
  label = list(ptype = character()),
  bool = list(ptype = logical()),
  list = list(ptype = list()),
  id = list(ptype = NULL)
)

schema_field_aggregations <- list(
  none = list(),
  mean = list(),
  sum = list(),
  proportion = list(),
  median = list(),
  percentile = list(),
  stdev = list(),
  min = list(),
  max = list(),
  gini = list(),
  count = list(),
  normalize_by_area = list()
)

missing_val <- cli::col_red("<missing>")

parse_dataset_schema <- function(toml) {
  parse_var <- function(
    ...,
    name = NULL,
    description = NULL,
    aggregation = NULL,
    type = NULL,
    units = NULL,
    data_source = NULL,
    reference_schema = NULL,
    reference_type = NULL,
    reference_uri = NULL,
    variable = NULL
  ) {
    # variable name must be in snake_case
    is_bare_string(name) &&  str_detect(name, "^[a-z]+(_[a-z0-9A-Z]+)*$") || error(
      "invalid {.field name} = {.q {name %||% missing_val}}",
      i = "{.q snake_case} variable name expected"
    )

    !(name %in% c("cell_id", "year")) || error(
      "invalid {.field name} = {.q {name}}",
      i = "field name is reserved and should not be declared explicitly"
    )

    # description must be a string
    is_list(description) && is_named(description) || error(
      "invalid {.field description} = {.q {description %||% missing_val}}",
      i = "table with components {.field description.short} and {.field description.temporal} expected"
    )

    is_bare_string(description[["short"]]) || error(
      "invalid {.field description.short} = {.q {description[['short']] %||% missing_val}}",
      i = "expected string"
    )

    is_bare_string(description[["temporal"]]) || error(
      "invalid {.field description.temporal} = {.q {description[['temporal']] %||% missing_val}}",
      i = "expected string"
    )

    is_bare_string(description[["detail"]] %||% "") || error(
      "invalid {.field description.temporal} = {.q {description[['temporal']] %||% missing_val}}",
      i = "expected string"
    )


    # type must be one of the registered types
    is_bare_string(type) && (type %in% names(schema_field_types)) || error(
      "invalid {.field type} = {.q {type %||% missing_val}}",
      i = "expected one of: {.q {names(schema_field_types)}}"
    )

    # aggregation must be one of the registered aggregation types
    is_bare_string(aggregation) && (aggregation %in% names(schema_field_aggregations)) || error(
      "invalid {.field aggregation} = {.q {aggregation %||% missing_val}}",
      i = "expected one of: {.q {names(schema_field_aggregations)}}"
    )

    # data_source must be provided as a character vector
    is_bare_character(data_source) || error(
      "invalid {.field data_source} = {.q {data_source %||% missing_val}}",
      i = "expected data source name(s)"
    )

    # unit must be a string, if provided (unit is mandatory for type = "quantity")
    (is_null(units) && type != "quantity") || is_bare_string(units) || error(
      "invalid {.field units} = {.q {units %||% missing_val}}",
      i = sprintf("expected string %s", if (type == "quantity") "(mandatory for quantity variables)")
    )

    # variable data prototype
    ptype <- schema_field_types[[type]]$ptype

    # id reference
    if (type == "id") {
      is_bare_string(reference_schema) || error(
        "invalid {.field reference_schema} = {.q {reference_schema %||% missing_val}}",
        i = "expected string (must be specified for {.field type} = {.q id})"
      )

      is_bare_string(reference_type) && (reference_type %in% c("integer", "string")) || error(
        "invalid {.field reference_type} = {.q {reference_type %||% missing_val}}",
        i = "expected 'integer' or 'string' (must be specified for {.field type} = {.q id})"
      )

      is_bare_string(reference_uri) || error(
        "invalid {.field reference_uri} = {.q {reference_uri %||% missing_val}}",
        i = "expected string (must be specified for {.field type} = {.q id})"
      )

      ptype <- switch(reference_type,
        integer = integer(),
        string = character()
      )
    } else {
      identical(reference_schema, reference_schema) || error(
        "invalid {.field reference_system} = {.q {reference_system}}",
        i = "reference schema can only be specified for {.field type} = {.q id}"
      )

      is_null(reference_type) || error(
        "invalid {.field reference_type} = {.q {reference_type}}",
        i = "reference type can only be specified for {.field type} = {.q id}"
      )

      is_null(reference_uri) || is_bare_string(reference_uri) || error(
        "invalid {.field reference_uri} = {.q {reference_uri}}",
        i = "string uri expected"
      )
    }

    # nested list
    missing(variable) || (type == "list") || error(
      "nested variable specification is only valid for {.field type} = {.q list}"
    )

    nested_vars <- if (type == "list") {
      !missing(variable) && is_bare_list(variable) || error(
        "missing nested variable specification for {.field type} = {.q list}"
      )

      nested_vars <- parse_vars(variable)
      ptype <- vctrs::list_of(.ptype = new_tibble(set_names(nested_vars$ptype, nested_vars$field)))
      nested_vars$field <- paste0(name, "$", nested_vars$field)

      nested_vars
    }

    # no unknown fields
    (...length() == 0L) || error(
      "unknown variable metadata {.field {(...names())}}"
    )


    tibble(
      field = name,
      ptype = list(ptype),
      description_short = description$short,
      description_temporal = description$temporal,
      description_detail = description$detail %||% NA_character_,
      units = units %||% NA_character_,
      aggregation = aggregation %||% NA_character_,
      data_source = list(data_source)
    ) |>
    vec_rbind(nested_vars)
  }



  # types <- list(

  # )

  parse_vars <- function(toml) {
    schema <- NULL
    for (var_toml in toml) {
      schema <- vec_rbind(schema, exec(parse_var, !!!var_toml))
    }

    schema
  }

  parse_vars(toml)
}


# adds system fields (cell_id and year) to the artifact schema
extend_artifact_schema <- function(
  schema,
  ...,
  data,
  has_cell_id = has_name(data, "cell_id"),
  has_year = has_name(data, "year")
) {
  bind_rows(
    if (has_cell_id) tibble(field = "cell_id", ptype = list(character())),
    if (has_year) tibble(field = "year", ptype = list(integer())),
    schema
  )
}

# type support: compatibility between cli_ansi_string and character
vec_ptype2.cli_ansi_string.character <- function(x, y, ...) {
  cli::ansi_string(character())
}

vec_ptype2.character.cli_ansi_string <- function(x, y, ...) {
  cli::ansi_string(character())
}

vec_cast.cli_ansi_string.character <- function(x, to, ...) {
  cli::ansi_string(x)
}

vec_cast.character.cli_ansi_string <- function(x, to, ...) {
  as.character(x)
}

# incomplete and mismatched type sentinels
incomplete_type <- new_partial(.marker = new_environment(), class = "incomplete")
mismatched_type <- new_partial(.marker = new_environment(), class = "mismatched")
uniform_type <- function(val) new_partial(val = val, class = "uniform")

vec_ptype_full.incomplete <- function(x, ...) {
  "incomplete"
}

vec_ptype_abbr.incomplete <- function(x, ...) {
  "incomplete"
}

vec_ptype_full.mismatched <- function(x, ...) {
  "mismatched"
}

vec_ptype_abbr.mismatched <- function(x, ...) {
  "mismatched"
}

vec_ptype_full.uniform <- function(x, ...) {
  sprintf("uniform:%s", as_label(x$val))
}

vec_ptype_abbr.uniform <- function(x, ...) {
  sprintf("uniform:%s", as_label(x$val))
}


# generic homogenous list of data frames
#
# this is a weak type used for type checks only, to allow the
# casts to suceed if the nested data frames have different shapes and types
list_of_tbl_ptype <- new_vctr(list_of(.ptype = tibble()), class = "list_of_tbl")

vec_ptype_full.list_of_tbl <- function(x, ...) {
  "list-of tbl"
}

vec_ptype_abbr.list_of_tbl <- function(x, ...) {
  "list-of tbl"
}

check_is_list_of_tbl <- function(x) {
  obj_is_list(x) &&
  !is_named(x) &&
  every(x, function(X) is_null(X) || is.data.frame(X)) ||
  stop_incompatible_cast(
    x,
    list_of_df_ptype,
    x_arg = caller_arg(x),
    to = "list-of tbl"
  )
}

vec_ptype2.list.list_of_df <- function(x, y, ...) {
  check_is_list_of_tbl(x)
  list_of_df_ptype
}

vec_ptype2.list_of_tbl.list <- function(x, y, ...) {
  check_is_list_of_tbl(x)
  list_of_df_ptype
}

vec_cast.list.list_of_tbl <- function(x, to, ...) {
  x
}

vec_cast.list_of_tbl.list <- function(x, to, ...) {
  check_is_list_of_tbl(x)
  x
}
