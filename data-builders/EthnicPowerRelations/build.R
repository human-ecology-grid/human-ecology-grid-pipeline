progress_message("Loading the EPR data")

# Load the EPR data
#
# The main EPR dataset and the GeoEPR dataset do not use matching
# time ranges, so to join them we have to expand the ranges
epr_uri <- data_uri("EPR", "EPR-2021.csv.gz")
geoepr_uri <- data_uri("EPR", "GeoEPR-2021.csv.gz")

ethnic_groups <- cached(local({
  epr <- read_csv(epr_uri, show_col_types = FALSE) |>
    # discard all the data before first coverage year and expand the year range
    filter(to >= min(YEARS)) |>
    mutate(from = pmax(from, min(YEARS))) |>
    mutate(year = purrr::map2(from, to, seq.int)) |>
    select(gwgroupid, year,  gwid, status, reg_aut, size) |>
    unnest(year)

  geo_epr <- read_csv(geoepr_uri, show_col_types = FALSE) |>
      # discard all the data before first coverage year and expand the year range
      filter(to >= min(YEARS)) |>
      mutate(from = pmax(from, min(YEARS))) |>
      mutate(year = purrr::map2(from, to, seq.int)) |>
      select(gwgroupid, year, the_geom) |>
      unnest(year)

  left_join(geo_epr, epr, join_by(year, gwgroupid)) |>
    nest(year = year) |>
    mutate(geometry = {
      s2_geog_from_text(the_geom, check = FALSE) |>
      mc_s2_union(options = s2_options(dimensions = "polygon"))
    }) |>
    select(
      gwgroupid,
      year,
      gwid,
      status,
      rel_autonomous = reg_aut,
      size,
      geometry
    ) |>
    mutate(
      # power relation one-hot recoding
      # see the codebook: https://icr.ethz.ch/data/epr/core
      #
      # note: we skip IRRELEVANT since none appear in the geocoded data
      rel_monopoly = status %in% "MONOPOLY",
      rel_dominant = status %in% "DOMINANT",
      rel_senior_partner = status %in% "SENIOR PARTNER",
      rel_junior_partner = status %in% "JUNIOR PARTNER",
      rel_discriminated = status %in% "DISCRIMINATED",
      rel_powerless = status %in% "POWERLESS",
      rel_self_exclusion = status %in% "SELF-EXCLUSION",
      rel_state_collapse = status %in% "STATE COLLAPSE",
      # unpriviledged is any of "DISCRIMINATED" or "POWERLESS"
      rel_unprivileged = rel_discriminated | rel_powerless
    )
}))

# add the country isocodes
country_codes <- read_artifact("Countries", "CountryCodes") |>
  select(gwcode, isocode)

ethnic_groups <- left_join(ethnic_groups,
  country_codes,
  join_by(gwid == gwcode)
)

progress_message("Mapping ethnic groups to grid")

ethnic_groups_grid <- local({
  loc <- mc_s2_locate_intersections(grid$cell_geometry, ethnic_groups$geometry, .chunk_size = 500L)

  ethnic_groups_grid <- tibble(
    cell_id = grid$cell_id[loc[[1L]]],
    vec_slice(ethnic_groups, loc[[2L]])
  ) |>
  select(
    cell_id,
    year,
    epr_group_code = gwgroupid,
    epr_group_country_gwcode = gwid,
    epr_group_country_isocode = isocode,
    epr_group_rel_monopoly = rel_monopoly,
    epr_group_rel_dominant = rel_dominant,
    epr_group_rel_senior_partner = rel_senior_partner,
    epr_group_rel_junior_partner = rel_junior_partner,
    epr_group_rel_autonomous = rel_autonomous,
    epr_group_rel_unprivileged = rel_unprivileged,
    epr_group_rel_discriminated = rel_discriminated,
    epr_group_rel_powerless = rel_powerless,
    epr_group_rel_self_exclusion = rel_self_exclusion,
    epr_group_rel_state_collapse = rel_state_collapse
  )

  # check that no recoded variable is uniform
  select(ethnic_groups_grid, starts_with("epr_group_rel_")) |>
  unclass() |>
  iwalk(function(data, var) {
    data <- data[!is.na(data)]
    (n_distinct(data) > 1L) || error("EPR status recoding recoding {.field {var}} has less than two levels")
  })

  ethnic_groups_grid
}) |>
# re-nest groups by year
unnest(
  year
) |>
# fix Russia/USSR prior to 1991
mutate(
  epr_group_country_isocode = if_else(
    epr_group_country_gwcode == 365 & year < 1991,
    "SUN",
    epr_group_country_isocode
  )
) |>
nest(
  epr_groups = starts_with("epr_group_")
)

# aggregate ethnic groups data
ethnic_groups_grid <- mutate(ethnic_groups_grid,
  mc_vec_apply(epr_groups, .n_cores = n_available_cores() %/% 2L, function(slice) {
    map(slice, function(X) {
      summarize(X,
        epr_groups_num = n(),
        epr_groups_rel_unpriviledged_num = sum(epr_group_rel_unprivileged, na.rm = TRUE),
        epr_groups_rel_autonomous_num = sum(epr_group_rel_autonomous, na.rm = TRUE)
      )
    }) |> list_unchop()
  }), .before = epr_groups
)

write_artifact(ethnic_groups_grid)
