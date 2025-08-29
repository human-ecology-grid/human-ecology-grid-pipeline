# combine the datasets
#
# World Atlas takes precedence before Nativeland data
progress_message("Combining language data")

languages <- bind_rows(
  worldatlas,
  anti_join(nativeland, worldatlas, join_by(glottocode))
) |>
# inner join since we also want to filter out invalid glottocodes
inner_join(
  glottolog_languoids,
  join_by(glottocode)
) |>
# a few glottocodes (ex. Ainu) have is_isolate = NA
#
# these are essentially very small groups of closely related
# languages that are not distuinguished on the maps, so we code
# them as isolates here
mutate(
  is_isolate = if_else(is.na(is_isolate), is.na(family_name), is_isolate)
) |>
select(
  glottocode,
  name,
  family_name,
  is_isolate,
  endangerment_status,
  source,
  geometry
) |> rename_with(
  function(x) paste0("language_", x)
)

progress_message("Mapping languages to the grid")

languages_grid <- local({
  loc <- mc_s2_locate_intersections(
    grid$cell_geometry,
    languages$language_geometry,
    .unnest = FALSE
  )

  tibble(
    cell_id = grid$cell_id,
    languages = vec_chop(select(languages, !language_geometry), loc)
  ) |>
  # drop all cells with no detected language coverage
  filter(list_sizes(languages) > 0L)
})

# aggregate the languages
progress_message("Aggregating language data")

languages_grid <- languages_grid |>
mutate(mc_map_vec(languages, function(X) {
    X |> summarize(
      languages_num = n(),
      # number of unique families (includig isolates)
      languages_families_num = n_distinct(if_else(
        language_is_isolate,
        language_glottocode,
        language_family_name
      )),
      # number of endangered languages (aes > 1)
      languages_endangered_num = sum(X$language_endangerment_status > 1, na.rm = TRUE)
    )
}), .before = languages)

write_artifact(languages_grid)
