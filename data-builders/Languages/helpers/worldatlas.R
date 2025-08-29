# load the Atlas of the World's Languages (Asher et al. 2007)
progress_message("Loading Atlas of the World's Languages")

worldatlas <- cached(read_file(unz(
    data_uri("GlottographyAtlas", "asher2007world-v1.0.0.zip"),
    filename = "Glottography-asher2007world-349af75/cldf/contemporary/languages.geojson"
  )) |>
  yyjsonr::read_geojson_str() |>
  as_tibble() |>
  select(
    glottocode = `cldf:languageReference`,
    geometry
  ) |>
  mutate(
    geometry = make_valid_s2_geography(geometry)
  ) |>
  mutate(
    source = "GlottographyAtlas"
  )
)

# the Atlas underrepresents the distribution of English, so we augment it using country polygons for
# Canada (sans Quebec), USA, and New Zealand
worldatlas <- local({
  ne_countries <- st_read(data_uri("NaturalEarth", "ne_10m_admin_0_countries"), quiet = TRUE) |> as_tibble()
  s2_opt <- s2_options(dimensions = "polygon")

  # english-speaking countries
  en_polygons <- {
    ne_countries |>
    filter(ISO_A3 %in% c("CAN", "USA", "NZL")) |>
    pull(geometry) |>
    s2_union_agg(options = s2_opt) |>
    s2_difference(filter(worldatlas, glottocode == "stan1290")$geometry, options = s2_opt)
  }

  worldatlas$geometry[worldatlas$glottocode == "stan1293"] <- s2_union(
    worldatlas$geometry[worldatlas$glottocode == "stan1293"],
    en_polygons,
    options = s2_opt
  )

  worldatlas
})
