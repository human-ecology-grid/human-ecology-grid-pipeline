progress_message("Loading Glottolog")

glottolog_languoids <- cached(local({
  glottolog_languoids <- read_csv(unz(
    data_uri("Glottolog", "glottolog-cldf-v5.1.zip"),
    filename = "glottolog-glottolog-cldf-40f58f3/cldf/languages.csv",
  ), show_col_types = FALSE)

  # remove bookkeeping
  glottolog_languoids <- filter(glottolog_languoids,
    !(Family_ID %in% "book1242"),
    Glottocode != "book1242"
  )

  # translate aes codes to factor
  glottolog_codes <- read_csv(unz(
    data_uri("Glottolog", "glottolog-cldf-v5.1.zip"),
    filename = "glottolog-glottolog-cldf-40f58f3/cldf/codes.csv",
  ), show_col_types = FALSE)

  # read the aes (endangerment status) variable
  glottolog_values <- read_csv(unz(
    data_uri("Glottolog", "glottolog-cldf-v5.1.zip"),
    filename = "glottolog-glottolog-cldf-40f58f3/cldf/values.csv",
  ), show_col_types = FALSE)

  glottolog_aes <- glottolog_values |> filter(
    Parameter_ID == "aes"
  ) |>
  mutate(
    aes = as.integer(Value)
  ) |>
  select(
    # note: Language_ID field in Values is the Glottocode of the languoid
    #       while Language_ID in the main languoid table is the glottocode
    #       of the parent languages for dialects
    ID = Language_ID,
    aes
  )

  # build the languoid table
  glottolog_languoids <- left_join(
    glottolog_languoids,
    glottolog_aes,
    join_by(ID)
  ) |>
  select(
    Glottocode, Level, Name, Language_ID, Family_ID, Is_Isolate, aes
  )

  # inherit the endangerment level from the language for dialects
  glottolog_languoids <- left_join(
    glottolog_languoids,
    select(glottolog_languoids, Language_ID = Glottocode, Language_aes = aes),
    join_by(Language_ID)
  ) |>
  mutate(
    aes = as.integer(if_else(is.na(aes), Language_aes, aes))
  ) |>
  # add family names
  left_join(
    select(glottolog_languoids, Family_ID = Glottocode, family_name = Name),
    join_by(Family_ID)
  ) |>
  select(
    glottocode = Glottocode,
    name = Name,
    Family_ID,
    family_name,
    level = Level,
    is_isolate = Is_Isolate,
    endangerment_status = aes
  )
}))
