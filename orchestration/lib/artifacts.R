get_artifact_path <- function(pipeline, stage_name, artifact = stage_name) {
  suffix <- paste0("_", rlang::hash(c(stage_name, artifact)))
  fs::path(pipeline$staging_dir, "artifacts", paste0(stage_name, suffix, ".qs2"))
}

locate_stage_artifacts <- function(pipeline, stage_name) {
  # locate the artifacts
  regexp <- sprintf("^.+/(%s)(_.*)?\\.qs2$", stage_name)

  fs::dir_ls(
    fs::path(pipeline$staging_dir, "artifacts"),
    regexp = regexp,
    type = "file"
  )
}

write_stage_manifest <- function(pipeline, stage, dependencies) {
  manifest <- list(
    artifact = locate_stage_artifacts(pipeline, stage$name) |> hash_files(),
    code = stage$fingerprint,
    dependencies = get_dependency_fingerprints(pipeline, dependencies)
  )

  # write the manifest
  manifest_path <- fs::path(pipeline$staging_dir, paste0(stage$name, ".json"))

  fs::dir_create(fs::path_dir(manifest_path))
  yyjsonr::write_json_file(
    manifest,
    manifest_path,
    opts = yyjsonr::opts_write_json(pretty = TRUE, auto_unbox = TRUE)
  )
}

read_stage_manifest <- function(pipeline, stage) {
  manifest_path <- fs::path(pipeline$staging_dir, paste0(stage$name, ".json"))

  # read the manifest (errors simply means that the stage is invalid)
  tryCatch(yyjsonr::read_json_file(manifest_path), error = function(...) NULL)
}


get_dependency_fingerprints <- function(pipeline, dependencies) {
  # data source fingerprints
  data_sources <- pipeline$data_sources
  fingerprints <- data_sources$fingerprint[match(dependencies, data_sources$name)]

  # artifact dependencies
  is_artifact <- grepl("_[a-z0-9]{32,}\\.qs2", dependencies)

  fingerprints[is_artifact] <- with_dir(fs::path(pipeline$staging_dir, "artifacts"), {
    dependencies[is_artifact] |>
    safe_hash_file()
  })

  set_names(as.list(fingerprints), dependencies)
}
