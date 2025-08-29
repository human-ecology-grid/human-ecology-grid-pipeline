# check that we are in the right execution environment
if (!fs::file_exists("/.db-builder")) cli::cli_abort(c(
  "The database pipeline is designed to run from the Docker container",
  i = "Please refer to the documentation on how to run the pipeline"
))

if (fs::file_exists("/data/.novolume")) cli::cli_abort(c(
  "Staging data volume is not attached",
  i = "Please refer to the documentation on how to run the pipeline"
))

ENV_DEVELOPMENT <- startsWith(Sys.getenv("ENV"), "DEV")

suppressMessages({
  library(tidyverse)
  library(withr)
  library(cli)
  library(glue)
})


source("/pipeline/lib/errors.R")
source("/pipeline/lib/parallel.R")
source("/pipeline/lib/async.R")
source("/pipeline/lib/fetch.R")
source("/pipeline/lib/cli.R")

source("/pipeline/orchestration/lib/artifacts.R")
source("/pipeline/orchestration/lib/codetools.R")
source("/pipeline/orchestration/lib/digest.R")
source("/pipeline/orchestration/lib/executor.R")
source("/pipeline/orchestration/lib/schema.R")
source("/pipeline/orchestration/lib/tests.R")

source("/pipeline/orchestration/grid-config.R")
source("/pipeline/orchestration/validate-data.R")
source("/pipeline/orchestration/pipeline-info.R")
source("/pipeline/orchestration/fetch-data-sources.R")
source("/pipeline/orchestration/run-data-builders.R")
source("/pipeline/orchestration/build-outputs.R")

# clean up the product
check_errors(fs::dir_delete("/data/product"))

pipeline <- load_pipeline_info()

# Fetch data sources
cli_h1("Fetching data sources")
fetch_data_sources(pipeline)

# Build all grid resolutions
fingerprints <- map(grid_config[pipeline$config$grid], function(grid_info) {
  # configure the pipeline
  grid_label <- sprintf("grid-%skm", grid_info$resolution)
  pipeline$config$grid <- grid_info
  pipeline$staging_dir <- fs::path("/data/build", grid_label)
  fs::dir_create(pipeline$staging_dir)

  cli_h1("[{.field {grid_label}}] Running data pipeline")
  run_data_builders(pipeline)

  # build the output
  build_outputs(pipeline)
})

# assemble the final product
build_product(pipeline)
