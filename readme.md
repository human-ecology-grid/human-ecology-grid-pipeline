# Human Ecology Grid Database

[![CC BY 4.0][cc-by-shield]][cc-by]

© 2025 Taras Zakharko

## Overview

The Human Ecology Grid is a multi-resolution, spatially indexed database for interdisciplinary research on human–environment interactions. This repository contains the code and metadata for building the database, and is not intended for the end user. The ready-to-use database distribution itself can be downloaded from Zenodo [INSERT LINK].

For methodology and data validation, see the accompanying dataset paper.


This work is licensed under a [Creative Commons Attribution 4.0 International License][cc-by].

[cc-by]: http://creativecommons.org/licenses/by/4.0/
[cc-by-shield]: https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg

### Repository Structure

```
├── data-sources
├── data-builders
├── orchestration
├── lib
├── tools
│   └── ivea-grid
├── product-assets
├── ...
├── config.toml
├── DOCKERFILE
├── references.bib
├── versions.toml
├── build-docker-image.sh
└── dev-build.sh
```

- `data-sources` contains the recipes data sources integrated in the Human Ecology Grid database.
Each recipe includes a data citation, a file digest (with XXH128 checksums), and an optional fetch script. The pipeline orchestrator uses these definitions to fetch and validate the source data.

- `data-builders` contains the recipes for the data aggregation modules. Each recipe includes variable metadata (including data type, aggregation kind, description, and others) and a build script. The pipeline orchestrator uses these definitions to run the data aggregations, verify the results, and assemble the outputs.

- `orchestration` contains the pipeline orchestration scripts responsible for execution, caching, validation, and output assembly. The entry point is the script `orchestration/execute-pipeline.R`

- `lib` contains function definitions and routines used by both the orchestrator and the pipeline scripts. This includes common error handling and reporting routines, accelerated geometry processing, and others.

- `tools` contains additional tools required to build the database. `tools/ivea-grid` contains the source code for generating the IVEA grid geometry at multiple resolutions.

- `product-assets` contains additional files to be included in the final database distribution.

- `config.toml` contains the pipeline configuration, specifying the target grid resolutions, variable output
order, and additional parameters.

- `DOCKERFILE` contains the Docker recipe for building the pipeline Docker image.

- `references.bib` contains the Human Ecology Grid bibliography.

- `versions.toml` contains the list of released versions and associated checksums (used for robust versioning).

- `build-docker-image.sh` is a convenience script for building the pipeline Docker image.

- `dev-build.sh` is a convenience script for running the pipeline in the development mode.

### Usage Notes

## Reproducibility and Containerized Builds

To facilitate reproducible builds, the Human Ecology Grid pipeline executes within a portable [container](https://en.wikipedia.org/wiki/Containerization_(computing)). The container includes all the code, metadata, tests, software dependencies, and any additional tools required to build the database distribution. By providing a fixed container image (essentially a snapshot of a pre-configured virtual Linux system), we can reproduce the database on any hardware or system that supports standard containerization technology.

A `docker`-compatible container runtime is required to run the data pipeline. It has been tested under `colima` on macOS and `docker` on Ubuntu. We do not supporting building the Human Ecology Grid database outside of the container environment.

> [!NOTE]
> We provide an official pre-built Docker image `ghcr.io/human-ecology-grid/pipeline` for each major database
> release. An OCI-compatible image is also archived on Zenodo. The official image is compatible with `x86-64` and `arm-64` architectures. You only need to build your own image if you are extending or modifying the database.

> [!WARNING]
> Full reproducibility is only guaranteed when using the official pre-built image, as new container builds could
> install different software versions.

## External Datasets

The Human Ecology Grid aggregates data from multiple external datasets. The original data is not included with the database distribution. The pipeline will automatically download and verify all required datasets, caching the downloaded data between the builds. Roughly 30 GB of disk space is required to store the original data.

> [!NOTE]
> While most data sources used in the Human Ecology Grid are openly accessible, some require registration.
> The pipeline is still able to fetch these data sources automatically, but a personal access key needs to be
> provided in the configuration file (see below).

## Staging Directory

The pipeline uses the staging directory to store and cache all the downloaded data, intermediate build artifacts, and final database products. This allows efficient incremental builds, as operations are only executed if the dependencies have changed.

The staging directory has the following structure

```
├── data-sources
├── build
├── product
└── config.toml
```

- `data-sources` contains the downloaded and cached external data to be aggregated
- `build` contains the intermediate pipeline outputs (aggregations, cached artifacts, and dependency metadata)
- `product` contains the final database distribution product
- `config.toml` is the local pipeline configuration

Only `config.toml` is provided by the user — other locations are managed by the pipeline and can be deleted during a build step.

## Pipeline Configuration

The pipeline is configured by providing a `config.toml` in the staging directory during the database build. The configuration should include access information for data with restricted access, and a list of grid resolutions to build. Example configuration:

```toml
# config.toml

# personal access token for NativeLand Digital API
# get it at https://api-docs.native-land.ca
[[credentials.NativeLand]]
token = "..."

# list of grid resolutions to build
grid = [
  # IVEA level 5 (~500km distance between cell centroids)
  "IVEA@5",
  # IVEA level 6 (~300km distance between cell centroids)
  "IVEA@6",
  # IVEA level 5 (~150km distance between cell centroids)
  "IVEA@7"
]
```

## Build Instructions (for Unix-like platforms)

#### Step 1: Build the Docker image *(optional)*

```bash
git clone https://github.com/human-ecology-grid/human-ecology-grid-pipeline
cd human-ecology-grid-pipeline
./build-container-image.sh
```

The Docker image recipe is provided in Dockerfile. Note that the script performs a multi-platform build, for x86 and ARM. Ensure that your Docker engine is configured for multi-platform builds or modify the script to only use your host platform.


#### Step 2: Create the staging directory and configure the pipeline

```bash
mkdir human-ecology-grid-staging
cd human-ecology-grid-staging
"$EDITOR" config.toml
```

See [Pipeline Configuration](#pipeline-configuration) about the contents of the `config.toml` file.

#### Step 3: Launch the pipeline

To run the pipeline included within the Docker image (for reproducible builds):

```bash
# if not yet in staging directory
cd human-ecology-grid-staging
docker run --rm --tty -v .:/data ghcr.io/human-ecology-grid/pipeline
```

The above instructions will launch the pipeline stored within the container image. For local
development and testing (e.g. when extending the pipeline), this is not practical, as it would require
Docker image rebuild before every launch. To support development workflows, the pipeline can be also
run in the development mode. This will launch the locally modified pipeline while still running the build inside a Docker container.

To run the pipeline in the development mode:

```bash
# if not yet in staging directory
cd human-ecology-grid-staging
# replace with the path to the pipeline
../human-ecology-grid-pipeline/dev-build.sh
```

## Extending the Human Ecology Grid

The Human Ecology Grid database is designed to be easily extensible.

- To add new external data source, add a new recipe under `data-sources`. At least `metadata.toml` and `filelist.txt` are required. The `filelist.txt` must provide XXH128 checksums for all data files — these checksums will be used to verify that the data has been fetched correctly and has the expected version. Optionally, the data source recipe can also include a fetch script. See `pipeline/data-sources/NativeLand/fetch.R` for an example.

- To add new data aggregation theme, add a new data module recipe under `data-builders`. At least `metadata.toml` and `build.R` are required. The metadata must include the variable schema. The `build.R` script is the main builder script and must call `write_artifact()` to write the main data artifact. See the provided build scripts for examples.

Contributions and bug reports are welcome.
