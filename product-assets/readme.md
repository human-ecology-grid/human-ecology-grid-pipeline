# Human Ecology Grid Data

## Overview

The Human Ecology Grid is a multi-resolution, spatially indexed database for interdisciplinary research on human–environment interactions. It aggregates variables from multiple domains into a consistent grid, delivered as easy-to-use tabular and geospatial files with rich metadata.

For full methodology and validation, see the accompanying dataset paper (listed in references.bib).

## Quick Use (with R)

The Human Ecology Grid distribution comes with an R script that makes it easy to load and use the data. Simply download the project and load the `scripts/human-ecology-grid.R` script.

```r
library(tidyverse)

# source the support script
source("<path-to-human-ecology-grid>/scripts/human-ecology-grid.R")

# load the data at a specific resolution
cross_section_latest <- human_ecology_grid_data(resolution = 300, year = "latest")
yearly <- human_ecology_grid_data(resolution = 300, year = "all")

# load and plot the cell geometry
grid_cells <- human_ecology_grid_cells(resolution = 300)
ggplot(grid_cells) + geom_sf(aes(fill = cell_is_ocean_ice))

# human_ecology_grid_cells() can also split the geometry on an
# arbitrary meridian for custom maps, such as Pacific-Centered maps
#
# if splitting is not done, the map with render with artifacts due to
# coordinate wrapping
grid_cells <- human_ecology_grid_cells(resolution = 300, split_meridian = -30)

# Pacific-centered Equal Earth map
ggplot(grid_cells) + geom_sf(aes(fill = cell_is_ocean_ice)) + coord_sf(crs = 8859, expand = FALSE)

# load the adjacency matrix for use with spatial autocorrelation analysis
adj_mat <- human_ecology_grid_adjacency_matrix(resolution = 300)
adj_list <- spdep::mat2listw(adj_mat, style = "M", zero.policy = TRUE)
```

> [!TIP]
> The script will attempt to auto-detect the data path. The user can also set the root data path
> explicitly via an option `option(human.ecology.grid.root = <path>)`

> [!TIP]
> For best performance, consider using the [arrow package](https://arrow.apache.org/docs/r/index.html)
> when working with panel data at higher resolutions. The script can load the data as an Arrow `Dataset`
> using `human_ecology_grid_data(..., result_type = "arrow")

## Variables

> [!NOTE]
> Refer to the variable metadata (`variables.xlsx`) for a detailed list of variables and datasets.

All Human Ecology Grid variables follow a unified naming schema. Variable names are multi-word identifiers joined by the underscore character ("snake case" convention), with the first name component referring to the topic or theme and the subsequent name components denoting additional properties in the order of increasing specificity. Optional variable name suffix describes the physical unit or measure. For example, the per-cell population count variable is named `population_total_num`,  while the per-cell urban population count is named `population_urban_num`. Similarly, the length of the total per-cell road network length is named `roads_length_km`, while the total per-cell railway network length is named `railways_length_km`.


> [!NOTE]
> Two special variables refer to the spatial and temporal dimension:
>
> - `cell_id` is the unique global cell id following the [OGC DGGRS Draft](https://docs.ogc.org/DRAFTS/21-038r1.html#ivea3h-dggrs)
> - `year` is the data year (only for panel data)

### Nested data

Some of the variables list observations per grid cell and are stored as lists of nested tables. Examples of these variables are `countries` (list of countries per grid cell) and `languages` (list of languages per grid cell). This data can be processed as a list, or collapsed into a long table.

Example using R

```r
library(tidyverse)

data <- human_ecology_grid_data(resolution = 300, year = "latest")
select(data, cell_id, languages) |> unnest(languages)
```

> [!WARNING]
> Make sure to filter and select relevant portions of the data before unnesting, as
> the unnested table can be exceedingly large at higher grid resolutions

## Repository Structure

```.
├── grid-500km
│   ├── cross-section-latest.parquet
│   ├── yearly
│   │   ├── year=1981/data.parquet
│   │   ├── year=1982/data.parquet
│   │   └── ...
│   │   ├── year=1983/data.parquet
│   ├── extra
│   │   └── grid-adjacency-matrix.rds
│   └── grid.geojson.xz
├── ...
├── metadata.xlsx
├── readme.md
├── references.bib
└── version.json
```

The directories `grid-[resolution]km` contain the gridded data for a given spatial resolution. The grid resolution is the average distance between centroids of adjacent grid cells in kilometers. The higher the resolution (smaller numbers), the finer the grid and smaller the grid cells.

- `variables.xlsx`: contains variable and dataset descriptions
- `references.bib`: citations for data sources and methods
-	`version.json`: database version information and data checksums

### Data Files

-	`grid-[resolution]km/yearly`: panel data (one row per cell and year) stored as Hive-partitioned annual datasets
-	`grid-[resolution]km/cross-section-latest.parquet` — latest data cross-section (one row per cell)
-	`grid-[resolution]km/grid.geojson.xz`: grid cell geometry in GeoJSON format (compressed with `xz` tool)
-	`grid-[resolution]km/extra/grid-adjacency-matrix.rds`: an R `sparseMatrix()` object with grid adjacency data

> [!NOTE]
> The yearly (panel) data is stored using Hive-style partitioning. Instead of one large file containing
> all annual data, the dataset is split into annual data segments stored under `yearly/year=X/data.parquet`.
> This can improve performance and memory usage when working with Hive-partition-aware tools.

> [!TIP]
> The R Arrow library takes advantage of Hive-style partitioning and will only load the data when
> it is needed. For example, `filter(dataset, year == 2000)` will only load the `yearly/year=200/data.parquet`
> file, improving performance and memory usage. Note that you must use
> `human_ecology_grid_data(..., result_type = "arrow")` to take advantage of this optimization. The default
> behavior converts the data into a data frame, which requires the entire dataset to be loaded into memory.


## Citation

Please cite the Human Ecology Grid dataset (including the version used) and the accompanying dataset paper.

```
Zakharko, Taras; Balthasar Bickel. A Comprehensive Multi-Domain Global Dataset for Environmental and Socioeconomic Research. Scientific Data [tbd].

Zakharko, Taras; Balthasar Bickel. Human Ecology Grid Data (version X). Zenodo [tbd].
```
