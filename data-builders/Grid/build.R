# validate the grid config
(PIPELINE_INFO$grid$model == "IVEA") || error(
  "unsupported grid model {.val {PIPELINE_INFO$grid$model}}"
)

grid_level <- PIPELINE_INFO$grid$level

# build the grid
grid <- cached({
  progress_message("Building IVEA grid at level {grid_level}")

  generate_ivea_grid(
    level = grid_level,
    adjacency = TRUE,
    subzones = 0,
    tesselate = TRUE
  )
}, grid_level, generate_ivea_grid)

# extract the subgrid (tesselated triangles)
subgrid <- tibble(
  cell_id = rep(grid$cell_id, list_sizes(grid$subtriangles)),
  subcell_geometry = s2:::new_s2_geography(unlist(grid$subtriangles))
)


grid <- select(grid, !subtriangles)

# Calculate land coverage
progress_message("Calculating land coverage")

grid <- mutate(grid,
  cell_area_km2 = s2_area(cell_geometry)/1e6,
  cell_land_area_km2 = mc_s2_intersection_area(cell_geometry, landmass) / 1e6,
  # cell primarily covers ocean or polar glacial areas,
  # with less than 10% "proper" land coverage
  cell_is_ocean_ice = local({
    land_area_sans_antarctic_km2 <- mc_s2_intersection_area(cell_geometry, landmass_sans_antarctica) / 1e6

    (land_area_sans_antarctic_km2/cell_area_km2) < 0.1
  }),
  # cell boundary land proportion (sea vs. land)
  adj_cells = mc_map(adj_cells, function(X) {
    overlap <- s2_intersection(X$adj_boundary, landmass)
    X$adj_boundary_ocean <- 1 - s2_length(overlap)/s2_length(X$adj_boundary)

    X
  })
)

subgrid <- mutate(subgrid,
  subcell_area_km2 = s2_area(subcell_geometry) / 1e6,
  subcell_land_area_km2 = mc_s2_intersection_area(subcell_geometry, landmass) / 1e6,
  # sub cell primarily covers ocean or polar glacial areas,
  # with less than 10% "proper" land coverage
  subcell_is_ocean_ice = grid$cell_is_ocean_ice[match(cell_id, grid$cell_id)],
  subcell_is_ocean_ice = subcell_is_ocean_ice | (subcell_land_area_km2/subcell_area_km2 < 0.1)
)

# main grid artifacts
grid <- select(grid,
  cell_id,
  cell_centroid,
  cell_area_km2,
  cell_land_area_km2,
  cell_is_ocean_ice,
  adj_cells,
  cell_geometry
)


subgrid <- select(subgrid,
  cell_id,
  subcell_area_km2,
  subcell_land_area_km2,
  subcell_is_ocean_ice,
  subcell_geometry
)

grid_sfc <- mutate(grid, cell_geometry = s2_as_sfc(cell_geometry))
subgrid_sfc <- mutate(subgrid, subcell_geometry = s2_as_sfc(subcell_geometry))

write_artifact(grid, "Grid")
write_artifact(subgrid, "Subgrid")

write_artifact(grid_sfc, "Grid_sfc")
write_artifact(subgrid_sfc, "Subgrid_sfc")


# masked grid artifacts (land only, no ice)
grid <- filter(grid, !cell_is_ocean_ice)
subgrid <- filter(subgrid, !subcell_is_ocean_ice)

grid_sfc <- filter(grid_sfc, !cell_is_ocean_ice)
subgrid_sfc <- filter(subgrid_sfc, !subcell_is_ocean_ice)

write_artifact(grid, "GridNoOceanIce")
write_artifact(subgrid, "SubgridNoOceanIce")

write_artifact(grid_sfc, "GridNoOceanIce_sfc")
write_artifact(subgrid_sfc, "SubgridNoOceanIce_sfc")
