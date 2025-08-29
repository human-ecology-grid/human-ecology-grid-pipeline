# we work with masked grid (no ocean) and s2 geometry
grid <- grid_no_ocean_ice
subgrid <- subgrid_no_ocean_ice

# incorporate pre-nested subgrid (will be used for all aggregations)
grid <- nest_join(grid,
  select(subgrid, cell_id, subcell_land_area_km2, subcell_geometry),
  join_by(cell_id),
  name = "subcells"
)
