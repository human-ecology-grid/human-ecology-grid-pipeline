#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <stddef.h>
#include <assert.h>

#include "dggal/dggal.h"
#include "geometry.h"
#include "dggal-util.h"
#include "cmdarg.h"

#include "ndjson.h"

typedef struct {
  size_t count;
  DGGRSZone* zones;
  GeoPoint* centroids;
} ZoneInfo;

void emit_zone_json(DGGRS dggrs, DGGRSZone zone, int refinement, int subzone_depth, bool adjacency, bool tesselate) {
  bool JSON_FIRST = true;
  FILE* JSON_OSTREAM = stdout;

  // get zone geometry
  GeoPoint centroid = get_zone_centroid(dggrs, zone);

  GeoPoint vertices_data[6];
  GeoPoints vertices = collect_vertices(dggrs, zone, vertices_data);
  ensure_ccw_geo_polygon(centroid, vertices);

  size_t n_edges = (int) (vertices.end - vertices.begin);

  JSON_OBJECT({
    // zone ID as string
    JSON_KEY("id", JSON_VALUE("\"%s\"", get_zone_text_id(dggrs, zone)))

    // zone shape (pendagon or hexagon)
    JSON_KEY("shape", JSON_VALUE("%ld",  n_edges))

    // zone centroid coordinates (lon, lat)
    JSON_KEY("centroid", JSON_VALUE(
      "[%.*g, %.*g]",
      DBL_DECIMAL_DIG, TO_DEGREES(centroid.lon),
      DBL_DECIMAL_DIG, TO_DEGREES(centroid.lat)
    ))

    // Adjacent zone information
    if (adjacency) JSON_KEY("neighbors", JSON_ARRAY({
      DGGRSZone neighbors[6];

      int n_neighbors = get_zone_neighbors(dggrs, zone, neighbors);
      assert(n_neighbors == n_edges);

      for (size_t i = 0; i < n_edges; i++) {
        // identify common vertices
        GeoPoint neighbour_vertices_data[6];
        GeoPoints neighbour_vertices = collect_vertices(dggrs, neighbors[i], neighbour_vertices_data);

        GeoPoint* v0 = find_common_edge(vertices, neighbour_vertices);
        GeoPoint* v1 = (v0 == vertices.end - 1) ? vertices.begin : v0 + 1;

        JSON_ITEM(JSON_OBJECT({
          JSON_KEY("neighbor_id", JSON_VALUE("\"%s\"", get_zone_text_id(dggrs, neighbors[i])))

          JSON_KEY("neighbor_edge", JSON_VALUE(
            "[%.*g, %.*g, %.*g, %.*g]",
            DBL_DECIMAL_DIG, TO_DEGREES(v0->lon),
            DBL_DECIMAL_DIG, TO_DEGREES(v0->lat),
            DBL_DECIMAL_DIG, TO_DEGREES(v1->lon),
            DBL_DECIMAL_DIG, TO_DEGREES(v1->lat)
          ))
        }))
      }
    }))

    // subzone IDs
    if (subzone_depth > 0) JSON_KEY("subzones", JSON_ARRAY({
      Array subzones = DGGRS_getSubZones(dggrs, zone, subzone_depth);
      Iterator it;

      Container_get_firstIterator(subzones, &it);
      do {
        DGGRSZone subzone = (DGGRSZone) Iterator_get_data(&it);
        JSON_ITEM(JSON_VALUE("\"%s\"", get_zone_text_id(dggrs, subzone)));
      } while (Iterator_next(&it));

      Iterator_free(&it);
      Container_free(subzones);
    }))

    // tesselated triangles
    if (tesselate) JSON_KEY("subtriangles", JSON_OBJECT({
      GeoPoint triangle_vertices[24*3];
      int n = tesselate_cell(centroid, vertices, triangle_vertices);

      JSON_KEY("lon", JSON_ARRAY({
        for (GeoPoint *v = triangle_vertices; v < triangle_vertices + n; v++) {
          JSON_ITEM(JSON_VALUE("%.*g", DBL_DECIMAL_DIG,  TO_DEGREES(v->lon)));
        }
      }))

      JSON_KEY("lat", JSON_ARRAY({
        for (GeoPoint *v = triangle_vertices; v < triangle_vertices + n; v++) {
          JSON_ITEM(JSON_VALUE("%.*g", DBL_DECIMAL_DIG,  TO_DEGREES(v->lat)));
        }
      }))
    }))

    // zone polygon (WGS84)
    JSON_KEY("geometry", {
      GeoPoints geometry = vertices;
      if (refinement != 0) {
        geometry = collect_refined_vertices(dggrs, zone, refinement);
        ensure_ccw_geo_polygon(centroid, geometry);
      }

      JSON_GEOPOLYGON_CLOSED(geometry);

      if (refinement != 0) GeoPoints_free(&geometry);
    })
  })
}

int main(int argc, char * argv[]) {
  Application app = ecrt_init(null, true, false, argc, argv);
  Module mDGGAL = dggal_init(app);
  CmdArgs args = parse_args(argc, argv);

  DGGRS dggrs = Instance_new(eC_findClass(mDGGAL, args.type));

  // collect the zones
  size_t n_zones = DGGRS_countZones(dggrs, args.level);
  DGGRSZone* zones = list_zones(dggrs, args.level);

  // progress report
  bool show_progress = n_zones > 10000;
  double progress = 0;
  size_t n_remaining_in_chunk = n_zones/10;

  for (size_t i = 0; i < n_zones; i++) {
      emit_zone_json(dggrs, zones[i], args.refinement, args.subzones, args.adjacency, args.tesselate);
      fputc('\n', stdout);

      // progress report
      if (show_progress && (--n_remaining_in_chunk == 0)) {
        n_remaining_in_chunk = n_zones/10;
        progress += 10;
        fprintf(stderr, "%.2f%%...", progress);
      }
  }

  free(zones);
}
