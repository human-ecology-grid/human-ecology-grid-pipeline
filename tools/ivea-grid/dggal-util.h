#ifndef __UTIL_H__
#define __UTIL_H__

#include "dggal/dggal.h"
#include "geometry.h"

static const char* get_zone_text_id(DGGRS dggrs, DGGRSZone zone) {
  static char buf[256];
  DGGRS_getZoneTextID(dggrs, zone, buf);

  return buf;
}

static GeoPoint get_zone_centroid(DGGRS dggrs, DGGRSZone zone) {
  GeoPoint centroid;
  DGGRS_getZoneWGS84Centroid(dggrs, zone, &centroid);
  return geopoint_normalize(centroid);
}

static GeoPoints collect_vertices(DGGRS dggrs, DGGRSZone zone, GeoPoint* storage) {
  if (storage == NULL) storage = (GeoPoint*) malloc(sizeof(GeoPoint)*6);
  int n = DGGRS_getZoneWGS84Vertices(dggrs, zone, storage);
  for (GeoPoint* v = storage; v != storage + n; v++) {
    *v = geopoint_normalize(*v);
  }

  return (GeoPoints) {.begin = storage, .end = storage + n};
}

static GeoPoints collect_refined_vertices(DGGRS dggrs, DGGRSZone zone, int refinement) {
  // if refinement is zero, just return the regular vertices
  if (refinement <= 0) {
    GeoPoint* v = (GeoPoint*) malloc(sizeof(GeoPoint)*6);
    int n = DGGRS_getZoneWGS84Vertices(dggrs, zone, v);
    return (GeoPoints) {.begin = v, .end = v + n};
  }

  // tolerance for coincidence detection
  // verified to work artifact-free DGGAL
  const double tol = 1e-15;

  Iterator it;
  Array varray = DGGRS_getZoneRefinedWGS84Vertices(dggrs, zone, refinement);

  GeoPoint* v = (GeoPoint*) malloc(sizeof(GeoPoint)*Container_getCount(varray));
  assert(v != NULL);

  size_t i = 0;

  Container_get_firstIterator(varray, &it);
  v[i++] = geopoint_normalize(*((GeoPoint*) Iterator_get_data(&it)));

  while (Iterator_next(&it)) {
    GeoPoint p = geopoint_normalize(* ((GeoPoint*) Iterator_get_data(&it)) );
    if (!geopoint_coincident(v[i - 1], p, tol)) v[i++] = p;
  }

  Iterator_free(&it);
  Container_free(varray);

  // make sure that the last vertex is not almost coincident with the first
  while (i > 0 && geopoint_coincident(v[i - 1], v[0], tol)) i--;

  return (GeoPoints) {.begin = v, .end = v + i};
}

static GeoPoint* find_common_edge(GeoPoints loop0, GeoPoints loop1) {
  // vertex coincidence tolerance
  // verified to work with DGGAL output for common resolutions
  const double tol = 1e-7;

  // edge start
  GeoPoint* v0 = NULL;

  for (GeoPoint* p = loop0.begin; p != loop0.end; p++) {
    if (geopoint_coincident_any(*p, loop1, tol)) {
      v0 = p;
      break;
    }
  }

  // there has to be a common edge
  assert(v0 != NULL);

  // first vertex is matched — the edge could start at the last vertex
  if (v0 == loop0.begin && geopoint_coincident_any(*(loop0.end - 1), loop1, tol)) {
    v0 = loop0.end - 1;
  } else {
    // match should be the edge start
    GeoPoint* v1 = (v0 == loop0.end - 1) ? loop0.begin : (v0 + 1);
    assert(geopoint_coincident_any(*v1, loop1, tol));
  }

  return v0;
}

typedef struct {
  size_t count;
  DGGRSZone* zones;
  GeoPoint* centroids;
} ZoneList;


static DGGRSZone* list_zones(DGGRS dggrs, int level) {
  size_t n_zones = DGGRS_countZones(dggrs, level);
  DGGRSZone* zones = malloc(sizeof(DGGRSZone) * n_zones);

  // copy the data
  Array zones_array = DGGRS_listZones(dggrs, level, NULL);
  Iterator it;
  Container_get_firstIterator(zones_array, &it);

  DGGRSZone* ptr = zones;
  do {
    assert(ptr < zones + n_zones);
    *(ptr++) = (DGGRSZone) Iterator_get_data(&it);
  } while (Iterator_next(&it));

  Iterator_free(&it);
  Container_free(zones_array);

  return zones;
}

static GeoPoint* get_zones_centroids(DGGRS dggrs, DGGRSZone* zones, size_t n) {
  GeoPoint* centroids = (GeoPoint*) malloc(sizeof(GeoPoint) * n);
  for (size_t i = 0; i < n; i++) {
    centroids[i] = get_zone_centroid(dggrs, zones[i]);
  }

  return centroids;
}

static int get_zone_neighbors(DGGRS dggrs, DGGRSZone zone, DGGRSZone neighbors[6]) {
  int n = DGGRS_getZoneNeighbors(dggrs, zone, neighbors, NULL);

  // sanity check
  DGGRSZone nn[6];
  for (int i = 0; i < n; i++) {
    int n1 = DGGRS_getZoneNeighbors(dggrs, neighbors[i], nn, NULL);
    bool match = false;
    for (int j = 0; j < n1; j++) {
      if (nn[j] == zone) {
        match = true;
        break;
      }
    }
    assert(match);
  }

  return n;
}

// single-iteration triangular tesselation
//
// there are up to 24 triangles (or 72 geopoints)
static int tesselate_cell(GeoPoint centroid, GeoPoints vertices, GeoPoint* out) {
  // current output
  int i = 0;

  vec vc = vec_from_geopoint(centroid);

  GeoPoint* p0 = vertices.end - 1;
  vec v0 = vec_from_geopoint(*p0);
  for (GeoPoint* p1 = vertices.begin; p1 != vertices.end; p1++) {
    vec v1 = vec_from_geopoint(*p1);
    //                       m01
    // triangle:    v0 *      x      * v1
    //
    //              m0c  x         x m1c
    //
    //                     vc *
    // Calculate midpoints of each edge
    vec m01 = vec_midpoint(v0, v1);
    vec m0c = vec_midpoint(v0, vc);
    vec m1c = vec_midpoint(v1, vc);

    // Triangle 1: v0, m0c, m01
    out[i++] = *p0;
    out[i++] = vec_to_geopoint(m0c);
    out[i++] = vec_to_geopoint(m01);

    // Triangle 2: m0c, vc, m1c
    out[i++] = vec_to_geopoint(m0c);
    out[i++] = centroid;
    out[i++] = vec_to_geopoint(m1c);

    // Triangle 3: m01, m1c, v1
    out[i++] = vec_to_geopoint(m01);
    out[i++] = vec_to_geopoint(m1c);
    out[i++] = *p1;

    // Triangle 4: m0c, m1c, m01
    out[i++] = vec_to_geopoint(m0c);
    out[i++] = vec_to_geopoint(m1c);
    out[i++] = vec_to_geopoint(m01);

    // next vertex
    p0 = p1;
    v0 = v1;
  }

  return i;
}

#endif
