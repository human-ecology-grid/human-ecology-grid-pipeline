#ifndef __GEOMETRY_H__
#define __GEOMETRY_H__

#include <math.h>
#include <float.h>
#include <assert.h>

#include "dggal/dggal.h"

#ifndef M_PI
#   define M_PI 3.1415926535897932384626433832
#endif

#ifndef M_PI_2
#   define M_PI_2 (M_PI/2)
#endif

#ifndef M_2_PI
#   define M_2_PI (2*M_PI)
#endif

#define TO_DEGREES(x) (x / M_PI * 180)

// GeoPoint manipulation
typedef struct {
  GeoPoint* begin;
  GeoPoint* end;
} GeoPoints;

static GeoPoints GeoPoints_alloc(size_t n) {
  GeoPoint* begin = malloc(n * sizeof(GeoPoint));
  assert(begin != NULL);
  return (GeoPoints) {.begin = begin, .end = begin + n};
}

static void GeoPoints_free(GeoPoints* p) {
  if (p->begin != NULL) free(p->begin);

  p->begin = NULL;
  p->end = NULL;
}

static GeoPoint geopoint_normalize(GeoPoint p) {
  assert(fabs(p.lat) <= M_PI_2);
  assert(fabs(p.lon) <= M_2_PI);

  // normalize longitude
  p.lon = (fabs(p.lon) <= M_PI) ? p.lon : p.lon + copysign(M_2_PI, -p.lon);

  return p;
}

static bool geopoint_coincident(GeoPoint a, GeoPoint b, double tol) {
  return fabs(a.lat - b.lat) < tol && fabs(a.lon - b.lon) < tol;
}

static bool geopoint_coincident_any(GeoPoint a, GeoPoints b, double tol) {
  for (GeoPoint* p = b.begin; p != b.end; ++p) {
    if (geopoint_coincident(a, *p, tol)) return true;
  }

  return false;
}

// Unit vectors on a sphere
typedef struct {
  double x, y, z;
} vec;


static vec vec_from_geopoint(GeoPoint g) {
  vec v = (vec) {
    .x = cos(g.lat) * cos(g.lon),
    .y = cos(g.lat) * sin(g.lon),
    .z = sin(g.lat)
  };

  assert(!isnan(v.x) && !isnan(v.y) && !isnan(v.z));
  return v;
}

static GeoPoint vec_to_geopoint(vec v) {
  double lat = atan2(v.z, sqrt(v.x * v.x + v.y * v.y));
  double lon = atan2(v.y, v.x);

  return (GeoPoint) {
    .lat = lat,
    .lon = lon
  };
}

static vec vec_add(vec v0, vec v1) {
  return (vec) {
    .x = v0.x + v1.x,
    .y = v0.y + v1.y,
    .z = v0.z + v1.z
  };
}

static vec vec_sub(vec v0, vec v1) {
  return (vec) {
    .x = v0.x - v1.x,
    .y = v0.y - v1.y,
    .z = v0.z - v1.z
  };
}

static double vec_dot_product(vec v0, vec v1) {
  return v0.x * v1.x + v0.y * v1.y + v0.z * v1.z;
}

static double vec_len(vec v) {
  return sqrt(vec_dot_product(v, v));
}

static vec vec_midpoint(vec v0, vec v1) {
  vec sum = vec_add(v0, v1);
  double scale = sqrt(2.0 + 2.0 * vec_dot_product(v0, v1));

  return (vec) {
    .x = sum.x / scale,
    .y = sum.y / scale,
    .z = sum.z / scale
  };
}

static vec vec_normalize(vec v) {
  double len = vec_len(v);
  return (len > 0) ? (vec) {.x = v.x / len, .y = v.y / len, .z = v.z / len} : (vec) {0};
}

static vec vec_cross_product(vec v0, vec v1) {
  return (vec) {
    .x = v0.y * v1.z - v0.z * v1.y,
    .y = v0.z * v1.x - v0.x * v1.z,
    .z = v0.x * v1.y - v0.y * v1.x
  };
}

static double vec_tripple_product(vec v0, vec v1, vec v2) {
  return vec_dot_product(v0, vec_cross_product(v1, v2));
}

static double vec_distance(vec v0, vec v1) {
  return 2.0*atan2(vec_len(vec_sub(v0, v1)), vec_len(vec_add(v0, v1)));
}

// areas
static double spherical_triangle_area(vec v0, vec v1, vec v2) {
  return 2.0*atan2(
    vec_tripple_product(v0, v1, v2),
    1 + vec_dot_product(v0, v1) + vec_dot_product(v1, v2) + vec_dot_product(v2, v0)
  );
}

static double geo_polygon_signed_area(GeoPoint centroid, GeoPoints vertices) {
  if ((vertices.end - vertices.begin) < 3) return NAN;

  double area = 0.0;

  vec c = vec_from_geopoint(centroid);
  vec v0 = vec_from_geopoint(*(vertices.end - 1));

  for (GeoPoint* g = vertices.begin + 1; g != vertices.end; g++) {
    vec v1 = vec_from_geopoint(*g);
    double tri_area = spherical_triangle_area(c, v0, v1);
    if (fabs(tri_area) < 1e-14) continue;

    if (area * tri_area < 0.0) return(NAN);

    area += tri_area;
    v0 = v1;
  }

  return area;
}

static void ensure_ccw_geo_polygon(GeoPoint centroid, GeoPoints vertices) {
  // check the signed area
  double area = geo_polygon_signed_area(centroid, vertices);
  assert(!isnan(area));

  // polygon is CCW already — nothign to do
  if (area > 0.0) return;

  // reverse the chain
  GeoPoint* p0 = vertices.begin;
  GeoPoint* p1 = vertices.end - 1;

  while (p0 < p1) {
    GeoPoint temp = *p0;
    *p0 = *p1;
    *p1 = temp;
    p0++;
    p1--;
  }

  // verify the orientation again
  area = geo_polygon_signed_area(centroid, vertices);
  assert(area > 0.0);
}

#endif
