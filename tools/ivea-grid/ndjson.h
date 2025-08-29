#define JSON_OBJECT(...) {                                                    \
  fputs("{", JSON_OSTREAM);                                                   \
  JSON_FIRST = true;                                                          \
  __VA_ARGS__;                                                                \
  JSON_FIRST = false;                                                         \
  fputs("}", JSON_OSTREAM);                                                   \
}

#define JSON_ARRAY(...) {                                                     \
  fputs( "[", JSON_OSTREAM);                                                  \
  JSON_FIRST = true;                                                          \
  __VA_ARGS__;                                                                \
  JSON_FIRST = false;                                                         \
  fputs("]", JSON_OSTREAM);                                                   \
}

#define JSON_KEY(key, ...) {                                                  \
  fputs(JSON_FIRST ? ("\"" key "\":") : (",\"" key "\":") , JSON_OSTREAM);    \
  __VA_ARGS__;                                                                \
  JSON_FIRST = false;                                                         \
}

#define JSON_VALUE(...) {                                                     \
  fprintf(JSON_OSTREAM, __VA_ARGS__);                                         \
}

#define JSON_ITEM(...) {                                                      \
  if (!JSON_FIRST) fputs(",", JSON_OSTREAM);                                  \
  __VA_ARGS__;                                                                \
  JSON_FIRST = false;                                                         \
}

#define JSON_GEOPOLYGON_CLOSED(geo) JSON_OBJECT({                             \
  JSON_KEY("lon", JSON_ARRAY({                                                \
    GeoPoint* v0 = (geo).begin;                                               \
    for (GeoPoint* v = v0; v != (geo).end; v++) {                             \
      JSON_ITEM(JSON_VALUE("%.*g", DBL_DECIMAL_DIG,  TO_DEGREES(v->lon)));    \
    }                                                                         \
    JSON_ITEM(JSON_VALUE("%.*g", DBL_DECIMAL_DIG,  TO_DEGREES(v0->lon)));     \
  }))                                                                         \
                                                                              \
  JSON_KEY("lat", JSON_ARRAY({                                                \
    GeoPoint* v0 = (geo).begin;                                               \
    for (GeoPoint* v = v0; v != (geo).end; v++) {                             \
      JSON_ITEM(JSON_VALUE("%.*g", DBL_DECIMAL_DIG,  TO_DEGREES(v->lat)));    \
    }                                                                         \
    JSON_ITEM(JSON_VALUE("%.*g", DBL_DECIMAL_DIG,  TO_DEGREES(v0->lat)));     \
  }))                                                                         \
})

#define JSON_GEOPOLYGON_OPEN(geo) JSON_OBJECT({                               \
  GeoPoint* v0 = (geo).begin;                                                 \
  JSON_KEY("lon", JSON_ARRAY({                                                \
    for (GeoPoint* v = v0; v != (geo).end; v++) {                             \
      JSON_ITEM(JSON_VALUE("%.*g", DBL_DECIMAL_DIG,  TO_DEGREES(v->lon)));    \
    }                                                                         \
  }))                                                                         \
                                                                              \
  JSON_KEY("lat", JSON_ARRAY({                                                \
    for (GeoPoint* v = v0; v != (geo).end; v++) {                             \
      JSON_ITEM(JSON_VALUE("%.*g", DBL_DECIMAL_DIG,  TO_DEGREES(v->lat)));    \
    }                                                                         \
  }))                                                                         \
})
