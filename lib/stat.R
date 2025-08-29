gini <- function(x) {
  if (anyNA(x)) error("NAs detected")
  if (any(x < 0)) error("gini index is only defined for non-negative values")

  # corner cases
  if (length(x) == 1L) return(NA_real_)
  if (n_distinct(x) == 1L) return(0.0)
  if (all(x == 0)) return(0.0)


  DescTools::Gini(x)
}
