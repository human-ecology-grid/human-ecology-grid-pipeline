with_data <- function(data, expr) {
  quo <- enquo(expr)
  eval_tidy(quo, data)
}
