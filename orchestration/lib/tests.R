# run testthat tests in parallel
#
# the test source code (as text) is supplied in the named list `tests`
# since testthat asumes that tests are run on a package, we set up a temporary
# package scaffold
run_tests <- function(tests) {
  # all tests are run in a temporary location
  test_dir <- local_tempdir(pattern = "human_ecology_grid_tests")
  check_errors(fs::dir_delete(test_dir))
  fs::dir_create(test_dir)

  # restore the working directory
  local_dir(test_dir)

  # set up a package scaffold for testing
  capture.output(suppressMessages({
    usethis::create_package("DataTests",  roxygen = FALSE,  open = TRUE,  rstudio = FALSE,  check_name = FALSE)
    usethis::use_testthat(parallel = TRUE)
  }))

  # emit the tests
  for (i in seq_along(tests)) {
    write_lines(tests[[i]], paste0("tests/testthat/test-", names(tests)[[i]], ".R"))
  }

  # run the tests
  env <- c("TESTTHAT_PARALLEL" = "TRUE", "TESTTHAT_CPUS" = as.character(max(1, n_available_cores() %/% 2)))
  callr::r(function() {
    options(cli.dynamic = TRUE, cli.ansi = TRUE, cli.num_colors = 256)

    testthat::test_local()
  }, show = TRUE, env = env)
}
