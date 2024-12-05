# read_prism unit tests
test_that("read_prism works correctly with demo files", {
  # Read demo file in package
  demo_pzfx <- system.file("extdata", "demo_dataset.pzfx", package = "pRism")
  demo_prism <- system.file("extdata", "demo_dataset.prism", package = "pRism")
  df_tbl <- tibble::as_tibble(structure(list(
    Hours = c(0, 6, 12, 24, 48, 72), Control_1 = c(
      45,
      56, 76, 81, 99, 97
    ), Control_2 = c(34, 58, 72, 95, 100, 110),
    Control_3 = c(NA, 61, 77, 83, 104, 115), Treated_1 = c(
      34,
      41, 52, 63, 72, 78
    ), Treated_2 = c(31, 43, 55, 63, 67, 87), Treated_3 = c(29, 42, 55, NA, 81, NA)
  ), row.names = c(
    NA,
    -6L
  ), class = "data.frame"))
  df_list <- list(df_tbl)
  names(df_list) <- "XY: Entering replicate data"

  # Test with a pzfx file
  expect_equal(read_prism(demo_pzfx)[1], df_list)

  # Test with a prism file
  expect_equal(read_prism(demo_prism)[1], df_list)
})
