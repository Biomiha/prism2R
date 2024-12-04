# get_table_names unit tests
test_that("get_table_names works correctly with demo files", {
  # Read demo file in package
  demo_pzfx <- system.file("extdata", "demo_dataset.pzfx", package = "pRism")
  demo_prism <- system.file("extdata", "demo_dataset.prism", package = "pRism")
  # Test with a pzfx file
  expect_equal(get_table_names(demo_pzfx), "XY: Entering replicate data")

  # Test with a prism file
  expect_equal(get_table_names(demo_prism), "XY: Entering replicate data")
})
