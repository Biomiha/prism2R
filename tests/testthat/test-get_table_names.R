# get_table_names unit tests
test_that("get_table_names works correctly with demo files", {
  # Read demo file in package
  demo_pzfx <- system.file("extdata", "demo_dataset.pzfx", package = "pRism")
  demo_prism <- system.file("extdata", "demo_dataset.prism", package = "pRism")
  tables_in_files <- c("XY: Entering replicate data", "XY: Entering mean with error values", "Grouped: Entering replicate data",
                       "Data 6", "Survival: Two groups", "Data - missing columns", "Y SEN", "Y CVN", "Y SD", "Y SE", "Y CV", "Y error", "Y high low", "Data 6")
  # Test with a pzfx file
  expect_equal(get_table_names(demo_pzfx), tables_in_files)

  # Test with a prism file
  expect_equal(get_table_names(demo_prism), tables_in_files)
})
