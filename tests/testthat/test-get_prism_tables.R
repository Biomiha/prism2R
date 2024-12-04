# get_prism_tables unit tests
# Test cases
test_that("get_prism_tables works correctly", {
  # Test with a valid prism file
  prism_file <- system.file("extdata", "demo_dataset.prism", package = "pRism")
  expect_equal(get_prism_tables(prism_file), c("XY: Entering replicate data"))
})

test_that("get_prism_tables handles errors gracefully", {
  # Test with a non-existent file
  suppressWarnings(expect_error(get_prism_tables("nonexistent_file.prism"), "cannot open the connection"))

  # Test with a file that is not a valid prism archive
  invalid_file <- tempfile(fileext = ".prism")
  file.create(invalid_file)
  expect_error(get_prism_tables(invalid_file), "Not a valid .prism file.")
})
