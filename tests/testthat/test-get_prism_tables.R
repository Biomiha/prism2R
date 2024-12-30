# get_prism_tables unit tests
# Test cases
test_that("get_prism_tables works correctly", {
  tables_in_files <- c("XY: Entering replicate data", "XY: Entering mean with error values", "Grouped: Entering replicate data",
                       "Data 6", "Survival: Two groups", "Data - missing columns", "Y SEN", "Y CVN", "Y SD", "Y SE", "Y CV", "Y error", "Y high low", "Data 6")
  # Test with a valid .prism file
  prism_file <- system.file("extdata", "demo_dataset.prism", package = "prism2R")
  expect_equal(get_prism_tables(prism_file), tables_in_files)
})

test_that("get_prism_tables handles errors gracefully", {
  # Test with a non-existent file
  suppressWarnings(expect_error(get_prism_tables("nonexistent_file.prism"), "cannot open the connection"))

  # Test with a file that is not a valid prism archive
  invalid_file <- tempfile(fileext = ".prism")
  file.create(invalid_file)
  expect_error(get_prism_tables(invalid_file), "Not a valid .prism file.")
})

test_that("get_analysis_tables errors on empty archive", {
  empty_file <- system.file("extdata/.for_testing", "empty.prism", package = "prism2R")
  expect_error(
    get_prism_tables(empty_file),
    "`file` must be a length one character vector or numeric"
  )
})
