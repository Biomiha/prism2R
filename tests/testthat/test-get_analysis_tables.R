# get_analysis_tables unit tests
# Test cases
test_that("get_analysis_tables returns expected tables for demo dataset", {
  demo_prism <- system.file("extdata", "demo_dataset.prism", package = "prism2R")
  tables <- get_analysis_tables(demo_prism)

  expect_named(tables, c("Nonlin fit of XY: Entering replicate data",
                         "Survival of Survival: Two groups"))
  expect_true(all(vapply(tables, is.data.frame, logical(1))))
})

test_that("get_prism_tables handles errors gracefully", {
  # Test with a non-existent file
  suppressWarnings(expect_error(get_prism_tables("nonexistent_file.prism"), "cannot open the connection"))

  # Test with a file that is not a valid prism archive
  invalid_file <- tempfile(fileext = ".prism")
  file.create(invalid_file)
  expect_error(get_analysis_tables(invalid_file), "Not a valid .prism file.")
})

test_that("get_analysis_tables errors on empty archive", {
  empty_file <- system.file("extdata/.for_testing", "empty.prism", package = "prism2R")
  expect_error(
    get_analysis_tables(empty_file),
    "`file` must be a length one character vector or numeric"
  )
})

test_that("get_analysis_tables handles files without analyses", {
  no_analyses <- system.file("extdata/.for_testing", "no_analysis.prism", package = "prism2R")
  expect_equal(length(get_analysis_tables(no_analyses)), 0)
})
