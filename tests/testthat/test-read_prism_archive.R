# read_prism_archive unit tests
test_that("read_prism_archive works correctly with demo files", {
  # Read demo file in package
  demo_prism <- system.file("extdata", "demo_dataset.prism", package = "prism2R")
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
  # selected sheet is numeric
  expect_equal(read_prism_archive(dot_prism_file = demo_prism, selected_sheet = 1), df_tbl)
  # selected sheet is character
  expect_equal(read_prism_archive(dot_prism_file = demo_prism, selected_sheet = "XY: Entering replicate data"), df_tbl)
  # selected sheet is NA
  df_list <- list(df_tbl)
  names(df_list) <- "XY: Entering replicate data"
  expect_equal(read_prism_archive(dot_prism_file = demo_prism, selected_sheet = NA)[1], df_list)
  # empty column names
  expect_equal(colnames(read_prism_archive(dot_prism_file = demo_prism, selected_sheet = 6)), c("conc", "Negative", "Negative", "Negative", "", "", ""))
  # Non-unique sheet names
  expect_error(read_prism_archive(dot_prism_file = demo_prism, selected_sheet = "Data 6"), "You have chosen multiple sheets. Please choose either just one or `sheet = NA` for all.")
})
