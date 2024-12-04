# extract_format unit tests
test_that("extract_format function works correctly", {

  # Test with valid .pzfx file
  expect_equal(extract_format("data.pzfx"), "pzfx")

  # Test with valid .prism file
  expect_equal(extract_format("experiment.prism"), "prism")

  # Test with full path to a valid file
  expect_equal(extract_format("/home/user/documents/results.pzfx"), "pzfx")

  # Test with invalid extension
  expect_error(extract_format("document.pdf"), "The selected file is not a prism file.")

  # Test with .pzf file (should raise an error)
  expect_error(
    extract_format("old_data.pzf"),
    "The old pzf format is a binary format and R is not able to parse it. Please save the file as either '.pzfx' or '.prism'."
  )

  # Test with no extension
  expect_error(extract_format("data_file"), "Input file must have an extension.")

  # Test with empty string
  expect_error(extract_format(""), "Input file must have an extension.")

  # Test with file ending in a dot
  expect_error(extract_format("data."), "Input file must have an extension.")
})
