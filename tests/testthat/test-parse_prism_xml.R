# parse_prism_xml unit tests
test_that("parse_prism_xml function works correctly", {

  # Read demo file in package
  demo_file <- system.file("extdata", "demo_dataset.pzfx", package = "pRism")
  # Test with a valid .pzfx file
  result <- parse_prism_xml(demo_file)
  expect_s3_class(result, "xml_document")
  expect_equal(xml2::xml_name(xml2::xml_root(result)), "GraphPadPrismFile")
  expect_equal(xml2::xml_attr(xml2::xml_root(result), "PrismXMLVersion"), "5.00")

  # Check if namespaces are stripped
  expect_length(xml2::xml_ns(result), 1)

  # Test with an invalid file extension
  invalid_file <- system.file("extdata", "demo_dataset.prism", package = "pRism")
  expect_error(parse_prism_xml(invalid_file), "Not a '.pzfx' file.")

  # Test with a non-existent file
  expect_error(parse_prism_xml("non_existent_file.pzfx"), "does not exist")

  # Test with a .pzf file (should raise an error)
  pzf_file <- "this_fake.pzf" # doesn't have to exist
  expect_error(
    parse_prism_xml(pzf_file),
    "The old pzf format is a binary format and R is not able to parse it. Please save the file as either '.pzfx' or '.prism'."
  )
})
