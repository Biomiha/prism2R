# getExtension unit tests
test_that("getExtension function works correctly", {

  # Test with a simple filename
  expect_equal(getExtension("document.pdf"), "pdf")

  # Test with a filename containing multiple dots
  expect_equal(getExtension("my.favorite.image.jpg"), "jpg")

  # Test with a filename without extension
  expect_error(getExtension("README"), "Input file must have an extension.")

  # Test with a full path
  expect_equal(getExtension("/home/user/documents/report.docx"), "docx")

  # Test with a hidden file (starting with a dot)
  expect_equal(getExtension(".hidden_file.txt"), "txt")

  # Test with an empty string
  expect_error(getExtension(""), "Input file must have an extension.")

  # Test with a filename ending with a dot
  expect_error(getExtension("filename."), "Input file must have an extension.")
})
