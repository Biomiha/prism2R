# read_pzfx_table unit tests
test_that("read_pzfx_table handles demo data correctly", {
  demo_pzfx <- system.file("extdata", "demo_dataset.pzfx", package = "pRism")
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

  pzfx_xml <- parse_prism_xml(pzfx_file = demo_pzfx)
  pzfx_tables <- xml2::xml_find_all(pzfx_xml, xpath = ".//Table")
  expect_equal(read_pzfx_table(pzfx_table = pzfx_tables[[1]]), df_tbl)
})

# Mock formats
create_sample_xml <- function(y_format, x_format = "numbers", replicates = NULL) {
  xml_template <- sprintf('
    <Table YFormat="%s" XFormat="%s" %s>
      <XColumn>
        <Title>X</Title>
        <Subcolumn>
          <d>1</d>
          <d>2</d>
          <d>3</d>
        </Subcolumn>
      </XColumn>
      <YColumn>
        <Title>Y1</Title>
        <Subcolumn>
          <d>10</d>
          <d>20</d>
          <d>30</d>
        </Subcolumn>
        %s
      </YColumn>
    </Table>
  ', y_format, x_format, ifelse(!is.null(replicates), sprintf('Replicates="%d"', replicates), ""),
                          ifelse(y_format %in% c("replicates", "SDN", "SEN", "CVN", "SD", "SE", "CV", "low-high", "upper-lower-limits"),
                                 '<Subcolumn><d>1</d><d>2</d><d>3</d></Subcolumn>', ""))

  return(xml2::read_xml(xml_template))
}
test_that("read_pzfx_table handles different XFormat types correctly", {
  # Test for XFormat = "none"
  xml_no_x <- create_sample_xml("replicates", x_format = "none", replicates = 2)
  result_no_x <- read_pzfx_table(xml_no_x)
  expect_false("X" %in% colnames(result_no_x))
  expect_equal(colnames(result_no_x), c("Y1_1", "Y1_2"))

  # Test for XFormat = "numbers"
  xml_numbers_x <- create_sample_xml("replicates", x_format = "numbers", replicates = 2)
  result_numbers_x <- read_pzfx_table(xml_numbers_x)
  expect_true("X" %in% colnames(result_numbers_x))
  expect_equal(colnames(result_numbers_x), c("X", "Y1_1", "Y1_2"))
  expect_type(result_numbers_x$X, "double")
})

test_that("read_pzfx_table handles tables without YFormat correctly", {
  xml_no_yformat <- xml2::read_xml('
    <Table XFormat="numbers">
      <XColumn>
        <Title>X</Title>
        <Subcolumn>
          <d>1</d>
          <d>2</d>
          <d>3</d>
        </Subcolumn>
      </XColumn>
      <YColumn>
        <Title>Y1</Title>
        <Subcolumn>
          <d>10</d>
          <d>20</d>
          <d>30</d>
        </Subcolumn>
      </YColumn>
    </Table>
  ')
  result_no_yformat <- read_pzfx_table(xml_no_yformat)
  expect_equal(colnames(result_no_yformat), c("X", "Y1"))
  expect_equal(nrow(result_no_yformat), 3)
})
