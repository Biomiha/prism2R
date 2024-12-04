# get_pzfx_tables unit tests
test_that("get_pzfx_tables function works correctly", {

  # Create a mock XML structure
  xml_content <- '
  <GraphPadPrismFile>
    <TableSequence>
      <Ref ID="Table0" Selected="1"/>
      <Ref ID="Table1" Selected="1"/>
    </TableSequence>
    <Table ID="Table0">
      <Title>Data 1</Title>
    </Table>
    <Table ID="Table1">
      <Title>Data 2</Title>
    </Table>
  </GraphPadPrismFile>'

  mock_xml <- xml2::read_xml(xml_content)

  # Test with valid XML input
  result <- get_pzfx_tables(mock_xml)
  expect_type(result, "character")
  expect_length(result, 2)
  expect_equal(result, c("Data 1", "Data 2"))

  # Test with XML input containing no tables
  xml_no_tables <- xml2::read_xml("<GraphPadPrismFile></GraphPadPrismFile>")
  expect_length(get_pzfx_tables(xml_no_tables), 0)

  # Test with XML input containing tables but no titles
  xml_no_titles <- xml2::read_xml('
  <GraphPadPrismFile>
    <Table ID="Table0"></Table>
    <Table ID="Table1"></Table>
  </GraphPadPrismFile>')
  expect_equal(get_pzfx_tables(xml_no_titles), as.character(c(NA, NA)))

  # Test with non-XML input (should raise an error)
  expect_error(get_pzfx_tables("not an XML document"), "Not an xml document")
  expect_error(get_pzfx_tables(list()), "Not an xml document")
})
