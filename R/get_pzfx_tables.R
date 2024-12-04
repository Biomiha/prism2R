#' Get table names from .pzfx xml
#' This is a utility function that extracts the table names from a pzfx_xml.
#'
#' @param pzfx_xml - an xml document
#' @noRd
get_pzfx_tables <- function(pzfx_xml) {
  if(!"xml_document" %in% class(pzfx_xml)) stop("Not an xml document") # check if xml was parsed correctly
  pzfx_tables <- xml2::xml_find_all(x = pzfx_xml, xpath = ".//Table")
  xml2::xml_text(xml2::xml_find_first(x = pzfx_tables, xpath = ".//Title"))
}
