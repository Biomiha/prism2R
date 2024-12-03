#' Parse prism xml
#' This is a utility function that parses the xml contents of the older prism format ".pzfx".
#'
#' @param pzfx_file a string - the file path
#' @noRd
parse_prism_xml <- function(pzfx_file) {
  stopifnot(extract_format(pzfx_file) == "pzfx")
  prism_xml <- xml2::read_xml(pzfx_file)
  return(prism_xml)
}
