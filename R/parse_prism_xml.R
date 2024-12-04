#' Parse prism xml
#' This is a utility function that parses the xml contents of the older prism format ".pzfx".
#'
#' @param pzfx_file a string - the file path
#' @noRd
parse_prism_xml <- function(pzfx_file) {
  if(extract_format(pzfx_file) != "pzfx") stop("Not a '.pzfx' file.")
  prism_xml <- xml2::read_xml(pzfx_file)
  # strip namespaces
  xml2::xml_ns_strip(x = prism_xml)
  return(prism_xml)
}
