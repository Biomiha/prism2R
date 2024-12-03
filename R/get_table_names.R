#' Get the names of the prism data tables
#'
#' @param prism_file The path to the prism file
#' @importFrom archive archive_read
#' @importFrom jsonlite fromJSON
#'
#' @return A vector of table names
#' @export
#'
#' @examples
#' # prism_file is the file path to the prism file. It needs to either be a .pzfx or a .prism file.
#' demo_file <- system.file("extdata", "demo_dataset.prism", package = "pRism")
#' get_table_names(demo_file)
get_table_names <- function(prism_file) {
  format <- extract_format(prism_file)
  if(format == "pzfx") {
    table_names <- get_pzfx_tables(pzfx_xml = parse_prism_xml(pzfx_file = prism_file))
  } else if(format == "prism") {
    table_names <- get_prism_tables(dot_prism_file = prism_file)
  }
  return(table_names)
}
