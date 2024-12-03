#' Read prism data tables into R
#'
#' @param prism_file a string - the path to the prism file
#' @param sheet Which table to read in. `sheet` can either be a number (the index of the table), a character string from the
#' \link[pRism]{get_table_names} output or NA (default),
#' in which case all tables in the file are read.
#'
#' @return A data.frame that tries to reproduce the data table output in prism.
#' @export
#'
#' @examples
#' # prism_file is the file path to the prism file. It needs to either be a .pzfx or a .prism file.
#' demo_file <- system.file("extdata", "demo_dataset.prism", package = "pRism")
#' read_prism(demo_file, sheet = 1) # This will read only the first data table.
#' read_prism(demo_file, sheet = "XY: Entering replicate data") # This will read the selected data table.
#' read_prism(demo_file, sheet = NA) # This will read all data tables in the file.
read_prism <- function(prism_file, sheet = NA) {
  format <- extract_format(prism_file)
  if(format == "pzfx") {
    table_data <- read_pzfx(pzfx_xml = parse_prism_xml(pzfx_file = prism_file), selected_sheet = sheet)
  } else if(format == "prism") {
    table_data <- read_prism_archive(dot_prism_file = prism_file, selected_sheet = sheet)
  }
  return(table_data)
}
