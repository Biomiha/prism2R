#' Parse table contents from a pzfx xml
#' This is a utility function that parses the table contents from the older prism format ".pzfx".
#'
#' @param pzfx_xml an xml_document
#' @param selected_sheet a vector - either a length 1 character vector, a length 1 integer vector or NA
#' @noRd
read_pzfx <- function(pzfx_xml, selected_sheet) {
  pzfx_tables <- xml2::xml_find_all(x = pzfx_xml, xpath = ".//Table")
  if(is.numeric(selected_sheet)) {
    read_pzfx_table(pzfx_table = pzfx_tables[[selected_sheet]])
  } else if(is.character(selected_sheet)) {
    table_names <- get_pzfx_tables(pzfx_xml)
    if(!selected_sheet %in% table_names) stop("The selected table name is not present in the selected prism file.")
    selected_sheet <- which(table_names %in% selected_sheet)
    read_pzfx_table(pzfx_table = pzfx_tables[[selected_sheet]])
  } else if(is.na(selected_sheet)) {
    # if no sheets are selected, the default is to read all tables into a list
    pzfx_tables_list <- vector("list", length = length(pzfx_tables))
    pzfx_tables_list <- lapply(pzfx_tables, read_pzfx_table)
    names(pzfx_tables_list) <- get_pzfx_tables(pzfx_xml)
    pzfx_tables_list
  }
}
