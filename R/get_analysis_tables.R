#' Get preferred analysis tables from a .prism file
#'
#' @param prism_file The path to the prism file
#'
#' @return A named list of data.frames with model parameters
#' @export
#'
#' @examples
#' # prism_file is the file path to the .prism file. Analysis data tables in Prism can include multiple (quite often hidden) tabs.
#' # One of these is usually designated as the preferred tab and this is returned as a list object, commonly a data.frame.
#' demo_file <- system.file("extdata", "demo_dataset.prism", package = "pRism")
#' get_analysis_tables(demo_file)
get_analysis_tables <- function(prism_file) {
  if(extract_format(prism_file) != "prism") stop("Not a valid .prism file.")
  files_in_archive <- archive::archive(prism_file)
  if(nrow(files_in_archive) == 0) stop("Not a valid .prism file.")
  files_in_archive$rowid <- seq_along(files_in_archive$path)
  file_number <- subset.data.frame(x = files_in_archive, subset = grepl(pattern = "document.json", x = path))[["rowid"]]
  top_level_json <- jsonlite::fromJSON(archive::archive_read(archive = prism_file, file = file_number))
  analysis_sheet_ids <- top_level_json$sheets$analyses
  analysis_sheet_names <- unlist(top_level_json$sheetAttributesMap[analysis_sheet_ids])
  analysis_data_tables <- lapply(analysis_sheet_ids, function(x) {
    # Get table data
    filtered_dataframe <- subset.data.frame(x = files_in_archive, subset = grepl(pattern = x, x = path))
    file_number <- subset.data.frame(x = filtered_dataframe, subset = grepl(pattern = "sheet.json", x = path))[["rowid"]]
    analysis_level_json <- jsonlite::fromJSON(archive::archive_read(archive = prism_file, file = file_number))
    analysis_datatable_id <- analysis_level_json$preferredResultSheet
    analysis_datatable_json_id <- subset.data.frame(x = files_in_archive, subset = grepl(pattern = analysis_datatable_id, x = path))[["rowid"]]
    analysis_level_json2 <- jsonlite::fromJSON(archive::archive_read(archive = prism_file, file = analysis_datatable_json_id))
    filtered_dataframe2 <- subset.data.frame(x = files_in_archive, subset = grepl(pattern = analysis_level_json2$dataSheet, x = path))
    analysis_level_json2_id <- subset.data.frame(x = filtered_dataframe2, subset = grepl(pattern = "sheet.json", x = path))[["rowid"]]
    analysis_level_json3 <- jsonlite::fromJSON(archive::archive_read(archive = prism_file, file = analysis_level_json2_id))
    filtered_dataframe3 <- subset.data.frame(x = files_in_archive, subset = grepl(pattern = analysis_level_json3$table$uid, x = path))
    analysis_level_json3_id <- subset.data.frame(x = filtered_dataframe3, subset = grepl(pattern = "data.csv", x = path))[["rowid"]]
    analysis_table_out <- readr::read_csv(archive::archive_read(archive = prism_file, file = analysis_level_json3_id), col_names = FALSE)
    # Get table headers
    header_datasets <- analysis_level_json3$table$dataSets
    headers <- lapply(header_datasets, function(y) {
      header_file_number <- subset.data.frame(x = files_in_archive, subset = grepl(pattern = y, x = path))[["rowid"]]
      header_json <- jsonlite::fromJSON(archive::archive_read(archive = prism_file, file = header_file_number))
      if(!is.null(header_json$title)) return(header_json$title)
    })
    colnames(analysis_table_out)[-1] <- headers
    return(analysis_table_out)
  })
  names(analysis_data_tables) <- analysis_sheet_names
  return(analysis_data_tables)
}
