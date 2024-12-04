#' Get table names from a .prism file
#' This is a utility function that extracts the table names from a prism zip archive.
#'
#' @param dot_prism_file string - the file path to the .prism file
#' @noRd
get_prism_tables <- function(dot_prism_file) {
  if(extract_format(dot_prism_file) != "prism") stop("Not a valid .prism file.")
  files_in_archive <- archive::archive(dot_prism_file)
  if(nrow(files_in_archive) == 0) stop("Not a valid .prism file.")
  files_in_archive$rowid <- seq_along(files_in_archive$path)
  file_number <- subset.data.frame(x = files_in_archive, subset = grepl(pattern = "document.json", x = path))[["rowid"]]
  top_level_json <- jsonlite::fromJSON(archive::archive_read(archive = dot_prism_file, file = file_number))
  sheet_ids <- top_level_json$sheets$data
  sheet_names <- unlist(top_level_json$sheetAttributesMap[sheet_ids])
  return(as.vector(sheet_names))
}
