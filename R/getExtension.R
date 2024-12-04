#' Get prism file extension
#' This is a utility function that extracts the file extension from the prism file name.
#'
#' @param file a string - the file path
#' @noRd
getExtension <- function(file){
  ex <- strsplit(basename(file), split="\\.")[[1]]
  return(tail(ex, n = 1))
}
