#' Get prism file extension
#' This is a utility function that extracts the file extension from the prism file name.
#'
#' @param file a string - the file path
#' @noRd
getExtension <- function(file){
  ex <- strsplit(basename(file), split="\\.")[[1]]
  if(length(ex) < 2) stop("Input file must have an extension.")
  return(ex[[length(ex)]])
}
