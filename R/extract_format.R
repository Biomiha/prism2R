#' Extract the prism file format
#' This is a utility function that determines which prism format the file is saved in.
#' There are 3 possible formats, i.e. "pzf", "pzfx" or "prism". The "pzf" format is very old and a binary format that is not parseable in R so we will ignore it.
#'
#' @param prism_file a string - the file path
#' @noRd
extract_format <- function(prism_file) {
  format <- getExtension(prism_file)
  if(!format %in% c("pzf", "pzfx", "prism")) stop("The selected file is not a prism file.")
  if(format == "pzf") stop("The old pzf format is a binary format and R is not able to parse it. Please save the file as either '.pzfx' or '.prism'.")
  return(format)
}
