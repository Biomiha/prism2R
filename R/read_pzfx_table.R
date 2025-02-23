read_pzfx_table <- function(pzfx_table) {
  # Helper functions
  pad_matrix <- function(m, max_rows) {
    if (nrow(m) < max_rows) {
      rbind(m, matrix(NA, nrow = max_rows - nrow(m), ncol = ncol(m)))
    } else m
  }
  
  process_y_columns <- function(y_cols_xml, format_info) {
    y_col_names <- xml2::xml_text(xml2::xml_find_all(x = y_cols_xml, xpath = ".//Title"))
    
    # Extract values from XML
    y_vals <- lapply(y_cols_xml, function(xml_subcol) {
      xml2::xml_text(xml2::xml_children(xml2::xml_find_all(x = xml_subcol, xpath = ".//Subcolumn")))
    })
    
    # Process based on format type
    if (!is.null(format_info$type)) {
      n_cols <- format_info$n_cols
      suffixes <- format_info$suffixes
      
      # Handle replicates case separately
      if (format_info$type == "replicates") {
        n_cols <- as.numeric(table_attrs[["Replicates"]])
        suffixes <- seq_len(n_cols)
      }
      
      # Update column names
      y_col_names <- paste(rep(y_col_names, each = n_cols), suffixes, sep = "_")
      
      # Convert to matrices with appropriate number of columns
      y_vals <- lapply(y_vals, function(x) matrix(data = x, ncol = n_cols))
    }
    
    # Standardize matrix dimensions
    max_rows <- if (is.null(format_info$type)) max(lengths(y_vals)) else max(sapply(y_vals, nrow))
    
    if (!is.null(format_info$type)) {
      y_vals <- lapply(y_vals, pad_matrix, max_rows = max_rows)
    } else {
      y_vals <- lapply(y_vals, function(x) {
        length(x) <- max_rows
        x
      })
    }
    
    # Convert to tibble
    result <- tibble::as_tibble(as.data.frame(Reduce(cbind, y_vals)), .name_repair = "minimal")
    colnames(result) <- y_col_names
    result
  }
  
  # Main processing
  table_attrs <- xml2::xml_attrs(pzfx_table)
  y_cols_xml <- xml2::xml_find_all(x = pzfx_table, xpath = ".//YColumn")
  
  # Define format types and their properties
  format_types <- list(
    "replicates" = list(type = "replicates", n_cols = NULL, suffixes = NULL),
    "SDN" = list(type = "stats", n_cols = 3, suffixes = c("Mean", "SD", "N")),
    "SEN" = list(type = "stats", n_cols = 3, suffixes = c("Mean", "SEM", "N")),
    "CVN" = list(type = "stats", n_cols = 3, suffixes = c("Mean", "%CV", "N")),
    "SD" = list(type = "stats", n_cols = 2, suffixes = c("Mean", "SD")),
    "SE" = list(type = "stats", n_cols = 2, suffixes = c("Mean", "SEM")),
    "CV" = list(type = "stats", n_cols = 2, suffixes = c("Mean", "%CV")),
    "low-high" = list(type = "stats", n_cols = 3, suffixes = c("Mean", "+Error", "-Error")),
    "upper-lower-limits" = list(type = "stats", n_cols = 3, suffixes = c("Mean", "Upper Limit", "Lower Limit"))
  )
  
  # Process Y columns
  format_info <- if ("YFormat" %in% names(table_attrs)) format_types[[table_attrs[["YFormat"]]]] else NULL
  y_vals_all <- process_y_columns(y_cols_xml, format_info)
  
  # Process X column
  if (table_attrs[["XFormat"]] != "none") {
    x_cols_xml <- xml2::xml_find_all(x = pzfx_table, xpath = ".//XColumn")
    stopifnot(length(x_cols_xml) == 1)
    
    x_col_name <- xml2::xml_text(xml2::xml_find_all(x = x_cols_xml, xpath = ".//Title"))
    x_vals <- xml2::xml_text(xml2::xml_children(xml2::xml_find_all(x = x_cols_xml, xpath = ".//Subcolumn")))
    
    if (table_attrs[["XFormat"]] == "numbers") x_vals <- as.numeric(x_vals)
    y_vals_all <- tibble::add_column(y_vals_all, x = x_vals, .before = 1, .name_repair = "minimal")
    colnames(y_vals_all)[1] <- x_col_name
  } else {
    row_cols_xml <- xml2::xml_find_all(x = pzfx_table, xpath = ".//RowTitlesColumn")
    if (length(row_cols_xml) == 1) {
      row_names <- xml2::xml_text(xml2::xml_children(xml2::xml_find_all(x = row_cols_xml, xpath = ".//Subcolumn")))
      y_vals_all <- tibble::add_column(y_vals_all, rowname = row_names, .before = 1)
    }
  }
  
  suppressWarnings(readr::type_convert(y_vals_all))
}