read_pzfx_table <- function(pzfx_table) {
  table_attrs <- xml2::xml_attrs(pzfx_table)

  # QC table attributes
  x_column_exists <- "XFormat" %in% names(table_attrs)
  y_column_exists <- "YFormat" %in% names(table_attrs)

  # YColumn
  y_cols_xml <- xml2::xml_find_all(x = pzfx_table, xpath = ".//YColumn")
  n_y_cols <- length(y_cols_xml)
  y_col_names <- xml2::xml_text(xml2::xml_find_all(x = y_cols_xml, xpath = ".//Title"))

  # If the sheet type is a y_replicates type
  if(y_column_exists) {
    if(table_attrs[["YFormat"]] == "replicates") {
      # extract number of replicates
      n_rep <- as.numeric(table_attrs[["Replicates"]])
      # make new unique y_col_names
      if(n_rep > 1) y_col_names <- paste(rep(x = y_col_names, each = n_rep), seq_len(n_rep), sep = "_")
      y_vals_all <- lapply(X = y_cols_xml, FUN = function(xml_subcol) {
        xml2::xml_text(xml2::xml_children(xml2::xml_find_all(x = xml_subcol, xpath = ".//Subcolumn")))
      })
      y_vals_all <- lapply(y_vals_all, FUN = function(x) {matrix(data = x, ncol = n_rep)})
      y_vals_all <- tibble::as_tibble(as.data.frame(Reduce(f = cbind, x = y_vals_all)), .name_repair = "minimal")
      colnames(y_vals_all) <- y_col_names
    } else if(table_attrs[["YFormat"]] == "SDN") { # If the sheet type is a y_sd_n type
      # make new unique y_col_names
      y_col_names <- paste(rep(x = y_col_names, each = 3), c("Mean", "SD", "N"), sep = "_")
      y_vals_all <- lapply(X = y_cols_xml, FUN = function(xml_subcol) {
        xml2::xml_text(xml2::xml_children(xml2::xml_find_all(x = xml_subcol, xpath = ".//Subcolumn")))
      })
      y_vals_all <- lapply(y_vals_all, FUN = function(x) {matrix(data = x, ncol = 3)})
      y_vals_all <- tibble::as_tibble(as.data.frame(Reduce(f = cbind, x = y_vals_all)), .name_repair = "minimal")
      colnames(y_vals_all) <- y_col_names
    } else if(table_attrs[["YFormat"]] == "SEN") { # If the sheet type is a y_se_n type
      # make new unique y_col_names
      y_col_names <- paste(rep(x = y_col_names, each = 3), c("Mean", "SEM", "N"), sep = "_")
      y_vals_all <- lapply(X = y_cols_xml, FUN = function(xml_subcol) {
        xml2::xml_text(xml2::xml_children(xml2::xml_find_all(x = xml_subcol, xpath = ".//Subcolumn")))
      })
      y_vals_all <- lapply(y_vals_all, FUN = function(x) {matrix(data = x, ncol = 3)})
      y_vals_all <- tibble::as_tibble(as.data.frame(Reduce(f = cbind, x = y_vals_all)), .name_repair = "minimal")
      colnames(y_vals_all) <- y_col_names
    } else if(table_attrs[["YFormat"]] == "CVN") { # If the sheet type is a y_cv_n type
      # make new unique y_col_names
      y_col_names <- paste(rep(x = y_col_names, each = 3), c("Mean", "%CV", "N"), sep = "_")
      y_vals_all <- lapply(X = y_cols_xml, FUN = function(xml_subcol) {
        xml2::xml_text(xml2::xml_children(xml2::xml_find_all(x = xml_subcol, xpath = ".//Subcolumn")))
      })
      y_vals_all <- lapply(y_vals_all, FUN = function(x) {matrix(data = x, ncol = 3)})
      y_vals_all <- tibble::as_tibble(as.data.frame(Reduce(f = cbind, x = y_vals_all)), .name_repair = "minimal")
      colnames(y_vals_all) <- y_col_names
    } else if(table_attrs[["YFormat"]] == "SD") { # If the sheet type is a y_sd type
      # make new unique y_col_names
      y_col_names <- paste(rep(x = y_col_names, each = 2), c("Mean", "SD"), sep = "_")
      y_vals_all <- lapply(X = y_cols_xml, FUN = function(xml_subcol) {
        xml2::xml_text(xml2::xml_children(xml2::xml_find_all(x = xml_subcol, xpath = ".//Subcolumn")))
      })
      y_vals_all <- lapply(y_vals_all, FUN = function(x) {matrix(data = x, ncol = 2)})
      y_vals_all <- tibble::as_tibble(as.data.frame(Reduce(f = cbind, x = y_vals_all)), .name_repair = "minimal")
      colnames(y_vals_all) <- y_col_names
    } else if(table_attrs[["YFormat"]] == "SE") { # If the sheet type is a y_se type
      # make new unique y_col_names
      y_col_names <- paste(rep(x = y_col_names, each = 2), c("Mean", "SEM"), sep = "_")
      y_vals_all <- lapply(X = y_cols_xml, FUN = function(xml_subcol) {
        xml2::xml_text(xml2::xml_children(xml2::xml_find_all(x = xml_subcol, xpath = ".//Subcolumn")))
      })
      y_vals_all <- lapply(y_vals_all, FUN = function(x) {matrix(data = x, ncol = 2)})
      y_vals_all <- tibble::as_tibble(as.data.frame(Reduce(f = cbind, x = y_vals_all)), .name_repair = "minimal")
      colnames(y_vals_all) <- y_col_names
    } else if(table_attrs[["YFormat"]] == "CV") { # If the sheet type is a y_cv type
      # make new unique y_col_names
      y_col_names <- paste(rep(x = y_col_names, each = 2), c("Mean", "%CV"), sep = "_")
      y_vals_all <- lapply(X = y_cols_xml, FUN = function(xml_subcol) {
        xml2::xml_text(xml2::xml_children(xml2::xml_find_all(x = xml_subcol, xpath = ".//Subcolumn")))
      })
      y_vals_all <- lapply(y_vals_all, FUN = function(x) {matrix(data = x, ncol = 2)})
      y_vals_all <- tibble::as_tibble(as.data.frame(Reduce(f = cbind, x = y_vals_all)), .name_repair = "minimal")
      colnames(y_vals_all) <- y_col_names
    } else if(table_attrs[["YFormat"]] == "low-high") { # If the sheet type is a y_plus_minus type
      # make new unique y_col_names
      y_col_names <- paste(rep(x = y_col_names, each = 3), c("Mean", "+Error", "-Error"), sep = "_")
      y_vals_all <- lapply(X = y_cols_xml, FUN = function(xml_subcol) {
        xml2::xml_text(xml2::xml_children(xml2::xml_find_all(x = xml_subcol, xpath = ".//Subcolumn")))
      })
      y_vals_all <- lapply(y_vals_all, FUN = function(x) {matrix(data = x, ncol = 3)})
      y_vals_all <- tibble::as_tibble(as.data.frame(Reduce(f = cbind, x = y_vals_all)), .name_repair = "minimal")
      colnames(y_vals_all) <- y_col_names
    } else if(table_attrs[["YFormat"]] == "upper-lower-limits") { # If the sheet type is a y_high_low type
      # make new unique y_col_names
      y_col_names <- paste(rep(x = y_col_names, each = 3), c("Mean", "Upper Limit", "Lower Limit"), sep = "_")
      y_vals_all <- lapply(X = y_cols_xml, FUN = function(xml_subcol) {
        xml2::xml_text(xml2::xml_children(xml2::xml_find_all(x = xml_subcol, xpath = ".//Subcolumn")))
      })
      y_vals_all <- lapply(y_vals_all, FUN = function(x) {matrix(data = x, ncol = 3)})
      y_vals_all <- tibble::as_tibble(Reduce(f = cbind, x = y_vals_all))
      colnames(y_vals_all) <- y_col_names
    }
  } else if(!"YFormat" %in% names(table_attrs)) {
    y_vals_all <- lapply(X = y_cols_xml, FUN = function(xml_subcol) {
      xml2::xml_text(xml2::xml_children(xml2::xml_find_all(x = xml_subcol, xpath = ".//Subcolumn")))
    })
    n_rows <- max(unlist(lapply(y_vals_all, length)))
    for(i in seq_along(y_vals_all)) {
      length(y_vals_all[[i]]) <- n_rows
    }
    y_vals_all <- tibble::as_tibble(as.data.frame(Reduce(f = cbind, x = y_vals_all)), .name_repair = "minimal")
    colnames(y_vals_all) <- y_col_names
  }

  # XColumn
  if(table_attrs[["XFormat"]] != "none") {
    x_cols_xml <- xml2::xml_find_all(x = pzfx_table, xpath = ".//XColumn")
    stopifnot(length(x_cols_xml) == 1)
    x_col_name <- xml2::xml_text(xml2::xml_find_all(x = x_cols_xml, xpath = ".//Title"))
    x_vals <- xml2::xml_text(xml2::xml_children(xml2::xml_find_all(x = x_cols_xml, xpath = ".//Subcolumn")))
    if(table_attrs[["XFormat"]] == "numbers") x_vals <- as.numeric(x_vals)
    y_vals_all <- tibble::add_column(y_vals_all, x = x_vals, .before = 1, .name_repair = "minimal")
    colnames(y_vals_all) <- c(x_col_name, y_col_names)
  } else if(table_attrs[["XFormat"]] == "none") {
    row_cols_xml <- xml2::xml_find_all(x = pzfx_table, xpath = ".//RowTitlesColumn")
    if(length(row_cols_xml) == 1) {
      row_names <- xml2::xml_text(xml2::xml_children(xml2::xml_find_all(x = row_cols_xml, xpath = ".//Subcolumn")))
      y_vals_all <- tibble::add_column(y_vals_all, rowname = row_names, .before = 1)
    }
  }
  return(suppressWarnings(readr::type_convert(y_vals_all)))
}
