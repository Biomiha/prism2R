#' Read tables from a .prism archive
#' This is a utility function that parses the table contents from the new prism format ".prism".
#'
#' @param dot_prism_file string - the file path to the .prism file
#' @param selected_sheet a vector - either a length 1 character vector, a length 1 integer vector or NA
#' @importFrom archive archive
#' @importFrom archive archive_read
#' @importFrom jsonlite fromJSON
#' @importFrom readr read_csv
#' @noRd
read_prism_archive <- function(dot_prism_file, selected_sheet) {
  files_in_archive <- archive(dot_prism_file)
  files_in_archive$rowid <- seq_along(files_in_archive$path)
  file_number <- subset.data.frame(x = files_in_archive, subset = grepl(pattern = "document.json", x = path))[["rowid"]]
  top_level_json <- fromJSON(archive_read(archive = dot_prism_file, file = file_number))
  sheet_ids <- top_level_json$sheets$data
  sheet_names <- unlist(top_level_json$sheetAttributesMap[sheet_ids])
  read_prism_table <- function(selected_sheet_json, files_in_archive) {
    data_tables <- selected_sheet_json$table$uid
    data_table_folder <- subset.data.frame(x = files_in_archive, subset = grepl(pattern = data_tables, x = path))
    data_table_num <- subset.data.frame(x = data_table_folder, subset = grepl(pattern = ".csv", x = path))[["rowid"]]
    sheet_data <- read_csv(archive_read(archive = dot_prism_file, file = data_table_num), col_names = FALSE, show_col_types = FALSE)
    # Add dataset info
    sheet_title <- selected_sheet_json$title
    # y metadata
    y_data_ids <- unlist(lapply(selected_sheet_json$table$dataSets, FUN = function(x) subset(files_in_archive, subset = grepl(pattern = x, x = path))[["rowid"]]))
    y_datasets <- lapply(X = y_data_ids, FUN = function(x) fromJSON(archive_read(archive = dot_prism_file, file = x)))
    # If the sheet type is a y_replicates type
    if(selected_sheet_json$table$dataFormat == "y_replicates") {
      # extract number of replicates
      n_rep <- selected_sheet_json$table$replicatesCount
      # make new unique y_col_names
      y_col_names <- lapply(X = y_datasets, FUN = function(x) {
        fromJSON(archive_read(archive = dot_prism_file, file = subset.data.frame(x = files_in_archive, subset = grepl(pattern = x$uid, x = path))[["rowid"]]))$title
      }[[1]])
      # For edge cases where the column is NULL, make it an empty string
      y_col_names <- lapply(X = y_col_names, FUN = function(x) {
        if(is.null(x)) {
          x <- ""
          } else x
      })
      # If n replicates is more than 1 make unique names, otherwise keep as is
      if(n_rep > 1) y_col_names <- paste(rep(x = y_col_names, each = n_rep), seq_len(n_rep), sep = "_")
      colnames(sheet_data)[(ncol(sheet_data) - length(y_col_names) + 1):ncol(sheet_data)] <- y_col_names
    } else if(selected_sheet_json$table$dataFormat == "y_sd_n") {
      # If the sheet type is a y_sd_n type
      y_col_names <- lapply(X = y_datasets, FUN = function(x) {
        fromJSON(archive_read(archive = dot_prism_file, file = subset.data.frame(x = files_in_archive, subset = grepl(pattern = x$uid, x = path))[["rowid"]]))$title
      }[[1]])
      # For edge cases where the column is NULL, make it an empty string
      y_col_names <- lapply(X = y_col_names, FUN = function(x) {
        if(is.null(x)) {
          x <- ""
        } else x
      })
      y_col_names <- paste(rep(x = y_col_names, each = 3), c("Mean", "SD", "N"), sep = "_")
      colnames(sheet_data)[(ncol(sheet_data) - length(y_col_names) + 1):ncol(sheet_data)] <- y_col_names
    } else if(selected_sheet_json$table$dataFormat == "y_se_n") {
      # If the sheet type is a y_se_n type
      y_col_names <- lapply(X = y_datasets, FUN = function(x) {
        fromJSON(archive_read(archive = dot_prism_file, file = subset.data.frame(x = files_in_archive, subset = grepl(pattern = x$uid, x = path))[["rowid"]]))$title
      }[[1]])
      # For edge cases where the column is NULL, make it an empty string
      y_col_names <- lapply(X = y_col_names, FUN = function(x) {
        if(is.null(x)) {
          x <- ""
        } else x
      })
      y_col_names <- paste(rep(x = y_col_names, each = 3), c("Mean", "SEM", "N"), sep = "_")
      colnames(sheet_data)[(ncol(sheet_data) - length(y_col_names) + 1):ncol(sheet_data)] <- y_col_names
    } else if(selected_sheet_json$table$dataFormat == "y_cv_n") {
      # If the sheet type is a y_cv_n type
      y_col_names <- lapply(X = y_datasets, FUN = function(x) {
        fromJSON(archive_read(archive = dot_prism_file, file = subset.data.frame(x = files_in_archive, subset = grepl(pattern = x$uid, x = path))[["rowid"]]))$title
      }[[1]])
      # For edge cases where the column is NULL, make it an empty string
      y_col_names <- lapply(X = y_col_names, FUN = function(x) {
        if(is.null(x)) {
          x <- ""
        } else x
      })
      y_col_names <- paste(rep(x = y_col_names, each = 3), c("Mean", "%CV", "N"), sep = "_")
      colnames(sheet_data)[(ncol(sheet_data) - length(y_col_names) + 1):ncol(sheet_data)] <- y_col_names
      # Fix transformation in the data table
      col_idx <- which(endsWith(x = colnames(sheet_data), suffix = "%CV"))
      for(i in col_idx) {sheet_data[, i] <- sheet_data[, i] / sheet_data[, (i-1)] * 100}
    } else if(selected_sheet_json$table$dataFormat == "y_sd") {
      # If the sheet type is a y_sd type
      y_col_names <- lapply(X = y_datasets, FUN = function(x) {
        fromJSON(archive_read(archive = dot_prism_file, file = subset.data.frame(x = files_in_archive, subset = grepl(pattern = x$uid, x = path))[["rowid"]]))$title
      }[[1]])
      # For edge cases where the column is NULL, make it an empty string
      y_col_names <- lapply(X = y_col_names, FUN = function(x) {
        if(is.null(x)) {
          x <- ""
        } else x
      })
      y_col_names <- paste(rep(x = y_col_names, each = 2), c("Mean", "SD"), sep = "_")
      colnames(sheet_data)[(ncol(sheet_data) - length(y_col_names) + 1):ncol(sheet_data)] <- y_col_names
    } else if(selected_sheet_json$table$dataFormat == "y_se") {
      # If the sheet type is a y_se type
      y_col_names <- lapply(X = y_datasets, FUN = function(x) {
        fromJSON(archive_read(archive = dot_prism_file, file = subset.data.frame(x = files_in_archive, subset = grepl(pattern = x$uid, x = path))[["rowid"]]))$title
      }[[1]])
      # For edge cases where the column is NULL, make it an empty string
      y_col_names <- lapply(X = y_col_names, FUN = function(x) {
        if(is.null(x)) {
          x <- ""
        } else x
      })
      y_col_names <- paste(rep(x = y_col_names, each = 2), c("Mean", "SEM"), sep = "_")
      colnames(sheet_data)[(ncol(sheet_data) - length(y_col_names) + 1):ncol(sheet_data)] <- y_col_names
    } else if(selected_sheet_json$table$dataFormat == "y_cv") {
      # If the sheet type is a y_cv type
      y_col_names <- lapply(X = y_datasets, FUN = function(x) {
        fromJSON(archive_read(archive = dot_prism_file, file = subset.data.frame(x = files_in_archive, subset = grepl(pattern = x$uid, x = path))[["rowid"]]))$title
      }[[1]])
      # For edge cases where the column is NULL, make it an empty string
      y_col_names <- lapply(X = y_col_names, FUN = function(x) {
        if(is.null(x)) {
          x <- ""
        } else x
      })
      y_col_names <- paste(rep(x = y_col_names, each = 2), c("Mean", "%CV"), sep = "_")
      colnames(sheet_data)[(ncol(sheet_data) - length(y_col_names) + 1):ncol(sheet_data)] <- y_col_names
      # Fix transformation in the data table
      col_idx <- which(endsWith(x = colnames(sheet_data), suffix = "%CV"))
      for(i in col_idx) {sheet_data[, i] <- sheet_data[, i] / sheet_data[, (i-1)] * 100}
    } else if(selected_sheet_json$table$dataFormat == "y_plus_minus") {
      # If the sheet type is a y_plus_minus type
      y_col_names <- lapply(X = y_datasets, FUN = function(x) {
        fromJSON(archive_read(archive = dot_prism_file, file = subset.data.frame(x = files_in_archive, subset = grepl(pattern = x$uid, x = path))[["rowid"]]))$title
      }[[1]])
      # For edge cases where the column is NULL, make it an empty string
      y_col_names <- lapply(X = y_col_names, FUN = function(x) {
        if(is.null(x)) {
          x <- ""
        } else x
      })
      y_col_names <- paste(rep(x = y_col_names, each = 3), c("Mean", "+Error", "-Error"), sep = "_")
      colnames(sheet_data)[(ncol(sheet_data) - length(y_col_names) + 1):ncol(sheet_data)] <- y_col_names
    } else if(selected_sheet_json$table$dataFormat == "y_high_low") {
      # If the sheet type is a y_high_low type
      y_col_names <- lapply(X = y_datasets, FUN = function(x) {
        fromJSON(archive_read(archive = dot_prism_file, file = subset.data.frame(x = files_in_archive, subset = grepl(pattern = x$uid, x = path))[["rowid"]]))$title
      }[[1]])
      # For edge cases where the column is NULL, make it an empty string
      y_col_names <- lapply(X = y_col_names, FUN = function(x) {
        if(is.null(x)) {
          x <- ""
        } else x
      })
      y_col_names <- paste(rep(x = y_col_names, each = 3), c("Mean", "Upper Limit", "Lower Limit"), sep = "_")
      colnames(sheet_data)[(ncol(sheet_data) - length(y_col_names) + 1):ncol(sheet_data)] <- y_col_names
      # Fix transformation in the data table
      col_idx_upper <- which(endsWith(x = colnames(sheet_data), suffix = "Upper Limit"))
      for(i in col_idx_upper) {sheet_data[, i] <- sheet_data[, (i-1)] + sheet_data[, i]}
      col_idx_lower <- which(endsWith(x = colnames(sheet_data), suffix = "Lower Limit"))
      for(i in col_idx_lower) {sheet_data[, i] <- sheet_data[, (i-2)] - sheet_data[, i]}
    } else if(selected_sheet_json$table$dataFormat == "y_single") {
      y_col_names <- lapply(X = y_datasets, FUN = function(x) {
        fromJSON(archive_read(archive = dot_prism_file, file = subset.data.frame(x = files_in_archive, subset = grepl(pattern = x$uid, x = path))[["rowid"]]))$title
      }[[1]])
      # For edge cases where the column is NULL, make it an empty string
      y_col_names <- lapply(X = y_col_names, FUN = function(x) {
        if(is.null(x)) {
          x <- ""
        } else x
      })
      colnames(sheet_data)[(ncol(sheet_data) - length(y_col_names) + 1):ncol(sheet_data)] <- y_col_names
    }

    # x metadata
    if(!is.null(selected_sheet_json$table$xDataSet)) {
      x_dataset <- fromJSON(archive_read(archive = dot_prism_file, file = subset.data.frame(x = files_in_archive, subset = grepl(pattern = selected_sheet_json$table$xDataSet, x = path))[["rowid"]]))
      if("title" %in% names(x_dataset)) {
        x_col_name <- x_dataset[["title"]]
        # assign x colname if appropriate
        if(length(colnames(sheet_data)) - length(y_col_names) == 1) colnames(sheet_data)[[1]] <- x_col_name
        if(length(colnames(sheet_data)) - length(y_col_names) > 1) {
          colnames(sheet_data)[(ncol(sheet_data) - length(y_col_names)):ncol(sheet_data)] <- c(x_col_name, y_col_names)
        }
      }
    } else if(is.null(selected_sheet_json$table$xDataSet) & !is.null(selected_sheet_json$table$rowTitlesDataSet)) {
      # assign V1 as rownames if appropriate
      if(length(colnames(sheet_data)) - length(y_col_names) == 1) {
        colnames(sheet_data)[[1]] <- "rowname"
      }
    }
    return(sheet_data)
  }
  if(is.numeric(selected_sheet)) {
    selected_sheet_id <- sheet_ids[[selected_sheet]]
    selected_sheet_folder <- subset.data.frame(x = files_in_archive, subset = grepl(pattern = selected_sheet_id, x = path))
    selected_sheet_file <- subset.data.frame(x = selected_sheet_folder, subset = grepl(pattern = "sheet.json", x = path))
    if(nrow(selected_sheet_file) == 1) selected_sheet_num <- selected_sheet_file[["rowid"]]
    selected_sheet_json <- fromJSON(archive_read(archive = dot_prism_file, file = selected_sheet_num))
    suppressWarnings(readr::type_convert(read_prism_table(selected_sheet_json = selected_sheet_json, files_in_archive = files_in_archive)))
  } else if (is.character(selected_sheet)) {
    if(length(which(sheet_names %in% selected_sheet)) > 1) stop("You have chosen multiple sheets. Please choose either just one or `sheet = NA` for all.")
    selected_sheet_id <- sheet_ids[[which(sheet_names %in% selected_sheet)]]
    selected_sheet_folder <- subset.data.frame(x = files_in_archive, subset = grepl(pattern = selected_sheet_id, x = path))
    selected_sheet_file <- subset.data.frame(x = selected_sheet_folder, subset = grepl(pattern = "sheet.json", x = path))
    if(nrow(selected_sheet_file) == 1) selected_sheet_num <- selected_sheet_file[["rowid"]]
    selected_sheet_json <- fromJSON(archive_read(archive = dot_prism_file, file = selected_sheet_num))
    suppressWarnings(readr::type_convert(read_prism_table(selected_sheet_json = selected_sheet_json, files_in_archive = files_in_archive)))
  } else if(is.na(selected_sheet)) {
    # if no sheets are selected, the default is to read all tables into a list
    prism_tables_list <- vector("list", length = length(sheet_names))
    prism_tables_list <- lapply(sheet_ids, function(x) {
      selected_sheet_id <- x
      selected_sheet_folder <- subset.data.frame(x = files_in_archive, subset = grepl(pattern = selected_sheet_id, x = path))
      selected_sheet_file <- subset.data.frame(x = selected_sheet_folder, subset = grepl(pattern = "sheet.json", x = path))
      if(nrow(selected_sheet_file) == 1) selected_sheet_num <- selected_sheet_file[["rowid"]]
      selected_sheet_json <- fromJSON(archive_read(archive = dot_prism_file, file = selected_sheet_num))
      suppressWarnings(readr::type_convert(read_prism_table(selected_sheet_json = selected_sheet_json, files_in_archive = files_in_archive)))
    })
    names(prism_tables_list) <- get_prism_tables(dot_prism_file = dot_prism_file)
    return(prism_tables_list)
  }
}
