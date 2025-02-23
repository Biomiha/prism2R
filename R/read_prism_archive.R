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
  # Check if the file exists
  if (!file.exists(dot_prism_file)) {
    stop("The specified .prism file does not exist.")
  }

  # Safely attempt to list files in archive, handle potential errors
  tryCatch({
    files_in_archive <- archive(dot_prism_file)
  }, error = function(e) {
    stop(paste("Error reading archive:", e$message))
  })

  # Check if the archive is empty
  if (nrow(files_in_archive) == 0) {
    stop("The archive appears to be empty.")
  }

  files_in_archive$rowid <- seq_along(files_in_archive$path)

  # Find the document.json file, handle cases where it's missing
  document_json_files <- files_in_archive[grepl(pattern = "document.json", x = files_in_archive$path), ]
  if (nrow(document_json_files) == 0) {
    stop("document.json not found in archive.")
  } else if (nrow(document_json_files) > 1) {
    warning("Multiple document.json files found, using the first one.")
    file_number <- document_json_files$rowid[[1]]
  } else {
    file_number <- document_json_files$rowid
  }

  # Safely read and parse the top-level JSON, handle potential errors
  tryCatch({
    top_level_json <- fromJSON(archive_read(archive = dot_prism_file, file = file_number))
  }, error = function(e) {
    stop(paste("Error parsing document.json:", e$message))
  })

  # Extract sheet IDs and names, handle missing sheet data
  if (is.null(top_level_json$sheets$data) || length(top_level_json$sheets$data) == 0) {
    stop("No sheets found in document.json.")
  }
  sheet_ids <- top_level_json$sheets$data

  if (is.null(top_level_json$sheetAttributesMap) || length(top_level_json$sheetAttributesMap) == 0) {
    stop("No sheet attributes found in document.json.")
  }
  sheet_names <- unlist(top_level_json$sheetAttributesMap[sheet_ids])

  read_prism_table <- function(selected_sheet_json, files_in_archive) {
    # Extract data table UID, handle missing table data
    if (is.null(selected_sheet_json$table$uid)) {
      stop("Table UID is missing in sheet.json.")
    }
    data_tables <- selected_sheet_json$table$uid

    # Find the data table folder, handle cases where it's missing
    data_table_folder <- files_in_archive[grepl(pattern = data_tables, x = files_in_archive$path), ]
    if (nrow(data_table_folder) == 0) {
      stop(paste("Data table folder for UID", data_tables, "not found in archive."))
    }

    # Find the CSV file, handle cases where it's missing
    data_table_num_df <- data_table_folder[grepl(pattern = ".csv", x = data_table_folder$path), ]
    if (nrow(data_table_num_df) == 0) {
      stop(paste("CSV data table for UID", data_tables, "not found in archive."))
    } else if (nrow(data_table_num_df) > 1) {
      warning(paste("Multiple CSV data tables found for UID", data_tables, ", using the first one."))
      data_table_num <- data_table_num_df$rowid[[1]]
    } else {
      data_table_num <- data_table_num_df$rowid
    }

    # Safely read the CSV data, handle potential errors
    tryCatch({
      sheet_data <- read_csv(archive_read(archive = dot_prism_file, file = data_table_num), col_names = FALSE, show_col_types = FALSE)
    }, error = function(e) {
      stop(paste("Error reading CSV data table:", e$message))
    })

    # Add dataset info
    sheet_title <- selected_sheet_json$title

    # y metadata
    y_data_ids <- unlist(lapply(selected_sheet_json$table$dataSets, FUN = function(x) {
      subset_result <- files_in_archive[grepl(pattern = x, x = files_in_archive$path), ]
      if (nrow(subset_result) > 0) {
        return(subset_result$rowid)
      } else {
        warning(paste("Y dataset with pattern", x, "not found.  Skipping."))
        return(NULL) # or NA, depending on desired behavior
      }
    }))
    y_data_ids <- y_data_ids[!is.null(y_data_ids)] # Remove NULL values

    y_datasets <- lapply(X = y_data_ids, FUN = function(x) {
      tryCatch({
        fromJSON(archive_read(archive = dot_prism_file, file = x))
      }, error = function(e) {
        warning(paste("Error reading or parsing Y dataset file:", x, ". Skipping. Error:", e$message))
        return(NULL)
      })
    })
    y_datasets <- y_datasets[!sapply(y_datasets, is.null)] # Remove NULLS

    # Helper function to extract column names safely
    extract_y_col_name <- function(y_dataset) {
      if (is.null(y_dataset)) return("") # Handle NULL y_dataset

      subset_result <- files_in_archive[grepl(pattern = y_dataset$uid, x = files_in_archive$path), ]
      if (nrow(subset_result) == 0) {
        warning(paste("Y dataset file for UID", y_dataset$uid, "not found.  Returning empty string for column name."))
        return("")
      }

      tryCatch({
        json_data <- fromJSON(archive_read(archive = dot_prism_file, file = subset_result$rowid))
        title <- json_data$title
        if (is.null(title)) return("") else return(title) # Handle NULL title
      }, error = function(e) {
        warning(paste("Error reading or parsing Y dataset file:", subset_result$rowid, ". Returning empty string for column name. Error:", e$message))
        return("")
      })
    }

    # Handle different data formats
    if(selected_sheet_json$table$dataFormat == "y_replicates") {
      n_rep <- selected_sheet_json$table$replicatesCount
      y_col_names <- lapply(X = y_datasets, FUN = extract_y_col_name)
      if(n_rep > 1) y_col_names <- paste(rep(x = y_col_names, each = n_rep), seq_len(n_rep), sep = "_")
      colnames(sheet_data)[(ncol(sheet_data) - length(y_col_names) + 1):ncol(sheet_data)] <- y_col_names
    } else if(selected_sheet_json$table$dataFormat %in% c("y_sd_n", "y_se_n", "y_cv_n", "y_sd", "y_se", "y_cv", "y_plus_minus", "y_high_low")) {
      # Define suffixes based on data format
      suffixes <- switch(selected_sheet_json$table$dataFormat,
                         "y_sd_n" = c("Mean", "SD", "N"),
                         "y_se_n" = c("Mean", "SEM", "N"),
                         "y_cv_n" = c("Mean", "%CV", "N"),
                         "y_sd" = c("Mean", "SD"),
                         "y_se" = c("Mean", "SEM"),
                         "y_cv" = c("Mean", "%CV"),
                         "y_plus_minus" = c("Mean", "+Error", "-Error"),
                         "y_high_low" = c("Mean", "Upper Limit", "Lower Limit"))

      y_col_names <- lapply(X = y_datasets, FUN = extract_y_col_name)
      y_col_names <- paste(rep(x = y_col_names, each = length(suffixes)), suffixes, sep = "_")
      colnames(sheet_data)[(ncol(sheet_data) - length(y_col_names) + 1):ncol(sheet_data)] <- y_col_names

      # Fix transformation in the data table for %CV and high_low
      if (selected_sheet_json$table$dataFormat == "y_cv_n" || selected_sheet_json$table$dataFormat == "y_cv") {
        col_idx <- which(endsWith(x = colnames(sheet_data), suffix = "%CV"))
        for(i in col_idx) {
          sheet_data[, i] <- sheet_data[, i] / sheet_data[, (i-1)] * 100
        }
      } else if (selected_sheet_json$table$dataFormat == "y_high_low") {
        col_idx_upper <- which(endsWith(x = colnames(sheet_data), suffix = "Upper Limit"))
        for(i in col_idx_upper) {
          sheet_data[, i] <- sheet_data[, (i-1)] + sheet_data[, i]
        }
        col_idx_lower <- which(endsWith(x = colnames(sheet_data), suffix = "Lower Limit"))
        for(i in col_idx_lower) {
          sheet_data[, i] <- sheet_data[, (i-2)] - sheet_data[, i]
        }
      }
    } else if(selected_sheet_json$table$dataFormat == "y_single") {
      y_col_names <- lapply(X = y_datasets, FUN = extract_y_col_name)
      colnames(sheet_data)[(ncol(sheet_data) - length(y_col_names) + 1):ncol(sheet_data)] <- y_col_names
    }

    # x metadata
    if(!is.null(selected_sheet_json$table$xDataSet)) {
      # Find x dataset
      x_dataset_info <- files_in_archive[grepl(pattern = selected_sheet_json$table$xDataSet, x = files_in_archive$path), ]
      if (nrow(x_dataset_info) == 0) {
        warning(paste("X dataset with pattern", selected_sheet_json$table$xDataSet, "not found.  Skipping."))
        x_dataset <- NULL
      } else {
        tryCatch({
          x_dataset <- fromJSON(archive_read(archive = dot_prism_file, file = x_dataset_info$rowid))
        }, error = function(e) {
          warning(paste("Error reading or parsing X dataset file:", x_dataset_info$rowid, ". Skipping. Error:", e$message))
          x_dataset <- NULL
        })
      }

      if(!is.null(x_dataset) && "title" %in% names(x_dataset)) {
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

  # Handle different ways of selecting sheets
  if(is.numeric(selected_sheet)) {
    if (selected_sheet > length(sheet_ids) || selected_sheet < 1) {
      stop("Selected sheet number is out of range.")
    }
    selected_sheet_id <- sheet_ids[[selected_sheet]]
  } else if (is.character(selected_sheet)) {
    if (!(selected_sheet %in% sheet_names)) {
      stop("Selected sheet name not found.")
    }
    if(length(which(sheet_names %in% selected_sheet)) > 1) stop("You have chosen multiple sheets. Please choose either just one or `sheet = NA` for all.")
    selected_sheet_id <- sheet_ids[[which(sheet_names %in% selected_sheet)]]
  } else if(is.na(selected_sheet)) {
    # if no sheets are selected, the default is to read all tables into a list
    prism_tables_list <- vector("list", length = length(sheet_names))
    prism_tables_list <- lapply(sheet_ids, function(x) {
      selected_sheet_id <- x
      selected_sheet_folder <- files_in_archive[grepl(pattern = selected_sheet_id, x = files_in_archive$path), ]
      selected_sheet_file <- selected_sheet_folder[grepl(pattern = "sheet.json", x = selected_sheet_folder$path), ]
      if(nrow(selected_sheet_file) == 1) selected_sheet_num <- selected_sheet_file[["rowid"]]
      selected_sheet_json <- fromJSON(archive_read(archive = dot_prism_file, file = selected_sheet_num))
      suppressWarnings(readr::type_convert(read_prism_table(selected_sheet_json = selected_sheet_json, files_in_archive = files_in_archive)))
    })
    names(prism_tables_list) <- sheet_names
    return(prism_tables_list)
  } else {
    stop("Invalid selected_sheet type.  Must be numeric, character, or NA.")
  }

  # Read selected sheet
  selected_sheet_folder <- files_in_archive[grepl(pattern = selected_sheet_id, x = files_in_archive$path), ]
  selected_sheet_file <- selected_sheet_folder[grepl(pattern = "sheet.json", x = selected_sheet_folder$path), ]

  if(nrow(selected_sheet_file) == 0) {
    stop(paste("Sheet.json not found for sheet ID", selected_sheet_id))
  } else if (nrow(selected_sheet_file) > 1) {
    warning(paste("Multiple sheet.json files found for sheet ID", selected_sheet_id, ", using the first one."))
    selected_sheet_num <- selected_sheet_file$rowid[[1]]
  } else {
    selected_sheet_num <- selected_sheet_file[["rowid"]]
  }

  tryCatch({
    selected_sheet_json <- fromJSON(archive_read(archive = dot_prism_file, file = selected_sheet_num))
  }, error = function(e) {
    stop(paste("Error parsing sheet.json:", e$message))
  })

  suppressWarnings(readr::type_convert(read_prism_table(selected_sheet_json = selected_sheet_json, files_in_archive = files_in_archive)))
}
