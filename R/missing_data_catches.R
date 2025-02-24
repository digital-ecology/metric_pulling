#' cleans C1 water metric data 
#'
#' @param metric feas metric
#'
#' @return df, a cleaned version of the input dataset - to be used as an argument in actual metric pulling function
#' @export
#'
#' @examples \dontrun{checked_dataset<-clean_C1_dataset(metric)}  
clean_C1_dataset <- function(metric) {
    # Read the dataset
    df <- openxlsx::read.xlsx(metric, "C-1 On-Site WaterC' Baseline", colNames = FALSE, startRow = 10)
    df <- as.data.frame(df)
    # Remove the first column
    df <- df[,-39]
    df <- df[,-35:-36]
    df <- df[,-32]
    df <- df[,-31]
    df <- df[,-28]
    df <- df[,-26]
    df <- df[,-25]
    df <- df[,-21]
    df <- df[, -1]
    
    
    # Always remove the last row of the dataset (if there is more than one row)
    if (nrow(df) > 1) {
      df <- df[-nrow(df), , drop = FALSE]
    }
    
    # Identify rows that are completely empty
    na_rows <- apply(df, 1, function(row) all(is.na(row) | row == ""))
    
    # Detect large NA blocks (adjust threshold if needed)
    na_threshold <- 3  # Define what counts as "many NAs in a row"
    na_streaks <- rle(na_rows)
    
    # Find the first large NA block
    na_lengths <- cumsum(na_streaks$lengths)
    first_large_na <- which(na_streaks$values & na_streaks$lengths >= na_threshold)
    
    if (length(first_large_na) > 0) {
      cut_off_index <- na_lengths[first_large_na[1]]  # Find where to cut
      df <- df[1:cut_off_index, , drop = FALSE]  # Keep only data before the NA block
    }
    
    
    # Replace sequential runs within any column with NA
    for (col_idx in 2:ncol(df)) {
      col_values <- as.numeric(df[[col_idx]])
      if (any(!is.na(col_values))) {
        diffs <- diff(col_values)
        seq_indices <- which(diffs == 1)  # Find indices where values increase sequentially
        
        if (length(seq_indices) > 0) {
          df[[col_idx]][c(seq_indices, seq_indices + 1)] <- NA  # Replace sequential values with NA
        }
      }
    }
    
    
    # Remove the last row if it's a duplicate of the previous one
    if (nrow(df) > 1 && identical(df[nrow(df), ], df[nrow(df) - 1, ])) {
      df <- df[-nrow(df), , drop = FALSE]
    }
    df[df == "NA" | df == ""| df == " "] <- NA
    df <- df[rowSums(is.na(df)) != ncol(df), ]  # Remove rows where all values are NA
    
    # Check for the presence of "Check Data ⚠" in any dataset
    for (i in df) {
      if (any(is.na(i) | i == "")) {
        return("Please check metric is filled in appropriately before continuing")
      }
    }
    # Check if all columns have the same number of rows
    column_lengths <- sapply(df, function(col) sum(!is.na(col) & col != ""))
    if (length(unique(column_lengths)) > 1) {
      return("Please check metric is filled in appropriately before continuing")
    }
    
    
    
    if (nrow(df) == 0) {
      return(df)
    }
    
    return(df)
  }
  
#' cleans C2 water metric data 
#'
#' @param metric feas metric
#'
#' @return df, a cleaned version of the input dataset - to be used as an argument in actual metric pulling function
#' @export
#'
#' @examples \dontrun{checked_dataset<-clean_C2_dataset(metric)}    
clean_C2_dataset <- function(metric) {
    # Read the dataset
    df <- openxlsx::read.xlsx(metric, "C-2 On-Site WaterC' Creation", colNames = FALSE, startRow = 10)
    
    # Remove the first column
    df <- df[,-30]
    df <- df[,-26:-29]
    df <- df[,-13]
    df <- df[, -1]
    df <- df[-1:-2,]
    
    # Always remove the last row of the dataset (if there is more than one row)
    if (nrow(df) > 1) {
      df <- df[-nrow(df), , drop = FALSE]
    }
    
    # Identify rows that are completely empty
    na_rows <- apply(df, 1, function(row) all(is.na(row) | row == ""))
    
    # Detect large NA blocks (adjust threshold if needed)
    na_threshold <- 3  # Define what counts as "many NAs in a row"
    na_streaks <- rle(na_rows)
    
    # Find the first large NA block
    na_lengths <- cumsum(na_streaks$lengths)
    first_large_na <- which(na_streaks$values & na_streaks$lengths >= na_threshold)
    
    if (length(first_large_na) > 0) {
      cut_off_index <- na_lengths[first_large_na[1]]  # Find where to cut
      df <- df[1:cut_off_index, , drop = FALSE]  # Keep only data before the NA block
    }
    
    
    # Replace sequential runs within any column with NA
    for (col_idx in 2:ncol(df)) {
      col_values <- as.numeric(df[[col_idx]])
      if (any(!is.na(col_values))) {
        diffs <- diff(col_values)
        seq_indices <- which(diffs == 1)  # Find indices where values increase sequentially
        
        if (length(seq_indices) > 0) {
          df[[col_idx]][c(seq_indices, seq_indices + 1)] <- NA  # Replace sequential values with NA
        }
      }
    }
    
    
    # Remove the last row if it's a duplicate of the previous one
    if (nrow(df) > 1 && identical(df[nrow(df), ], df[nrow(df) - 1, ])) {
      df <- df[-nrow(df), , drop = FALSE]
    }
    df[df == "NA" | df == ""| df == " "] <- NA
    df <- df[rowSums(is.na(df)) != ncol(df), ]  # Remove rows where all values are NA
    
    # Check for the presence of "Check Data ⚠" in any dataset
    for (i in df) {
      if (any(is.na(i) | i == "")) {
        return("Please check metric is filled in appropriately before continuing")
      }
    }
    # Check if all columns have the same number of rows
    column_lengths <- sapply(df, function(col) sum(!is.na(col) & col != ""))
    if (length(unique(column_lengths)) > 1) {
      return("Please check metric is filled in appropriately before continuing")
    }
    if (nrow(df) == 0) {
      return(df)
    }
    
    return(df)
  }
  
  

  
#' cleans C3 water metric data 
#'
#' @param metric feas metric
#'
#' @return df, a cleaned version of the input dataset - to be used as an argument in actual metric pulling function
#' @export
#'
#' @examples \dontrun{checked_dataset<-clean_C3_dataset(metric)}    
clean_C3_dataset <- function(metric) {
    # Read the dataset
    df <- openxlsx::read.xlsx(metric, "C-3 On-Site WaterC' Enhancement", colNames = FALSE, startRow = 10)
    
    # Remove the first column
    df <- df[, -50]
    df <- df[, -39:-41]
    df <- df[, -26]
    df <- df[, -1]
    df <- df[-1:-2,]
    
   
    
    if (nrow(df) > 1) {
      df <- df[-nrow(df), , drop = FALSE]
    }
    
    # Identify rows that are completely empty
    na_rows <- apply(df, 1, function(row) all(is.na(row) | row == ""))
    
    # Detect large NA blocks (adjust threshold if needed)
    na_threshold <- 3  # Define what counts as "many NAs in a row"
    na_streaks <- rle(na_rows)
    
    # Find the first large NA block
    na_lengths <- cumsum(na_streaks$lengths)
    first_large_na <- which(na_streaks$values & na_streaks$lengths >= na_threshold)
    
    if (length(first_large_na) > 0) {
      cut_off_index <- na_lengths[first_large_na[1]]  # Find where to cut
      df <- df[1:cut_off_index, , drop = FALSE]  # Keep only data before the NA block
    }
    
    
    
    # Replace sequential runs within any column with NA
    for (col_idx in 2:ncol(df)) {
      col_values <- as.numeric(df[[col_idx]])
      if (any(!is.na(col_values))) {
        diffs <- diff(col_values)
        seq_indices <- which(diffs == 1)  # Find indices where values increase sequentially
        
        if (length(seq_indices) > 0) {
          df[[col_idx]][c(seq_indices, seq_indices + 1)] <- NA  # Replace sequential values with NA
        }
      }
    }
    
    # Keep only rows where there is data
    df <- df[apply(df, 1, function(row) any(row != "" & !is.na(row))), , drop = FALSE]
    
    # Remove the last row if it's a duplicate of the previous one
    if (nrow(df) > 1 && identical(df[nrow(df), ], df[nrow(df) - 1, ])) {
      df <- df[-nrow(df), , drop = FALSE]
    }
    
    # Remove rows with all NA values
    df[df == "NA" | df == ""| df == " "] <- NA
    df <- df[rowSums(is.na(df)) != ncol(df), ]  # Remove rows where all values are NA
    
    # Check for the presence of "Check Data ⚠" in any dataset
    for (i in df) {
      if (any(is.na(i) | i == "")) {
        return("Please check metric is filled in appropriately before continuing")
      }
    }
    # Check if all columns have the same number of rows
    column_lengths <- sapply(df, function(col) sum(!is.na(col) & col != ""))
    if (length(unique(column_lengths)) > 1) {
      return("Please check metric is filled in appropriately before continuing")
    }
    if (nrow(df) == 0) {
      return(df)
    }
    
    return(df)
  }

#' cleans habsum metric data 
#'
#' @param metric feas metric
#'
#' @return df, a cleaned version of the input dataset - to be used as an argument in actual metric pulling function
#' 
#' @export
#'
#' @examples \dontrun{checked_dataset<-clean_habsum_dataset(metric)}   
clean_habsum_dataset <- function(metric) {
    # Define column and row ranges for each dataset
    datasets_info <- list(
      Satisfied       = list(cols = 7, rows = 5:8),
      Distinctiveness = list(cols = 2, rows = 5:8),
      VHHab          = list(cols = 2, rows = 13:32),
      VHChange       = list(cols = 6, rows = 13:32),
      HHab           = list(cols = 2, rows = 41:82),
      HChange        = list(cols = 6, rows = 41:82),
      MHab           = list(cols = 2, rows = 89:115),
      MChange        = list(cols = 6, rows = 89:115),
      LHab           = list(cols = 2, rows = 125:162),
      LChange        = list(cols = 6, rows = 125:162)
    )
    
    # Read all datasets into a list
    datasets <- lapply(datasets_info, function(info) {
      openxlsx::read.xlsx(metric, sheet = "Trading Summary Area Habitats", 
                          cols = info$cols, rows = info$rows, 
                          colNames = FALSE, skipEmptyRows = TRUE)
    })
    
    # Check for missing values or empty strings in any dataset
    if (any(sapply(datasets, function(df) any(sapply(df, function(col) any(is.na(col) | col == "")))))) {
      return("Please check metric is filled in appropriately before continuing")
    }
    
    # Function to check if a dataset is completely empty
    is_dataset_empty <- function(df) {
      return(nrow(df) == 0 || all(sapply(df, function(col) all(is.na(col) | col == ""))))
    }
    
    # Check if all datasets are completely empty
    if (all(sapply(datasets, is_dataset_empty))) {
      return(data.frame())  # Return an empty dataframe if all datasets are empty
    }
    
    return(datasets$Satisfied)
  }
  
   
#' cleans hedgesum metric data 
#'
#' @param metric feas metric
#'
#' @return df, a cleaned version of the input dataset - to be used as an argument in actual metric pulling function
#' 
#' @export
#'
#' @examples \dontrun{checked_dataset<-clean_hedgesum_dataset(metric)}     
clean_hedgesum_dataset <- function(metric) {
    datasets_info <- list(
      Satisfied       = list(cols = 6, rows = 5:9),
      Distinctiveness = list(cols = 2, rows = 5:9),
      VHHab          = list(cols = 2, rows = 14),
      VHChange       = list(cols = 5, rows = 14),
      HHab           = list(cols = 2, rows = 22:24),
      HChange        = list(cols = 5, rows = 22:24),
      MHab           = list(cols = 2, rows = 32:36),
      MChange        = list(cols = 5, rows = 32:36),
      VLHab          = list(cols = 2, rows = 54),
      VLChange       = list(cols = 5, rows = 54)
    )
    
    datasets <- lapply(datasets_info, function(info) {
      openxlsx::read.xlsx(metric, sheet = "Trading Summary Hedgerows", 
                          cols = info$cols, rows = info$rows, 
                          colNames = FALSE, skipEmptyRows = TRUE)
    })
    
    if (any(sapply(datasets, function(df) any(sapply(df, function(col) any(is.na(col) | col == "")))))) {
      return("Please check metric is filled in appropriately before continuing")
    }
    
    if (all(sapply(datasets, function(df) nrow(df) == 0 || all(sapply(df, function(col) all(is.na(col) | col == "")))))) {
      return(data.frame())
    }
    
    return(datasets$Satisfied)
  }
  

#' cleans watersum metric data 
#'
#' @param metric feas metric
#'
#' @return df, a cleaned version of the input dataset - to be used as an argument in actual metric pulling function
#' 
#' @export
#'
#' @examples \dontrun{checked_dataset<-clean_watersum_dataset(metric)}
clean_watersum_dataset <- function(metric) {
    datasets_info <- list(
      Satisfied       = list(cols = 7, rows = 4:9),
      Distinctiveness = list(cols = 2, rows = 4:9),
      VHHab          = list(cols = 2, rows = 13),
      VHChange       = list(cols = 5, rows = 13),
      HHab           = list(cols = 2, rows = 22),
      HChange        = list(cols = 5, rows = 22),
      MHab           = list(cols = 2, rows = 30:31),
      MChange        = list(cols = 5, rows = 30:31),
      LHab           = list(cols = 2, rows = 42),
      LChange        = list(cols = 5, rows = 42)
    )
    
    datasets <- lapply(datasets_info, function(info) {
      openxlsx::read.xlsx(metric, sheet = "Trading Summary WaterC's", 
                          cols = info$cols, rows = info$rows, 
                          colNames = FALSE, skipEmptyRows = TRUE)
    })
    
    if (any(sapply(datasets, function(df) any(sapply(df, function(col) any(is.na(col) | col == "")))))) {
      return("Please check metric is filled in appropriately before continuing")
    }
    
    if (all(sapply(datasets, function(df) nrow(df) == 0 || all(sapply(df, function(col) all(is.na(col) | col == "")))))) {
      return(data.frame())
    }
    
    return(datasets$Satisfied)
  }
  
  
#' cleans onsitenet metric data 
#'
#' @param metric feas metric
#'
#' @return df, a cleaned version of the input dataset - to be used as an argument in actual metric pulling function
#' 
#' @export
#'
#' @examples \dontrun{checked_dataset<-clean_onsitenet_dataset(metric)}    
clean_onsitenet_dataset <- function(metric) {
    # Read the dataset
    NetHabUnits <- openxlsx::read.xlsx(metric, sheet = "Headline Results", cols = 8, rows = 47, colNames = FALSE, skipEmptyRows = TRUE)
    NetHedgeUnits<- openxlsx::read.xlsx(metric, sheet = "Headline Results", cols = 8, rows = 48, colNames = FALSE, skipEmptyRows = TRUE)
    NetWaterUnits<- openxlsx::read.xlsx(metric, sheet = "Headline Results", cols = 8, rows = 49, colNames = FALSE, skipEmptyRows = TRUE)
    NetHabPercent <- openxlsx::read.xlsx(metric, sheet = "Headline Results", cols = 8, rows = 51, colNames = FALSE, skipEmptyRows = TRUE)
    NetHedgePercent<- openxlsx::read.xlsx(metric, sheet = "Headline Results", cols = 8, rows = 52, colNames = FALSE, skipEmptyRows = TRUE)
    NetWaterPercent<- openxlsx::read.xlsx(metric, sheet = "Headline Results", cols = 8, rows = 53, colNames = FALSE, skipEmptyRows = TRUE)
    TradeSatisfied <- openxlsx::read.xlsx(metric, sheet = "Headline Results", cols = 6, rows = 55, colNames = FALSE, skipEmptyRows = TRUE)
    HabDeficit <- openxlsx::read.xlsx(metric, sheet = "Headline Results", cols = 8, rows = 61, colNames = FALSE, skipEmptyRows = TRUE)
    HedgeDeficit <- openxlsx::read.xlsx(metric, sheet = "Headline Results", cols = 8, rows = 62, colNames = FALSE, skipEmptyRows = TRUE)
    WaterDeficit <- openxlsx::read.xlsx(metric, sheet = "Headline Results", cols = 8, rows = 63, colNames = FALSE, skipEmptyRows = TRUE)
    
    datasets <- list(NetHabUnits, NetHedgeUnits, NetWaterUnits, NetHabPercent, NetHedgePercent, NetWaterPercent, 
                     TradeSatisfied, HabDeficit, HedgeDeficit, WaterDeficit)
    
    # Check for missing values or empty strings in each dataset
    for (data in datasets) {
      if (any(sapply(data, function(x) any(is.na(x) | x == ""  | x =="Check Data ⚠"| x == "Error ▲")))) {
        return("Please check metric is filled in appropriately before continuing")
      }
    }
    
    # Check if any dataset has zero rows
    if (any(sapply(datasets, nrow) == 0)) {
      return(NetHabUnits)
    }
    
    return(NetHabUnits)
  }
  
#' cleans onsitehab baseline metric data 
#'
#' @param metric feas metric
#'
#' @return df, a cleaned version of the input dataset - to be used as an argument in actual metric pulling function
#' 
#' @export
#'
#' @examples \dontrun{checked_dataset<-clean_onsitehab_baseline(metric)}   
clean_onsitehab_baseline <- function(metric) {
    # Read the dataset
    df <- openxlsx::read.xlsx(metric, "A-1 On-Site Habitat Baseline", colNames = FALSE, startRow = 10)
    
    # Remove the first column
    df <- df[,-40:-42]
    df <- df[, -33:-38]
    df <- df[,-31:-32]
    df <- df[,-28]
    df <- df[,-23:-25]
    df <- df[,-18]
    df <- df[,-17]
    df <- df[,-3]
    df <- df[-1,]
    
    # Replace sequential runs within any column with NA
    for (col_idx in 2:ncol(df)) {
      col_values <- as.numeric(df[[col_idx]])
      if (any(!is.na(col_values))) {
        diffs <- diff(col_values)
        seq_indices <- which(diffs == 1)  # Find indices where values increase sequentially
        
        if (length(seq_indices) > 0) {
          df[[col_idx]][c(seq_indices, seq_indices + 1)] <- NA  # Replace sequential values with NA
        }
      }
    }
    
    # Identify rows that are completely empty
    na_rows <- apply(df, 1, function(row) all(is.na(row) | row == ""))
    
    # Detect large NA blocks (adjust threshold if needed)
    na_threshold <- 3  # Define what counts as "many NAs in a row"
    na_streaks <- rle(na_rows)
    
    # Find the first large NA block
    na_lengths <- cumsum(na_streaks$lengths)
    first_large_na <- which(na_streaks$values & na_streaks$lengths >= na_threshold)
    
    if (length(first_large_na) > 0) {
      cut_off_index <- na_lengths[first_large_na[1]]  # Find where to cut
      df <- df[1:cut_off_index, , drop = FALSE]  # Keep only data before the NA block
    }
    
  
    # Keep only rows where there is data
    df <- df[apply(df, 1, function(row) any(row != "" & !is.na(row))), , drop = FALSE]
    
    
    # Remove the last row if it's a duplicate of the previous one
    if (nrow(df) > 1 && identical(df[nrow(df), ], df[nrow(df) - 1, ])) {
      df <- df[-nrow(df), , drop = FALSE]
    }
    
    # Check for the presence of "Check Data ⚠" in any dataset
    for (i in df) {
      if (any(is.na(i) | i == "")) {
        return("Please check metric is filled in appropriately before continuing")
      }
    }
    # Check if all columns have the same number of rows
    column_lengths <- sapply(df, function(col) sum(!is.na(col) & col != ""))
    if (length(unique(column_lengths)) > 1) {
      return("Please check metric is filled in appropriately before continuing")
    }
    if (nrow(df) == 0) {
      return(df)
    }
    
    return(df)
  }
  
#' cleans onsitehab retain metric data 
#'
#' @param metric feas metric
#'
#' @return df, a cleaned version of the input dataset - to be used as an argument in actual metric pulling function
#' 
#' @export
#'
#' @examples \dontrun{checked_dataset<-clean_onsitehab_retain(metric)}     
clean_onsitehab_retain <- function(metric) {
    # Read the dataset
    df <- openxlsx::read.xlsx(metric, "A-1 On-Site Habitat Baseline", colNames = FALSE, startRow = 10)
    
    # Remove the first column
    df <- df[,-40:-42]
    df <- df[, -33:-38]
    df <- df[,-31:-32]
    df <- df[,-28]
    df <- df[,-23:-25]
    df <- df[,-18]
    df <- df[,-17]
    df <- df[,-3]
    df <- df[-1,]
    
    # Replace sequential runs within any column with NA
    for (col_idx in 2:ncol(df)) {
      col_values <- as.numeric(df[[col_idx]])
      if (any(!is.na(col_values))) {
        diffs <- diff(col_values)
        seq_indices <- which(diffs == 1)  # Find indices where values increase sequentially
        
        if (length(seq_indices) > 0) {
          df[[col_idx]][c(seq_indices, seq_indices + 1)] <- NA  # Replace sequential values with NA
        }
      }
    }
    
    # Identify rows that are completely empty
    na_rows <- apply(df, 1, function(row) all(is.na(row) | row == ""))
    
    # Detect large NA blocks (adjust threshold if needed)
    na_threshold <- 3  # Define what counts as "many NAs in a row"
    na_streaks <- rle(na_rows)
    
    # Find the first large NA block
    na_lengths <- cumsum(na_streaks$lengths)
    first_large_na <- which(na_streaks$values & na_streaks$lengths >= na_threshold)
    
    if (length(first_large_na) > 0) {
      cut_off_index <- na_lengths[first_large_na[1]]  # Find where to cut
      df <- df[1:cut_off_index, , drop = FALSE]  # Keep only data before the NA block
    }
    
    # Keep only rows where there is data
    df <- df[apply(df, 1, function(row) any(row != "" & !is.na(row))), , drop = FALSE]
    
    # Remove the last row if it's a duplicate of the previous one
    if (nrow(df) > 1 && identical(df[nrow(df), ], df[nrow(df) - 1, ])) {
      df <- df[-nrow(df), , drop = FALSE]
    }
    
    # Check for the presence of "Check Data ⚠" in any dataset
    for (i in df) {
      if (any(is.na(i) | i == "")) {
        return("Please check metric is filled in appropriately before continuing")
      }
    }
    # Check if all columns have the same number of rows
    column_lengths <- sapply(df, function(col) sum(!is.na(col) & col != ""))
    if (length(unique(column_lengths)) > 1) {
      return("Please check metric is filled in appropriately before continuing")
    }
    if (nrow(df) == 0) {
      return(df)
    }
    
    return(df)
  }
  
#' cleans onsitehab loss metric data 
#'
#' @param metric feas metric
#'
#' @return df, a cleaned version of the input dataset - to be used as an argument in actual metric pulling function
#' 
#' @export
#'
#' @examples \dontrun{checked_dataset<-clean_onsitehab_loss(metric)}       
clean_onsitehab_loss <- function(metric) {
    # Read the dataset
    df <- openxlsx::read.xlsx(metric, "A-1 On-Site Habitat Baseline", colNames = FALSE, startRow = 10)
    
    # Remove the first column
    df <- df[,-40:-42]
    df <- df[, -33:-38]
    df <- df[,-31:-32]
    df <- df[,-28]
    df <- df[,-23:-25]
    df <- df[,-18]
    df <- df[,-17]
    df <- df[,-3]
    df <- df[-1,]
    
    # Replace sequential runs within any column with NA
    for (col_idx in 2:ncol(df)) {
      col_values <- as.numeric(df[[col_idx]])
      if (any(!is.na(col_values))) {
        diffs <- diff(col_values)
        seq_indices <- which(diffs == 1)  # Find indices where values increase sequentially
        
        if (length(seq_indices) > 0) {
          df[[col_idx]][c(seq_indices, seq_indices + 1)] <- NA  # Replace sequential values with NA
        }
      }
    }
    
    # Identify rows that are completely empty
    na_rows <- apply(df, 1, function(row) all(is.na(row) | row == ""))
    
    # Detect large NA blocks (adjust threshold if needed)
    na_threshold <- 3  # Define what counts as "many NAs in a row"
    na_streaks <- rle(na_rows)
    
    # Find the first large NA block
    na_lengths <- cumsum(na_streaks$lengths)
    first_large_na <- which(na_streaks$values & na_streaks$lengths >= na_threshold)
    
    if (length(first_large_na) > 0) {
      cut_off_index <- na_lengths[first_large_na[1]]  # Find where to cut
      df <- df[1:cut_off_index, , drop = FALSE]  # Keep only data before the NA block
    }
    
    
    # Keep only rows where there is data
    df <- df[apply(df, 1, function(row) any(row != "" & !is.na(row))), , drop = FALSE]
    
    # Remove the last row if it's a duplicate of the previous one
    if (nrow(df) > 1 && identical(df[nrow(df), ], df[nrow(df) - 1, ])) {
      df <- df[-nrow(df), , drop = FALSE]
    }
    
    # Check for the presence of "Check Data ⚠" in any dataset
    for (i in df) {
      if (any(is.na(i) | i == "")) {
        return("Please check metric is filled in appropriately before continuing")
      }
    }
    # Check if all columns have the same number of rows
    column_lengths <- sapply(df, function(col) sum(!is.na(col) & col != ""))
    if (length(unique(column_lengths)) > 1) {
      return("Please check metric is filled in appropriately before continuing")
    }
    if (nrow(df) == 0) {
      return(df)
    }
    
    return(df)
  }
  
#' cleans onsitehab creation metric data 
#'
#' @param metric feas metric
#'
#' @return df, a cleaned version of the input dataset - to be used as an argument in actual metric pulling function
#' 
#' @export
#'
#' @examples \dontrun{checked_dataset<-clean_onsitehab_creation(metric)}     
clean_onsitehab_creation <- function(metric) {
    # Read the dataset
    df <- openxlsx::read.xlsx(metric, "A-2 On-Site Habitat Creation", colNames = FALSE, startRow = 10)
    
    # Remove the first column
    df <- df[,-26:-29]
    df <- df[,-16:-17]
    df <- df[,-2]
    df <- df[-1,]
    
    # Replace sequential runs within any column with NA
    for (col_idx in 2:ncol(df)) {
      col_values <- as.numeric(df[[col_idx]])
      if (any(!is.na(col_values))) {
        diffs <- diff(col_values)
        seq_indices <- which(diffs == 1)  # Find indices where values increase sequentially
        
        if (length(seq_indices) > 0) {
          df[[col_idx]][c(seq_indices, seq_indices + 1)] <- NA  # Replace sequential values with NA
        }
      }
    }
    
    # Identify rows that are completely empty
    na_rows <- apply(df, 1, function(row) all(is.na(row) | row == ""))
    
    # Detect large NA blocks (adjust threshold if needed)
    na_threshold <- 3  # Define what counts as "many NAs in a row"
    na_streaks <- rle(na_rows)
    
    # Find the first large NA block
    na_lengths <- cumsum(na_streaks$lengths)
    first_large_na <- which(na_streaks$values & na_streaks$lengths >= na_threshold)
    
    if (length(first_large_na) > 0) {
      cut_off_index <- na_lengths[first_large_na[1]]  # Find where to cut
      df <- df[1:cut_off_index, , drop = FALSE]  # Keep only data before the NA block
    }
    
    
    # Keep only rows where there is data
    df <- df[apply(df, 1, function(row) any(row != "" & !is.na(row))), , drop = FALSE]
    
    # Remove the last row if it's a duplicate of the previous one
    if (nrow(df) > 1 && identical(df[nrow(df), ], df[nrow(df) - 1, ])) {
      df <- df[-nrow(df), , drop = FALSE]
    }
    
    # Check for the presence of "Check Data ⚠" in any dataset
    for (i in df) {
      if (any(is.na(i) | i == "")) {
        return("Please check metric is filled in appropriately before continuing")
      }
    }
    # Check if all columns have the same number of rows
    column_lengths <- sapply(df, function(col) sum(!is.na(col) & col != ""))
    if (length(unique(column_lengths)) > 1) {
      return("Please check metric is filled in appropriately before continuing")
    }
    if (nrow(df) == 0) {
      return(df)
    }
    
    return(df)
  }

#' cleans onsitehab enchancement metric data 
#'
#' @param metric feas metric
#'
#' @return df, a cleaned version of the input dataset - to be used as an argument in actual metric pulling function
#' 
#' @export
#'
#' @examples \dontrun{checked_dataset<-clean_onsitehab_enhancement(metric)}   
clean_onsitehab_enhancement <- function(metric) {
    # Read the dataset
    df <- openxlsx::read.xlsx(metric, "A-3 On-Site Habitat Enhancement", colNames = FALSE, startRow = 10)
    
    # Remove the first column
    df <- df[,-40:-44]
    df <- df[,-30:-31]
    #df <- df[,-2]
    df <- df[-1:-2,]
    
    # Replace sequential runs within any column with NA
    for (col_idx in 2:ncol(df)) {
      col_values <- as.numeric(df[[col_idx]])
      if (any(!is.na(col_values))) {
        diffs <- diff(col_values)
        seq_indices <- which(diffs == 1)  # Find indices where values increase sequentially
        
        if (length(seq_indices) > 0) {
          df[[col_idx]][c(seq_indices, seq_indices + 1)] <- NA  # Replace sequential values with NA
        }
      }
    }
    
    # Identify rows that are completely empty
    na_rows <- apply(df, 1, function(row) all(is.na(row) | row == ""))
    
    # Detect large NA blocks (adjust threshold if needed)
    na_threshold <- 3  # Define what counts as "many NAs in a row"
    na_streaks <- rle(na_rows)
    
    # Find the first large NA block
    na_lengths <- cumsum(na_streaks$lengths)
    first_large_na <- which(na_streaks$values & na_streaks$lengths >= na_threshold)
    
    if (length(first_large_na) > 0) {
      cut_off_index <- na_lengths[first_large_na[1]]  # Find where to cut
      df <- df[1:cut_off_index, , drop = FALSE]  # Keep only data before the NA block
    }
    
    
    # Keep only rows where there is data
    df <- df[apply(df, 1, function(row) any(row != "" & !is.na(row))), , drop = FALSE]
    
    # Remove the last row if it's a duplicate of the previous one
    if (nrow(df) > 1 && identical(df[nrow(df), ], df[nrow(df) - 1, ])) {
      df <- df[-nrow(df), , drop = FALSE]
    }
    
    # Check for the presence of "Check Data ⚠" in any dataset
    for (i in df) {
      if (any(is.na(i) | i == "")) {
        return("Please check metric is filled in appropriately before continuing")
      }
    }
    # Check if all columns have the same number of rows
    column_lengths <- sapply(df, function(col) sum(!is.na(col) & col != ""))
    if (length(unique(column_lengths)) > 1) {
      return("Please check metric is filled in appropriately before continuing")
    }
    if (nrow(df) == 0) {
      return(df)
    }
    
    return(df)
  }
  
#' cleans onsitehedge baseline metric data 
#'
#' @param metric feas metric
#'
#' @return df, a cleaned version of the input dataset - to be used as an argument in actual metric pulling function
#' 
#' @export
#'
#' @examples \dontrun{checked_dataset<-clean_onsitehedge_baseline(metric)}     
clean_onsitehedge_baseline <- function(metric) {
    # Read the dataset
    df <- openxlsx::read.xlsx(metric, "B-1 On-Site Hedge Baseline", colNames = FALSE, startRow = 10)
    
    # Remove the first column
    df <- df[,-22:-24]
    df <- df[,-18:-19]
    df <- df[,-16]
    df <- df[,-14]
    df <- df[,-1]
    df <- df[-1:-2,]
    
    # Replace sequential runs within any column with NA
    for (col_idx in 2:ncol(df)) {
      col_values <- as.numeric(df[[col_idx]])
      if (any(!is.na(col_values))) {
        diffs <- diff(col_values)
        seq_indices <- which(diffs == 1)  # Find indices where values increase sequentially
        
        if (length(seq_indices) > 0) {
          df[[col_idx]][c(seq_indices, seq_indices + 1)] <- NA  # Replace sequential values with NA
        }
      }
    }
    
    # Identify rows that are completely empty
    na_rows <- apply(df, 1, function(row) all(is.na(row) | row == ""))
    
    # Detect large NA blocks (adjust threshold if needed)
    na_threshold <- 3  # Define what counts as "many NAs in a row"
    na_streaks <- rle(na_rows)
    
    # Find the first large NA block
    na_lengths <- cumsum(na_streaks$lengths)
    first_large_na <- which(na_streaks$values & na_streaks$lengths >= na_threshold)
    
    if (length(first_large_na) > 0) {
      cut_off_index <- na_lengths[first_large_na[1]]  # Find where to cut
      df <- df[1:cut_off_index, , drop = FALSE]  # Keep only data before the NA block
    }
    
    
    
    # Keep only rows where there is data
    df <- df[apply(df, 1, function(row) any(row != "" & !is.na(row))), , drop = FALSE]
    
    # Remove the last row if it's a duplicate of the previous one
    if (nrow(df) > 1 && identical(df[nrow(df), ], df[nrow(df) - 1, ])) {
      df <- df[-nrow(df), , drop = FALSE]
    }
    
    # Check for the presence of "Check Data ⚠" in any dataset
    for (i in df) {
      if (any(is.na(i) | i == "")) {
        return("Please check metric is filled in appropriately before continuing")
      }
    }
    # Check if all columns have the same number of rows
    column_lengths <- sapply(df, function(col) sum(!is.na(col) & col != ""))
    if (length(unique(column_lengths)) > 1) {
      return("Please check metric is filled in appropriately before continuing")
    }
    if (nrow(df) == 0) {
      return(df)
    }
    
    return(df)
  }
  
#' cleans onsitehedge retain metric data 
#'
#' @param metric feas metric
#'
#' @return df, a cleaned version of the input dataset - to be used as an argument in actual metric pulling function
#' 
#' @export
#'
#' @examples \dontrun{checked_dataset<-clean_onsitehedge_retain(metric)}     
clean_onsitehedge_retain <- function(metric) {
    # Read the dataset
    df <- openxlsx::read.xlsx(metric, "B-1 On-Site Hedge Baseline", colNames = FALSE, startRow = 10)
    
    # Remove the first column
    df <- df[,-22:-24]
    df <- df[,-18:-19]
    df <- df[,-16]
    df <- df[,-14]
    df <- df[,-1]
    df <- df[-1:-2,]
    
    # Replace sequential runs within any column with NA
    for (col_idx in 2:ncol(df)) {
      col_values <- as.numeric(df[[col_idx]])
      if (any(!is.na(col_values))) {
        diffs <- diff(col_values)
        seq_indices <- which(diffs == 1)  # Find indices where values increase sequentially
        
        if (length(seq_indices) > 0) {
          df[[col_idx]][c(seq_indices, seq_indices + 1)] <- NA  # Replace sequential values with NA
        }
      }
    }
    
    # Identify rows that are completely empty
    na_rows <- apply(df, 1, function(row) all(is.na(row) | row == ""))
    
    # Detect large NA blocks (adjust threshold if needed)
    na_threshold <- 3  # Define what counts as "many NAs in a row"
    na_streaks <- rle(na_rows)
    
    # Find the first large NA block
    na_lengths <- cumsum(na_streaks$lengths)
    first_large_na <- which(na_streaks$values & na_streaks$lengths >= na_threshold)
    
    if (length(first_large_na) > 0) {
      cut_off_index <- na_lengths[first_large_na[1]]  # Find where to cut
      df <- df[1:cut_off_index, , drop = FALSE]  # Keep only data before the NA block
    }
    
    
    
    
    # Keep only rows where there is data
    df <- df[apply(df, 1, function(row) any(row != "" & !is.na(row))), , drop = FALSE]
    
    # Remove the last row if it's a duplicate of the previous one
    if (nrow(df) > 1 && identical(df[nrow(df), ], df[nrow(df) - 1, ])) {
      df <- df[-nrow(df), , drop = FALSE]
    }
    
    # Check for the presence of "Check Data ⚠" in any dataset
    for (i in df) {
      if (any(is.na(i) | i == "")) {
        return("Please check metric is filled in appropriately before continuing")
      }
    }
    # Check if all columns have the same number of rows
    column_lengths <- sapply(df, function(col) sum(!is.na(col) & col != ""))
    if (length(unique(column_lengths)) > 1) {
      return("Please check metric is filled in appropriately before continuing")
    }
    if (nrow(df) == 0) {
      return(df)
    }
    
    return(df)
  }

#' cleans onsitehedge retain loss data 
#'
#' @param metric feas metric
#'
#' @return df, a cleaned version of the input dataset - to be used as an argument in actual metric pulling function
#' 
#' @export
#'
#' @examples \dontrun{checked_dataset<-clean_onsitehedge_loss(metric)}    
clean_onsitehedge_loss <- function(metric) {
    # Read the dataset
    df <- openxlsx::read.xlsx(metric, "B-1 On-Site Hedge Baseline", colNames = FALSE, startRow = 10)
    
    # Remove the first column
    df <- df[,-22:-24]
    df <- df[,-18:-19]
    df <- df[,-16]
    df <- df[,-14]
    df <- df[,-1]
    df <- df[-1:-2,]
    
    # Replace sequential runs within any column with NA
    for (col_idx in 2:ncol(df)) {
      col_values <- as.numeric(df[[col_idx]])
      if (any(!is.na(col_values))) {
        diffs <- diff(col_values)
        seq_indices <- which(diffs == 1)  # Find indices where values increase sequentially
        
        if (length(seq_indices) > 0) {
          df[[col_idx]][c(seq_indices, seq_indices + 1)] <- NA  # Replace sequential values with NA
        }
      }
    }
    
    # Identify rows that are completely empty
    na_rows <- apply(df, 1, function(row) all(is.na(row) | row == ""))
    
    # Detect large NA blocks (adjust threshold if needed)
    na_threshold <- 3  # Define what counts as "many NAs in a row"
    na_streaks <- rle(na_rows)
    
    # Find the first large NA block
    na_lengths <- cumsum(na_streaks$lengths)
    first_large_na <- which(na_streaks$values & na_streaks$lengths >= na_threshold)
    
    if (length(first_large_na) > 0) {
      cut_off_index <- na_lengths[first_large_na[1]]  # Find where to cut
      df <- df[1:cut_off_index, , drop = FALSE]  # Keep only data before the NA block
    }
    
    
    
    
    # Keep only rows where there is data
    df <- df[apply(df, 1, function(row) any(row != "" & !is.na(row))), , drop = FALSE]
    
    # Remove the last row if it's a duplicate of the previous one
    if (nrow(df) > 1 && identical(df[nrow(df), ], df[nrow(df) - 1, ])) {
      df <- df[-nrow(df), , drop = FALSE]
    }
    
    # Check for the presence of "Check Data ⚠" in any dataset
    for (i in df) {
      if (any(is.na(i) | i == "")) {
        return("Please check metric is filled in appropriately before continuing")
      }
    }
    # Check if all columns have the same number of rows
    column_lengths <- sapply(df, function(col) sum(!is.na(col) & col != ""))
    if (length(unique(column_lengths)) > 1) {
      return("Please check metric is filled in appropriately before continuing")
    }
    if (nrow(df) == 0) {
      return(df)
    }
    
    return(df)
}

#' cleans onsitehedge creation loss data 
#'
#' @param metric feas metric
#'
#' @return df, a cleaned version of the input dataset - to be used as an argument in actual metric pulling function
#' 
#' @export
#'
#' @examples \dontrun{checked_dataset<-clean_onsitehedge_creation(metric)}    
clean_onsitehedge_creation <- function(metric) {
    # Read the dataset
    df <- openxlsx::read.xlsx(metric, "B-2 On-Site Hedge Creation", colNames = FALSE, startRow = 10)
    
    # Remove the first column
    df <- df[,-23:-26]
    df <- df[,-14]
    df <- df[,-1]
    df <- df[-1:-2,]
    
    # Replace sequential runs within any column with NA
    for (col_idx in 2:ncol(df)) {
      col_values <- as.numeric(df[[col_idx]])
      if (any(!is.na(col_values))) {
        diffs <- diff(col_values)
        seq_indices <- which(diffs == 1)  # Find indices where values increase sequentially
        
        if (length(seq_indices) > 0) {
          df[[col_idx]][c(seq_indices, seq_indices + 1)] <- NA  # Replace sequential values with NA
        }
      }
    }
    
    # Identify rows that are completely empty
    na_rows <- apply(df, 1, function(row) all(is.na(row) | row == ""))
    
    # Detect large NA blocks (adjust threshold if needed)
    na_threshold <- 3  # Define what counts as "many NAs in a row"
    na_streaks <- rle(na_rows)
    
    # Find the first large NA block
    na_lengths <- cumsum(na_streaks$lengths)
    first_large_na <- which(na_streaks$values & na_streaks$lengths >= na_threshold)
    
    if (length(first_large_na) > 0) {
      cut_off_index <- na_lengths[first_large_na[1]]  # Find where to cut
      df <- df[1:cut_off_index, , drop = FALSE]  # Keep only data before the NA block
    }
    
    
    # Keep only rows where there is data
    df <- df[apply(df, 1, function(row) any(row != "" & !is.na(row))), , drop = FALSE]
    
    # Remove the last row if it's a duplicate of the previous one
    if (nrow(df) > 1 && identical(df[nrow(df), ], df[nrow(df) - 1, ])) {
      df <- df[-nrow(df), , drop = FALSE]
    }
    
    # Check for the presence of "Check Data ⚠" in any dataset
    for (i in df) {
      if (any(is.na(i) | i == "")) {
        return("Please check metric is filled in appropriately before continuing")
      }
    }
    
    # Check if all columns have the same number of rows
    column_lengths <- sapply(df, function(col) sum(!is.na(col) & col != ""))
    if (length(unique(column_lengths)) > 1) {
      return("Please check metric is filled in appropriately before continuing")
    }
    
    if (nrow(df) == 0) {
      return(df)
    }
    
    return(df)
  }

#' cleans onsitehedge enhancement loss data 
#'
#' @param metric feas metric
#'
#' @return df, a cleaned version of the input dataset - to be used as an argument in actual metric pulling function
#' 
#' @export
#'
#' @examples \dontrun{checked_dataset<-clean_onsitehedge_enhancement(metric)}   
clean_onsitehedge_enhancement <- function(metric) {
    # Read the dataset
    df <- openxlsx::read.xlsx(metric, "B-3 On-Site Hedge Enhancement", colNames = FALSE, startRow = 10)
    
    # Remove the first column
    df <- df[,-34:-37]
    df <- df[,-24:-25]
    #df <- df[,-1]
    df <- df[-1:-2,]
    
    
    # Replace sequential runs within any column with NA
    for (col_idx in 2:ncol(df)) {
      col_values <- as.numeric(df[[col_idx]])
      if (any(!is.na(col_values))) {
        diffs <- diff(col_values)
        seq_indices <- which(diffs == 1)  # Find indices where values increase sequentially
        
        if (length(seq_indices) > 0) {
          df[[col_idx]][c(seq_indices, seq_indices + 1)] <- NA  # Replace sequential values with NA
        }
      }
    }
    # Identify rows that are completely empty
    na_rows <- apply(df, 1, function(row) all(is.na(row) | row == ""))
    
    # Detect large NA blocks (adjust threshold if needed)
    na_threshold <- 3  # Define what counts as "many NAs in a row"
    na_streaks <- rle(na_rows)
    
    # Find the first large NA block
    na_lengths <- cumsum(na_streaks$lengths)
    first_large_na <- which(na_streaks$values & na_streaks$lengths >= na_threshold)
    
    if (length(first_large_na) > 0) {
      cut_off_index <- na_lengths[first_large_na[1]]  # Find where to cut
      df <- df[1:cut_off_index, , drop = FALSE]  # Keep only data before the NA block
    }
    
    
    
    # Keep only rows where there is data
    df <- df[apply(df, 1, function(row) any(row != "" & !is.na(row))), , drop = FALSE]
    
    # Remove the last row if it's a duplicate of the previous one
    if (nrow(df) > 1 && identical(df[nrow(df), ], df[nrow(df) - 1, ])) {
      df <- df[-nrow(df), , drop = FALSE]
    }
    
    # Check for the presence of "Check Data ⚠" in any dataset
    for (i in df) {
      if (any(is.na(i) | i == "")) {
        return("Please check metric is filled in appropriately before continuing")
      }
    }
    # Check if all columns have the same number of rows
    column_lengths <- sapply(df, function(col) sum(!is.na(col) & col != ""))
    if (length(unique(column_lengths)) > 1) {
      return("Please check metric is filled in appropriately before continuing")
    }
    
    if (nrow(df) == 0) {
      return(df)
    }
    
    return(df)
}