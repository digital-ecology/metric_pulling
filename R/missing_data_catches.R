  clean_C1_dataset <- function(metric) {
    # Read the dataset
    df <- openxlsx::read.xlsx(metric, "C-1 On-Site WaterC' Baseline", colNames = FALSE, startRow = 10)
    
    # Remove the first column
    df <- df[, -1]
    
    # Keep only rows where column 1 has data
    df <- df[df[[1]] != "" & !is.na(df[[1]]), , drop = FALSE]
    
    # Keep only columns 1-23, 26-33, and 36-37
    selected_columns <- c(1:23, 26:33, 36:37)
    df <- df[, selected_columns, drop = FALSE]
    
    # Remove the last row if it's a duplicate of the previous one
    if (nrow(df) > 1 && identical(df[nrow(df), ], df[nrow(df) - 1, ])) {
      df <- df[-nrow(df), , drop = FALSE]
    }
    
    # Remove rows with all NA values
    df <- df[rowSums(is.na(df)) < ncol(df), ]
    
    # Check for any NA values in the dataset
    if (any(is.na(df))) {
      return("Please check metric is filled in appropriately before continuing")
    }
    
    return(df)
  }
  
  
  clean_C2_dataset <- function(metric) {
    # Read the dataset
    df <- openxlsx::read.xlsx(metric, "C-2 On-Site WaterC' Creation", colNames = FALSE, startRow = 10)
    
    # Remove the first column
    df <- df[, -1]
    df <- df[-1:-2,]
    
    # Keep only rows where column 1 has data
    df <- df[df[[1]] != "" & !is.na(df[[1]]), , drop = FALSE]
    
    # Keep only columns 1-23, 26-33, and 36-37
    selected_columns <- c(1:9, 20:22, 24)
    df <- df[, selected_columns, drop = FALSE]
    
    # Remove the last row if it's a duplicate of the previous one
    if (nrow(df) > 1 && identical(df[nrow(df), ], df[nrow(df) - 1, ])) {
      df <- df[-nrow(df), , drop = FALSE]
    }
    
    # Remove rows with all NA values
    df <- df[rowSums(is.na(df)) < ncol(df), ]
    
    # Check for any NA values in the dataset
    if (any(is.na(df))) {
      return("Please check metric is filled in appropriately before continuing")
    }
    
    return(df)
  }
  
  
  
  clean_C3_dataset <- function(metric) {
    # Read the dataset
    df <- openxlsx::read.xlsx(metric, "C-3 On-Site WaterC' Enhancement", colNames = FALSE, startRow = 10)
    
    # Remove the first column
    df <- df[, -1]
    df <- df[-1:-2,]
    
    # Keep only rows where column 1 has data
    df <- df[df[[1]] != "" & !is.na(df[[1]]), , drop = FALSE]
    
    # Keep only columns 1-23, 26-33, and 36-37
    selected_columns <- c(1:13, 30:37, 41:49)
    df <- df[, selected_columns, drop = FALSE]
    
    # Remove the last row if it's a duplicate of the previous one
    if (nrow(df) > 1 && identical(df[nrow(df), ], df[nrow(df) - 1, ])) {
      df <- df[-nrow(df), , drop = FALSE]
    }
    
    # Remove rows with all NA values
    df <- df[rowSums(is.na(df)) < ncol(df), ]
    
    # Check for any NA values in the dataset
    if (any(is.na(df))) {
      return("Please check metric is filled in appropriately before continuing")
    }
    
    return(df)
  }
