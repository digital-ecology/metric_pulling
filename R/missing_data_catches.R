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

  
  clean_habsum_dataset <- function(metric) {
    # Read the dataset
    Satisfied <- openxlsx::read.xlsx(metric, sheet = "Trading Summary Area Habitats", cols = 7, rows = 5:8, colNames = FALSE, skipEmptyRows = TRUE)
    Distinctiveness <- openxlsx::read.xlsx(metric, sheet = "Trading Summary Area Habitats", cols = 2, rows = 5:8, colNames = FALSE, skipEmptyRows = TRUE)
    VHHab <- openxlsx::read.xlsx(metric, sheet = "Trading Summary Area Habitats", cols = 2, rows = 13:32, colNames = FALSE, skipEmptyRows = TRUE)
    VHChange <- openxlsx::read.xlsx(metric, sheet = "Trading Summary Area Habitats", cols = 6, rows = 13:32, colNames = FALSE, skipEmptyRows = TRUE)
    HHab <- openxlsx::read.xlsx(metric, sheet = "Trading Summary Area Habitats", cols = 2, rows = 41:82, colNames = FALSE, skipEmptyRows = TRUE)
    HChange <- openxlsx::read.xlsx(metric, sheet = "Trading Summary Area Habitats", cols = 6, rows = 41:82, colNames = FALSE, skipEmptyRows = TRUE)
    MHab <- openxlsx::read.xlsx(metric, sheet = "Trading Summary Area Habitats", cols = 2, rows = 89:115, colNames = FALSE, skipEmptyRows = TRUE)
    MChange <- openxlsx::read.xlsx(metric, sheet = "Trading Summary Area Habitats", cols = 6, rows = 89:115, colNames = FALSE, skipEmptyRows = TRUE)
    LHab <- openxlsx::read.xlsx(metric, sheet = "Trading Summary Area Habitats", cols = 2, rows = 125:162, colNames = FALSE, skipEmptyRows = TRUE)
    LChange <- openxlsx::read.xlsx(metric, sheet = "Trading Summary Area Habitats", cols = 6, rows = 125:162, colNames = FALSE, skipEmptyRows = TRUE)
    
    datasets <- list(Satisfied, Distinctiveness, VHHab, VHChange, HHab, HChange, MHab, MChange,LHab,LChange)
    
    # Check for any NA values in the dataset
    for (i in datasets){
      if (any(is.na(i))) {
        return("Please check metric is filled in appropriately before continuing")
      }
    }
    
    return(Satisfied)
  }
   
  
  clean_hedgesum_dataset <- function(metric) {
    # Read the dataset
    Satisfied <- openxlsx::read.xlsx(metric, sheet = "Trading Summary Hedgerows", cols = 6, rows = 5:9, colNames = FALSE, skipEmptyRows = TRUE)
    Distinctiveness <- openxlsx::read.xlsx(metric, sheet = "Trading Summary Hedgerows", cols = 2, rows = 5:9, colNames = FALSE, skipEmptyRows = TRUE)
    VHHab <- openxlsx::read.xlsx(metric, sheet = "Trading Summary Hedgerows", cols = 2, rows = 14, colNames = FALSE, skipEmptyRows = TRUE)
    VHChange <- openxlsx::read.xlsx(metric, sheet = "Trading Summary Hedgerows", cols = 5, rows = 14, colNames = FALSE, skipEmptyRows = TRUE)
    HHab <- openxlsx::read.xlsx(metric, sheet = "Trading Summary Hedgerows", cols = 2, rows = 22:24, colNames = FALSE, skipEmptyRows = TRUE)
    HChange <- openxlsx::read.xlsx(metric, sheet = "Trading Summary Hedgerows", cols = 5, rows = 22:24, colNames = FALSE, skipEmptyRows = TRUE)
    MHab <- openxlsx::read.xlsx(metric, sheet = "Trading Summary Hedgerows", cols = 2, rows = 32:36, colNames = FALSE, skipEmptyRows = TRUE)
    MChange <- openxlsx::read.xlsx(metric, sheet = "Trading Summary Hedgerows", cols = 5, rows = 32:36, colNames = FALSE, skipEmptyRows = TRUE)
    VLHab <- openxlsx::read.xlsx(metric, sheet = "Trading Summary Hedgerows", cols = 2, rows = 54, colNames = FALSE, skipEmptyRows = TRUE)
    VLChange <- openxlsx::read.xlsx(metric, sheet = "Trading Summary Hedgerows", cols = 5, rows = 54, colNames = FALSE, skipEmptyRows = TRUE)
    
    datasets <- list(Satisfied, Distinctiveness, VHHab, VHChange, HHab, HChange, MHab, MChange,VLHab,VLChange)
    
    # Check for any NA values in the dataset
    for (i in datasets){
      if (any(is.na(i))) {
        return("Please check metric is filled in appropriately before continuing")
      }
    }
    
    return(Satisfied)
  }
  
  clean_watersum_dataset <- function(metric) {
    # Read the dataset
    Satisfied <- openxlsx::read.xlsx(metric, sheet = "Trading Summary WaterC's", cols = 7, rows = 4:9, colNames = FALSE, skipEmptyRows = TRUE)
    Distinctiveness <- openxlsx::read.xlsx(metric, sheet = "Trading Summary WaterC's", cols = 2, rows = 4:9, colNames = FALSE, skipEmptyRows = TRUE)
    VHHab <- openxlsx::read.xlsx(metric, sheet = "Trading Summary WaterC's", cols = 2, rows = 13, colNames = FALSE, skipEmptyRows = TRUE)
    VHChange <- openxlsx::read.xlsx(metric, sheet = "Trading Summary WaterC's", cols = 5, rows = 13, colNames = FALSE, skipEmptyRows = TRUE)
    HHab <- openxlsx::read.xlsx(metric, sheet = "Trading Summary WaterC's", cols = 2, rows = 22, colNames = FALSE, skipEmptyRows = TRUE)
    HChange <- openxlsx::read.xlsx(metric, sheet = "Trading Summary WaterC's", cols = 5, rows = 22, colNames = FALSE, skipEmptyRows = TRUE)
    MHab <- openxlsx::read.xlsx(metric, sheet = "Trading Summary WaterC's", cols = 2, rows = 30:31, colNames = FALSE, skipEmptyRows = TRUE)
    MChange <- openxlsx::read.xlsx(metric, sheet = "Trading Summary WaterC's", cols = 5, rows = 30:31, colNames = FALSE, skipEmptyRows = TRUE)
    LHab <- openxlsx::read.xlsx(metric, sheet = "Trading Summary WaterC's", cols = 2, rows = 42, colNames = FALSE, skipEmptyRows = TRUE)
    LChange <- openxlsx::read.xlsx(metric, sheet = "Trading Summary WaterC's", cols = 5, rows = 42, colNames = FALSE, skipEmptyRows = TRUE)
    
    datasets <- list(Satisfied, Distinctiveness, VHHab, VHChange, HHab, HChange, MHab, MChange,LHab,LChange)
    
    # Check for any NA values in the dataset
    for (i in datasets){
      if (any(is.na(i))) {
        return("Please check metric is filled in appropriately before continuing")
      }
    }
    
    return(Satisfied)
  }
  
  

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
    
    # Check for the presence of "Check Data ⚠" in any dataset
    for (i in datasets) {
      if (any(i == "Check Data ⚠", i == "Error ▲",na.rm = TRUE)) {
        return("Please check metric is filled in appropriately before continuing")
      }
    }
    
    return(NetHabUnits)
  }
  
  
  clean_onsitehab_baseline <- function(metric) {
    # Read the dataset
    df <- openxlsx::read.xlsx(metric, "A-1 On-Site Habitat Baseline", colNames = FALSE, startRow = 10)
    
    # Remove the first column
    df <- df[, -36]
    df <- df[,-32]
    df <- df[,-3]
    df <- df[-1,]
    
    # Keep only rows where column 1 has data
    df <- df[df[[1]] != "" & !is.na(df[[1]]), , drop = FALSE]
    
    # Remove the last row if it's a duplicate of the previous one
    if (nrow(df) > 1 && identical(df[nrow(df), ], df[nrow(df) - 1, ])) {
      df <- df[-nrow(df), , drop = FALSE]
    }
    
    # Check for the presence of "Check Data ⚠" in any dataset
    for (i in df){
      if (any(is.na(i))) {
        return("Please check metric is filled in appropriately before continuing")
      }
    }
    
    return(df)
  }
  
  
  
  
  