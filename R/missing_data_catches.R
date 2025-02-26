#' cleans C1 water metric data 
#'
#' @param metric feas metric
#'
#' @return df, a cleaned version of the input dataset - to be used as an argument in actual metric pulling function
#' @export
#'
#' @examples \dontrun{checked_dataset<-clean_C1_dataset(metric)}  
clean_c1_dataset <- function(metric) {
  
  #read the dataset
  df <- openxlsx::read.xlsx(metric, "C-1 On-Site WaterC' Baseline", cols = c(4,5,6,8,10,23,24,25,26), colNames = TRUE, startRow = 9)
  
  #row nrow of the SECOND col with data in it, which is the last lines of the habitats, giving nrow for rest 
  df[df == ""] <- NA #change any empty values with NA 
  numberrows <- sum(!is.na(df[[2]])) #number of non NA vals
  df <- df[1:numberrows, ] #chop dataframe beyond numberrows
  df <- df[rowSums(is.na(df)) < ncol(df), ] #remove any straggling rows which are fully NA values
  errormessages <- c()
  
  #first thing that needs to be checked, is if there are any NAs in any column 
  for (i in 1:ncol(df)) { #for each column in the dataframe
    
    if (any(is.na(df[[i]]))) { #if ANY have NA, return a message - can assign this to a message object, to pass back to user
      errormessages<- paste(errormessages, "Column", i, "contains NA values.\n")
      
    }
  }
  
  
  
  return(errormessages)
}
  
#' cleans C2 water metric data 
#'
#' @param metric feas metric
#'
#' @return df, a cleaned version of the input dataset - to be used as an argument in actual metric pulling function
#' @export
#'
#' @examples \dontrun{checked_dataset<-clean_C2_dataset(metric)}    
clean_c2_dataset <- function(metric) {
  
  #read the dataset
  df <- openxlsx::read.xlsx(metric, "C-2 On-Site WaterC' Creation", cols = c(3,4,5,7,9,26,29), colNames = TRUE, startRow = 11)
  
  #row nrow of the SECOND col with data in it, which is the last lines of the habitats, giving nrow for rest 
  df[df == ""] <- NA #change any empty values with NA 
  numberrows <- sum(!is.na(df[[2]])) #number of non NA vals
  df <- df[1:numberrows, ] #chop dataframe beyond numberrows
  df <- df[rowSums(is.na(df)) < ncol(df), ] #remove any straggling rows which are fully NA values
  errormessages <- c()
  
  #first thing that needs to be checked, is if there are any NAs in any column 
  for (i in 1:ncol(df)) { #for each column in the dataframe
    
    if (any(is.na(df[[i]]))) { #if ANY have NA, return a message - can assign this to a message object, to pass back to user
      errormessages<- paste(errormessages, "Column", i, "contains NA values.\n")
      
    }
  }
  
  
  
  return(errormessages)
}

  
#' cleans C3 water metric data 
#'
#' @param metric feas metric
#'
#' @return df, a cleaned version of the input dataset - to be used as an argument in actual metric pulling function
#' @export
#'
#' @examples \dontrun{checked_dataset<-clean_C3_dataset(metric)}    
clean_c3_dataset <- function(metric) {
  
  #read the dataset
  df <- openxlsx::read.xlsx(metric, "C-3 On-Site WaterC' Enhancement", cols = c(3,7,14,17,18,20,22,39), colNames = TRUE, startRow = 11)
  
  #row nrow of the SECOND col with data in it, which is the last lines of the habitats, giving nrow for rest 
  df[df == ""] <- NA #change any empty values with NA 
  numberrows <- sum(!is.na(df[[2]])) #number of non NA vals
  df <- df[1:numberrows, ] #chop dataframe beyond numberrows
  df <- df[rowSums(is.na(df)) < ncol(df), ] #remove any straggling rows which are fully NA values
  errormessages <- c()
  
  #first thing that needs to be checked, is if there are any NAs in any column 
  for (i in 1:ncol(df)) { #for each column in the dataframe
    
    if (any(is.na(df[[i]]))) { #if ANY have NA, return a message - can assign this to a message object, to pass back to user
      errormessages<- paste(errormessages, "Column", i, "contains NA values.\n")
      
    }
  }
  
  
  
  return(errormessages)
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
  
  errormessages <- c()
  
  # Read all datasets and check for missing values
  for (dataset_name in names(datasets_info)) {
    info <- datasets_info[[dataset_name]]
    df <- openxlsx::read.xlsx(metric, sheet = "Trading Summary Area Habitats", 
                              cols = info$cols, rows = info$rows, 
                              colNames = FALSE, skipEmptyRows = TRUE)
    
    # Replace empty values with NA
    df[df == ""] <- NA 
    
    # Check for missing values
    for (i in 1:ncol(df)) {
      if (any(is.na(df[[i]]))) {
        errormessages <- paste(errormessages, dataset_name, "- Column", i, "contains NA values.\n")
      }
    }
  }
  
  return(errormessages)
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
  
  # Define column and row ranges for each dataset
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
  
  errormessages <- c()
  
  # Read all datasets and check for missing values
  for (dataset_name in names(datasets_info)) {
    info <- datasets_info[[dataset_name]]
    df <- openxlsx::read.xlsx(metric, sheet = "Trading Summary Hedgerows", 
                              cols = info$cols, rows = info$rows, 
                              colNames = FALSE, skipEmptyRows = TRUE)
    
    # Replace empty values with NA
    df[df == ""] <- NA 
    
    # Check for missing values
    for (i in 1:ncol(df)) {
      if (any(is.na(df[[i]]))) {
        errormessages <- paste(errormessages, dataset_name, "- Column", i, "contains NA values.\n")
      }
    }
  }
  
  return(errormessages)
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
  
  
  
  errormessages <- c()
  
  # Read all datasets and check for missing values
  for (dataset_name in names(datasets_info)) {
    info <- datasets_info[[dataset_name]]
    df <- openxlsx::read.xlsx(metric, sheet = "Trading Summary WaterC's", 
                              cols = info$cols, rows = info$rows, 
                              colNames = FALSE, skipEmptyRows = TRUE)
    
    # Replace empty values with NA
    df[df == ""] <- NA 
    
    # Check for missing values
    for (i in 1:ncol(df)) {
      if (any(is.na(df[[i]]))) {
        errormessages <- paste(errormessages, dataset_name, "- Column", i, "contains NA values.\n")
      }
    }
  }
  
  return(errormessages)
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
  
  # Define column and row ranges for each dataset
  datasets_info <- list(
    BaseHabUnits      = list(cols = 8, rows = 8),
    BaseHedgeUnits    = list(cols = 8, rows = 9),
    BaseWaterUnits    = list(cols = 8, rows = 10),
    PIHabUnits    = list(cols = 8, rows = 12),
    PIHedgeUnits  = list(cols = 8, rows = 13),
    PIWaterUnits  = list(cols = 8, rows = 14),
    NetHabUnits   = list(cols = 8, rows = 16),
    NetHedgeUnits       = list(cols = 8, rows = 17),
    NetWaterUnits     = list(cols = 8, rows = 18),
    NetHabPercent     = list(cols = 10, rows = 16),
    NetHedgePercent= list(cols = 10, rows = 17),
    NetWaterPercent= list(cols = 10, rows = 18),
    TradeSatisfied= list(cols = 6, rows = 55),
    HabDeficit= list(cols = 8, rows = 61),
    HedgeDeficit= list(cols = 8, rows = 62),
    WaterDeficit= list(cols = 8, rows = 63)
  )
  
  errormessages <- c()
  
  # Read all datasets and check for missing values
  for (dataset_name in names(datasets_info)) {
    info <- datasets_info[[dataset_name]]
    df <- openxlsx::read.xlsx(metric, sheet = "Headline Results", 
                              cols = info$cols, rows = info$rows, 
                              colNames = FALSE, skipEmptyRows = TRUE)
    
    # Replace empty values with NA
    df[df == ""] <- NA 
    
    # Check for missing values
    for (i in 1:ncol(df)) {
      if (any(is.na(df[[i]]) | df[[i]] == "Check Data ⚠" | df[[i]] == "Error ▲")) {
        errormessages <- paste(errormessages, dataset_name, "- Column", i, "contains NA/Check Data ⚠/Error ▲ values.\n")
      }
    }
  }
  
  return(errormessages)
}
#' cleans onsitehab baseline metric data 
#'
#' @param metric feas metric
#'
#' @return error messages, a string of error messages to be shown so user knows where data is incorrect 
#' 
#' @export
#'
#' @examples \dontrun{checked_dataset<-clean_onsitehab_baseline(metric)}   
clean_onsitehab_baseline <- function(metric) {
  
  #read the dataset
  df <- openxlsx::read.xlsx(metric, "A-1 On-Site Habitat Baseline", cols = c(4:6, 8:9, 11, 17:25, 28), colNames = TRUE, startRow = 10)
  
  #row nrow of the SECOND col with data in it, which is the last lines of the habitats, giving nrow for rest 
  df[df == ""] <- NA #change any empty values with NA 
  numberrows <- sum(!is.na(df[[2]])) #number of non NA vals
  df <- df[1:numberrows, ] #chop dataframe beyond numberrows
  df <- df[rowSums(is.na(df)) < ncol(df), ] #remove any straggling rows which are fully NA values
  errormessages <- c()
  
  #first thing that needs to be checked, is if there are any NAs in any column 
  for (i in 1:ncol(df)) { #for each column in the dataframe
    
    if (any(is.na(df[[i]]))) { #if ANY have NA, return a message - can assign this to a message object, to pass back to user
      errormessages<- paste(errormessages, "Column", i, "contains NA values.\n")
      
    }
  }
    return(errormessages)
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
  
  #read the dataset
  df <- openxlsx::read.xlsx(metric, "A-2 On-Site Habitat Creation", cols = c(4,5,7,8,10,12,19,25), colNames = TRUE, startRow = 10)
  
  #row nrow of the SECOND col with data in it, which is the last lines of the habitats, giving nrow for rest 
  df[df == ""] <- NA #change any empty values with NA 
  numberrows <- sum(!is.na(df[[2]])) #number of non NA vals
  df <- df[1:numberrows, ] #chop dataframe beyond numberrows
  df <- df[rowSums(is.na(df)) < ncol(df), ] #remove any straggling rows which are fully NA values
  errormessages <- c()
  
  #first thing that needs to be checked, is if there are any NAs in any column 
  for (i in 1:ncol(df)) { #for each column in the dataframe
    
    if (any(is.na(df[[i]]))) { #if ANY have NA, return a message - can assign this to a message object, to pass back to user
      errormessages<- paste(errormessages, "Column", i, "contains NA values.\n")
      
    }
  }
  
  
  
  return(errormessages)
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
  
  #read the dataset
  df <- openxlsx::read.xlsx(metric, "A-3 On-Site Habitat Enhancement", cols = c(6,17,18,22,23,25,27,30,40), colNames = TRUE, startRow = 11)
  
  #set columns to tell user where data is missing 
  colnames(df)[colnames(df) == "X4"] <- "Area"
  
  #row nrow of the SECOND col with data in it, which is the last lines of the habitats, giving nrow for rest 
  df[df == ""] <- NA #change any empty values with NA 
  numberrows <- sum(!is.na(df[[2]])) #number of non NA vals
  df <- df[1:numberrows, ] #chop dataframe beyond numberrows
  df <- df[rowSums(is.na(df)) < ncol(df), ] #remove any straggling rows which are fully NA values
  errormessages <- c()
  
  #first thing that needs to be checked, is if there are any NAs in any column 
  for (i in 1:ncol(df)) { #for each column in the dataframe
    
    if (any(is.na(df[[i]]))) { #if ANY have NA, return a message - can assign this to a message object, to pass back to user
      errormessages<- paste(errormessages, "Column", i, "contains NA values.\n")
      
    }
  }
  
  
  
  return(errormessages)
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
  
  #read the dataset
  df <- openxlsx::read.xlsx(metric, "B-1 On-Site Hedge Baseline", cols = c(3,4,5,8,10,14,16,17,18,19,20,21), colNames = TRUE, startRow = 9)
  
  #row nrow of the SECOND col with data in it, which is the last lines of the habitats, giving nrow for rest 
  df[df == ""] <- NA #change any empty values with NA 
  numberrows <- sum(!is.na(df[[2]])) #number of non NA vals
  df <- df[1:numberrows, ] #chop dataframe beyond numberrows
  df <- df[rowSums(is.na(df)) < ncol(df), ] #remove any straggling rows which are fully NA values
  errormessages <- c()
  
  #first thing that needs to be checked, is if there are any NAs in any column 
  for (i in 1:ncol(df)) { #for each column in the dataframe
    
    if (any(is.na(df[[i]]))) { #if ANY have NA, return a message - can assign this to a message object, to pass back to user
      errormessages<- paste(errormessages, "Column", i, "contains NA values.\n")
      
    }
  }
  
  
  
  return(errormessages)
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
  
  #read the dataset
  df <- openxlsx::read.xlsx(metric, "B-2 On-Site Hedge Creation", cols = c(3,4,5,6,8,10,23), colNames = TRUE, startRow = 11)
  
  df[df == ""] <- NA #change any empty values with NA 
  numberrows <- sum(!is.na(df[[2]])) #number of non NA vals
  df <- df[1:numberrows, ] #chop dataframe beyond numberrows
  df <- df[rowSums(is.na(df)) < ncol(df), ] #remove any straggling rows which are fully NA values
  errormessages <- c()
  
  #first thing that needs to be checked, is if there are any NAs in any column 
  for (i in 1:ncol(df)) { #for each column in the dataframe
    
    if (any(is.na(df[[i]]))) { #if ANY have NA, return a message - can assign this to a message object, to pass back to user
      errormessages<- paste(errormessages, "Column", i, "contains NA values.\n")
      
    }
  }
  
  
  
  return(errormessages)
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
  
  #read the dataset
  df <- openxlsx::read.xlsx(metric, "B-3 On-Site Hedge Enhancement", cols = c(2,3,7,16,17,19,21,34), colNames = TRUE, startRow = 11)
  
  df[df == ""] <- NA #change any empty values with NA 
  numberrows <- sum(!is.na(df[[2]])) #number of non NA vals
  df <- df[1:numberrows, ] #chop dataframe beyond numberrows
  df <- df[rowSums(is.na(df)) < ncol(df), ] #remove any straggling rows which are fully NA values
  errormessages <- c()
  
  #first thing that needs to be checked, is if there are any NAs in any column 
  for (i in 1:ncol(df)) { #for each column in the dataframe
    
    if (any(is.na(df[[i]]))) { #if ANY have NA, return a message - can assign this to a message object, to pass back to user
      errormessages<- paste(errormessages, "Column", i, "contains NA values.\n")
      
    }
  }
  
  
  
  return(errormessages)
}

