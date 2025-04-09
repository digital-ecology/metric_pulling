#' cleans onsitehab baseline metric data 
#'
#' @param metric a filepath to a metric
#'
#' @return error messages, a string of error messages to be shown so user knows where data is incorrect 
#'
#' @examples checked <- check_A1(metric = system.file("extdata", "OnSiteBoth.xlsx", package = "metricpulling"))
check_A1 <- function(metric) {
  
  #read the dataset
  df <- openxlsx::read.xlsx(metric, "A-1 On-Site Habitat Baseline", cols = c(5:6, 8:9, 11, 17:25), colNames = TRUE, startRow = 10)
  
  #remove 'xml:space="preserve">' from column names
  colnames(df) <- gsub('xml:space="preserve">', '', colnames(df))
  
  errormessages <- c()
  if(!is.na(df$Broad.Habitat[1])) {
    
    colnames(df) <- gsub("\\.", " ", colnames(df))
  
    #sometimes, there are entire rows of NAs. for the baseline sheet, you can remove all rows where col 1 = NA, if someone hasnt put broad hab in, wont exist
    df <- df[!is.na(df[, 1]), ]
    
    #correct the ones which WILL have NAs, as autocorrected
    df[[7]][is.na(df[[7]])] <- 0
    df[[8]][is.na(df[[8]])] <- 0
    
    colnames(df) <-c("Broad Habitat", "Habitat Type", "Area (Ha)", "Distinctiveness", "Condition",
                                 "Total Habitat Units", "Area Retained (Ha)", "Area Enhanced  (Ha)",
                                 "Baseline Units Retained", "Baseline Units Enhanced",
                                 "Area Lost", "Baseline Units Lost")
    
    #make all numbers numeric and round
    df[, c(3, 6:12)] <- lapply(df[, c(3, 6:12)], function(x) round(as.numeric(x), 3))
    
     #first thing that needs to be checked, is if there are any NAs in any column 
    for (i in 1:ncol(df)) { #for each column in the dataframe
      
      if (any(is.na(df[[i]]))) { #if ANY have NA, return a message - can assign this to a message object, to pass back to user
        errormessages<- paste(errormessages, "\n - Column '", names(df)[i], "' contains NA values. Please fill in.", sep = "")
        }
      
      if (any(df[[i]] == "", na.rm = TRUE)) { #if the sheet doesnt read data in, can be replaced with ""
        errormessages <- paste(errormessages, "\n - Column '", names(df)[i], "' contains empty strings. This can be a result of importing a metric with errors in the macros, please check and retry.", sep = "")
      }
      
      }
      
    if (all(df[[6]] == 0, na.rm = TRUE)) {
        errormessages <- paste(errormessages, " \n - Column '", names(df)[6], "' indicates a total of 0 existing habitat units. There may be a 'Check Data' or 'Error in Areas' warning. Please check and fill in if necessary.", sep = "")
      }
  }
  
  metriccheckresults<-list(sheetdata = df,
                           errormessages = errormessages)
  
  return(metriccheckresults)
}



#' cleans onsitehab creation metric data 
#'
#' @param metric feas metric
#'
#' @return df, a cleaned version of the input dataset - to be used as an argument in actual metric pulling function
#' 
#'
#' @examples \dontrun{checked_dataset<-check_A2(metric)}     
check_A2 <- function(metric) {
  
  #read the dataset
  df <- openxlsx::read.xlsx(metric, "A-2 On-Site Habitat Creation", cols = c(4:5,7:8,10, 25), colNames = TRUE, startRow = 10, skipEmptyRows = TRUE)
  
  #remove 'xml:space="preserve">' from column names
  colnames(df) <- gsub('xml:space="preserve">', '', colnames(df))
  
  errormessages <- c()
  if(!is.na(df$Condition[1])) {
   
    #sometimes, there are entire rows of NAs. for the creation sheet, you can remove all rows where col 1 = NA, as thats autofileld
    df <- df[!is.na(df[, 1]), ]
    
    colnames(df) <- c("Proposed Broad Habitat", "Proposed Habitat Type", "Area (Ha)", 
                      "Distinctiveness", "Condition", "Habitat Units Delivered")
    
    #make numeric and round
    df[, c(3, 6)] <- lapply(df[, c(3, 6)], function(x) round(as.numeric(x), 3))
    
    #first thing that needs to be checked, is if there are any NAs in any column 
    for (i in 1:ncol(df)) { #for each column in the dataframe
      
      if (any(is.na(df[[i]]))) { #if ANY have NA, return a message - can assign this to a message object, to pass back to user
        errormessages<- paste(errormessages, " \n - Column '", names(df)[i], "' contains NA values.\n", sep = "")
      }
      
      if (any(df[[i]] == "", na.rm = TRUE)) {
        errormessages <- paste(errormessages, " \n - Column '", names(df)[i], "' contains empty strings. This can be a result of importing a metric with errors in the macros, please check and retry.\n", sep = "")
      }
    }
  }
  
  metriccheckresults<-list(sheetdata = df,
                           errormessages = errormessages)
  
  return(metriccheckresults)
  
  }

#' cleans onsitehab enchancement metric data 
#'
#' @param metric feas metric
#'
#' @return df, a cleaned version of the input dataset - to be used as an argument in actual metric pulling function
#' 
#'
#' @examples \dontrun{checked_dataset<-check_A3(metric)}   
check_A3 <- function(metric) {
  
  df <- openxlsx::read.xlsx(metric, sheet = "A-3 On-Site Habitat Enhancement", 
                            cols = c(6,17:18,22:23,25,40), 
                            colNames = TRUE, startRow = 11)
  
  #remove 'xml:space="preserve">' from column names
  colnames(df) <- gsub('xml:space="preserve">', '', colnames(df))
  
  errormessages <- c()
  if(!is.na(df$Proposed.habitat[1])) {
   
    #sometimes, there are entire rows of NAs. for the enhancement sheet, you can remove all rows where col 3 = NA, as thats manually entered by user
    df <- df[!is.na(df[, 3]), ]
    
    #set colnames 
    colnames(df) <- c("Existing Habitat", "Proposed Broad Habitat", "Proposed Habitat Type",
                      "Area (Ha)", "Distinctiveness", "Condition", "Habitat Units Delivered") #missing area if not filled in 
    
    #make numeric and round
    df[, c(4, 7)] <- lapply(df[, c(4, 7)], function(x) round(as.numeric(x), 3))
    
    #first thing that needs to be checked, is if there are any NAs in any column 
    for (i in 1:ncol(df)) { #for each column in the dataframe
      
      if (any(is.na(df[[i]]))) { #if ANY have NA, return a message - can assign this to a message object, to pass back to user
        errormessages<- paste(errormessages, " \n - Column '", names(df)[i], "' contains NA values.\n", sep = "")
        }
      
      if (any(df[[i]] == "", na.rm = TRUE)) {
        errormessages <- paste(errormessages, " \n - Column '", names(df)[i], "' contains empty strings. This can be a result of importing a metric with errors in the macros, please check and retry.\n", sep = "")
      }
      
    }
    
  }

  metriccheckresults<-list(sheetdata = df,
                           errormessages = errormessages)
  
  return(metriccheckresults)
}

#' cleans onsitehedge baseline metric data 
#'
#' @param metric feas metric
#'
#' @return df, a cleaned version of the input dataset - to be used as an argument in actual metric pulling function
#' 
#'
#' @examples \dontrun{checked_dataset<-check_B1(metric)}     
check_B1 <- function(metric) {
 
  #read the dataset
  df <- openxlsx::read.xlsx(metric, "B-1 On-Site Hedge Baseline", cols = c(3:5,8,14,16:21), colNames = TRUE, startRow = 9)
  
  #remove 'xml:space="preserve">' from column names
  colnames(df) <- gsub('xml:space="preserve">', '', colnames(df))
  
  errormessages <- c()
  if(!is.na(df$Habitat.type[1])) {
    
    #sometimes, there are entire rows of NAs. for the baseline sheet, you can remove all rows where col 2 = NA, as thats manually entered by user
    df <- df[!is.na(df[, 2]), ]
    
    #correct the ones which WILL have NAs, as autocorrected
    df[[7]][is.na(df[[7]])] <- 0
    df[[6]][is.na(df[[6]])] <- 0
    
    #set colnames 
    colnames(df) <- c("Hedge Number", "Habitat Type", "Length (Km)", "Condition", "Baseline Units",
                      "Length Retained", "Length Enhanced", "Units Retained", "Units Enhanced",
                      "Length Lost", "Units Lost")
    
    #make numeric and round
    df[, c(3, 5:11)] <- lapply(df[, c(3, 5:11)], function(x) round(as.numeric(x), 3))
    
    
    #first thing that needs to be checked, is if there are any NAs in any column 
    for (i in 1:ncol(df)) { #for each column in the dataframe
      
      if (any(is.na(df[[i]]))) { #if ANY have NA, return a message - can assign this to a message object, to pass back to user
        errormessages<- paste(errormessages, " \n - Column '", names(df)[i], "' contains NA values.\n", sep = "")
        }
      
      if (any(df[[i]] == "", na.rm = TRUE)) {
        errormessages <- paste(errormessages, " \n - Column '", names(df)[i], "' contains empty strings. This can be a result of importing a metric with errors in the macros, please check and retry.\n", sep = "")
      }
    }
    
  }
  
  metriccheckresults<-list(sheetdata = df,
                           errormessages = errormessages)
  
  return(metriccheckresults)
}


#' cleans onsitehedge creation loss data 
#'
#' @param metric feas metric
#'
#' @return df, a cleaned version of the input dataset - to be used as an argument in actual metric pulling function
#' 
#'
#' @examples \dontrun{checked_dataset<-check_B2(metric)}    
check_B2 <- function(metric) {
  
  #read the dataset
  df <- openxlsx::read.xlsx(metric, "B-2 On-Site Hedge Creation", cols = c(3,4,5,6,8,10,23), colNames = TRUE, startRow = 11)
  
  #remove 'xml:space="preserve">' from column names
  colnames(df) <- gsub('xml:space="preserve">', '', colnames(df))
  
  errormessages <- c()
  
  if(!is.na(df$Habitat.type[1])) {
   
    #sometimes, there are entire rows of NAs. for the baseline sheet, you can remove all rows where col 2 = NA, as thats manually entered by user
    df <- df[!is.na(df[, 2]), ]
    
    #make numeric and round
    df[, c(3, 7)] <- lapply(df[, c(3, 7)], function(x) round(as.numeric(x), 3))
    
    #set colnames 
    colnames(df) <- c("New Hedge Number", "Created Habitat Type", "Length (Km)", "Distinctiveness",
                      "Condition", "Strategic Significance", "Units Created")
    
    
    #first thing that needs to be checked, is if there are any NAs in any column 
    for (i in 1:ncol(df)) { #for each column in the dataframe
      
      if (any(is.na(df[[i]]))) { #if ANY have NA, return a message - can assign this to a message object, to pass back to user
        errormessages<- paste(errormessages, " \n - Column '", names(df)[i], "' contains NA values.\n", sep = "")
        }
      
      if (any(df[[i]] == "", na.rm = TRUE)) {
        errormessages <- paste(errormessages, " \n - Column '", names(df)[i], "' contains empty strings. This can be a result of importing a metric with errors in the macros, please check and retry.\n", sep = "")
      }
    }
    
  }
  
 
  metriccheckresults<-list(sheetdata = df,
                           errormessages = errormessages)
  
  return(metriccheckresults)
}

#' cleans onsitehedge enhancement loss data 
#'
#' @param metric feas metric
#'
#' @return df, a cleaned version of the input dataset - to be used as an argument in actual metric pulling function
#' 
#'
#' @examples \dontrun{checked_dataset<-check_B3(metric)}   
check_B3 <- function(metric) {
  
  #read the dataset
  df <- openxlsx::read.xlsx(metric, "B-3 On-Site Hedge Enhancement", cols = c(3, 4, 13, 19, 21, 34), colNames = TRUE, startRow = 11)
  
  #remove 'xml:space="preserve">' from column names
  colnames(df) <- gsub('xml:space="preserve">', '', colnames(df))
  
  errormessages <- c()
  
  if(!is.na(df$Condition[1])) {
    
    #sometimes, there are entire rows of NAs. for the baseline sheet, you can remove all rows where col 2 = NA, as thats manually entered by user
    df <- df[!is.na(df[, 4]), ]
    
    #set colnames 
    colnames(df) <- c("Baseline Habitat", "Length (Km)", "Proposed Habitat Type", "Condition", 
                      "Strategic Significance", "Units Created")
    
    #make numeric and round
    df[, c(2, 6)] <- lapply(df[, c(2, 6)], function(x) round(as.numeric(x), 3))
    
    
    #first thing that needs to be checked, is if there are any NAs in any column 
    for (i in 1:ncol(df)) { #for each column in the dataframe
      
      if (any(is.na(df[[i]]))) { #if ANY have NA, return a message - can assign this to a message object, to pass back to user
        errormessages<- paste(errormessages, " \n - Column '", names(df)[i], "' contains NA values.\n", sep = "")
        }
      
      if (any(df[[i]] == "", na.rm = TRUE)) {
        errormessages <- paste(errormessages, " \n - Column '", names(df)[i], "' contains empty strings. This can be a result of importing a metric with errors in the macros, please check and retry.\n", sep = "")
      }
    }
    
    
    
  }

  metriccheckresults<-list(sheetdata = df,
                           errormessages = errormessages)
  
  return(metriccheckresults)
  
}

#' cleans C1 water metric data 
#'
#' @param metric feas metric
#'
#' @return df, a cleaned version of the input dataset - to be used as an argument in actual metric pulling function
#'
#' @examples \dontrun{checked_dataset<-check_C1(metric)}  
check_C1 <- function(metric) {
  
  #read the dataset
  df <- openxlsx::read.xlsx(metric, "C-1 On-Site WaterC' Baseline", cols = c(4,5,8,10,18,21,23,25,26), colNames = TRUE, startRow = 9)
  
  #remove 'xml:space="preserve">' from column names
  colnames(df) <- gsub('xml:space="preserve">', '', colnames(df))
  
  errormessages <- c()
  if(!is.na(df$Watercourse.type[1])) {
    
    #sometimes, there are entire rows of NAs. for the baseline sheet, you can remove all rows where col 2 = NA, as thats manually entered by user
    df <- df[!is.na(df[, 1]), ]
    
    #correct the ones which WILL have NAs, as autocorrected
    df[[6]][is.na(df[[6]])] <- 0
    df[[8]][is.na(df[[8]])] <- 0
    
    #set colnames 
    colnames(df) <- c("Watercourse Type", "Length (Km)", "Condition", 
                      "Strategic Significance", "Existing Units", "Length Retained (Km)",
                      "Units Retained", "Length Lost (Km)", "Units Lost")
    
    #make numeric and round
    df[, c(2, 5:9)] <- lapply(df[, c(2, 5:9)], function(x) round(as.numeric(x), 3))

  #first thing that needs to be checked, is if there are any NAs in any column 
  for (i in 1:ncol(df)) { #for each column in the dataframe
    
    if (any(is.na(df[[i]]))) { #if ANY have NA, return a message - can assign this to a message object, to pass back to user
      errormessages<- paste(errormessages, " \n - Column '", names(df)[i], "' contains NA values.\n", sep = "")
      }
    
    if (any(df[[i]] == "", na.rm = TRUE)) {
      errormessages <- paste(errormessages, " \n - Column '", names(df)[i], "' contains empty strings. This can be a result of importing a metric with errors in the macros, please check and retry.\n", sep = "")
    }
  }
  
  }
  
  metriccheckresults<-list(sheetdata = df,
                           errormessages = errormessages)
  
  return(metriccheckresults)
}
  
#' cleans C2 water metric data 
#'
#' @param metric feas metric
#'
#' @return df, a cleaned version of the input dataset - to be used as an argument in actual metric pulling function
#'
#' @examples \dontrun{checked_dataset<-check_C2(metric)}    
check_C2 <- function(metric) {
  
  #read the dataset
  df <- openxlsx::read.xlsx(metric, "C-2 On-Site WaterC' Creation", cols = c(3,4,7,9,26), colNames = TRUE, startRow = 11)
  
  #remove 'xml:space="preserve">' from column names
  colnames(df) <- gsub('xml:space="preserve">', '', colnames(df))
  
  errormessages <- c()
  if(!is.na(df$Watercourse.type[1])) {
    
    #sometimes, there are entire rows of NAs. for the baseline sheet, you can remove all rows where col 2 = NA, as thats manually entered by user
    df <- df[!is.na(df[, 1]), ]
    
    #set colnames 
    colnames(df) <- c("Watercourse Type Created", "Length (Km)", "Target Condition", 
                      "Strategic Significance", "Units Created")
    
    #make numeric and round
    df[, c(2, 5)] <- lapply(df[, c(2, 5)], function(x) round(as.numeric(x), 3))

  #first thing that needs to be checked, is if there are any NAs in any column 
  for (i in 1:ncol(df)) { #for each column in the dataframe
    
    if (any(is.na(df[[i]]))) { #if ANY have NA, return a message - can assign this to a message object, to pass back to user
      errormessages<- paste(errormessages, " \n - Column '", names(df)[i], "' contains NA values.\n", sep = "")
      }
    
    if (any(df[[i]] == "", na.rm = TRUE)) {
      errormessages <- paste(errormessages, " \n - Column '", names(df)[i], "' contains empty strings. This can be a result of importing a metric with errors in the macros, please check and retry.\n", sep = "")
    }
  }
  
  
}
  metriccheckresults<-list(sheetdata = df,
                           errormessages = errormessages)
  
  return(metriccheckresults)
}

  
#' cleans C3 water metric data 
#'
#' @param metric feas metric
#'
#' @return df, a cleaned version of the input dataset - to be used as an argument in actual metric pulling function
#'
#' @examples \dontrun{checked_dataset<-check_C3(metric)}    
check_C3 <- function(metric) {
  
  #read the dataset
  df <- openxlsx::read.xlsx(metric, "C-3 On-Site WaterC' Enhancement", cols = c(3,7,14,17,20,22,39), colNames = TRUE, startRow = 11)
  
  #remove 'xml:space="preserve">' from column names
  colnames(df) <- gsub('xml:space="preserve">', '', colnames(df))
  
  errormessages <- c()
  
    if(!is.na(df$Condition[1])) {
    
    #sometimes, there are entire rows of NAs. for the baseline sheet, you can remove all rows where col 2 = NA, as thats manually entered by user
    df <- df[!is.na(df[, 5]), ]
    
    #set colnames 
    colnames(df) <- c("Existing Watercourse Type", "Existing Condition", "Proposed Watercourse Type", "Length (Km)", 
                      "Target Condition", "Strategic Significance", "Units Enhanced")
    
    #make numeric and round
    df[, c(4, 7)] <- lapply(df[, c(4, 7)], function(x) round(as.numeric(x), 3))
   
  #first thing that needs to be checked, is if there are any NAs in any column 
  for (i in 1:ncol(df)) { #for each column in the dataframe
    
    if (any(is.na(df[[i]]))) { #if ANY have NA, return a message - can assign this to a message object, to pass back to user
      errormessages<- paste(errormessages, " \n - Column '", names(df)[i], "' contains NA values.\n", sep = "")
      }
    
    if (any(df[[i]] == "", na.rm = TRUE)) {
      errormessages <- paste(errormessages, " \n - Column '", names(df)[i], "' contains empty strings. This can be a result of importing a metric with errors in the macros, please check and retry.\n", sep = "")
    }
  }
  
}
  
  metriccheckresults<-list(sheetdata = df,
                           errormessages = errormessages)
  
  return(metriccheckresults)
}

#' cleans habsum metric data 
#'
#' @param metric feas metric
#'
#' @return df, a cleaned version of the input dataset - to be used as an argument in actual metric pulling function
#' 
#'
#' @examples \dontrun{checked_dataset<-checkhabsum_dataset(metric)}   
checkhabsum_dataset <- function(metric) {
  
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
        errormessages <- paste(errormessages, dataset_name, "Column '", names(df)[i], "' contains NA values.\n", sep = "")
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
#'
#' @examples \dontrun{checked_dataset<-checkhedgesum_dataset(metric)}     
checkhedgesum_dataset <- function(metric) {
  
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
        errormessages <- paste(errormessages, dataset_name, "Column '", names(df)[i], "' contains NA values.\n", sep = "")
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
#'
#' @examples \dontrun{checked_dataset<-checkwatersum_dataset(metric)}
checkwatersum_dataset <- function(metric) {
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
        errormessages <- paste(errormessages, dataset_name, "Column '", names(df)[i], "' contains NA values.\n", sep = "")
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
#'
#' @examples \dontrun{checked_dataset<-checkonsitenet_dataset(metric)}    
checkonsitenet_dataset <- function(metric) {
  
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
        errormessages <- paste(errormessages, dataset_name, "Column", names(df)[i], "contains NA/Check Data ⚠/Error ▲ values.\n")
      }
    }
  }
  
  return(errormessages)
}