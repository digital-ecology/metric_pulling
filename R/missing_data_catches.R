#' cleans onsitehab baseline metric data 
#'
#' @param metric feas metric
#'
#' @return error messages, a string of error messages to be shown so user knows where data is incorrect 
#' 
#' @export
#'
#' @examples \dontrun{checked_dataset<-check_A1(metric)}   
check_A1 <- function(metric) {
  
  #read the dataset
  df <- openxlsx::read.xlsx(metric, "A-1 On-Site Habitat Baseline", cols = c(4:6, 8:9, 11, 17:25), colNames = TRUE, startRow = 10)
  
  errormessages <- c()
  if(!is.na(df$Broad.Habitat[1])) {
    
    df$Area.retained[is.na(df$Area.retained)] <- 0
    df$Area.enhanced[is.na(df$Area.enhanced)] <- 0
    
    colnames(df) <- gsub("\\.", " ", colnames(df))
    
    #row nrow of the SECOND col with data in it, which is the last lines of the habitats, giving nrow for rest 
    df[df == ""] <- NA #change any empty values with NA 
    numberrows <- sum(!is.na(df[[2]])) #number of non NA vals
    df <- df[1:numberrows, ] #chop dataframe beyond numberrows
    df <- df[rowSums(is.na(df)) < ncol(df), ] #remove any straggling rows which are fully NA values
    
    
    #first thing that needs to be checked, is if there are any NAs in any column 
    for (i in 1:ncol(df)) { #for each column in the dataframe
      
      if (any(is.na(df[[i]]))) { #if ANY have NA, return a message - can assign this to a message object, to pass back to user
        errormessages<- paste(errormessages, "Column '", names(df)[i], "' contains NA values.\n", sep = "")
        
      }
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
#' @examples \dontrun{checked_dataset<-check_A2(metric)}     
check_A2 <- function(metric) {
  
  #read the dataset
  df <- openxlsx::read.xlsx(metric, "A-2 On-Site Habitat Creation", cols = c(4,5,7,8,10,12,19,25), colNames = TRUE, startRow = 10)
  
  errormessages <- c()
  if(!is.na(df$Condition[1])) {
    
    colnames(df)[colnames(df) == "X1"] <- "Broad Habitat"
    colnames(df)[colnames(df) == "X2"] <- "Proposed Habitats"
    colnames(df)[colnames(df) == "X3"] <- "Acres (Ha)"
    colnames(df)[colnames(df) == "X8"] <- "Habitat Units Delivered"
    colnames(df) <- gsub("\\.", " ", colnames(df))
    
    #row nrow of the SECOND col with data in it, which is the last lines of the habitats, giving nrow for rest 
    df[df == ""] <- NA #change any empty values with NA 
    numberrows <- sum(!is.na(df[[1]])) #number of non NA vals
    df <- df[1:numberrows, ] #chop dataframe beyond numberrows
    df <- df[rowSums(is.na(df)) < ncol(df), ] #remove any straggling rows which are fully NA values
    
    
    #first thing that needs to be checked, is if there are any NAs in any column 
    for (i in 1:ncol(df)) { #for each column in the dataframe
      
      if (any(is.na(df[[i]]))) { #if ANY have NA, return a message - can assign this to a message object, to pass back to user
        errormessages<- paste(errormessages, "Column '", names(df)[i], "' contains NA values.\n", sep = "")
        
      }
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
#' @examples \dontrun{checked_dataset<-check_A3(metric)}   
check_A3 <- function(metric) {
  
  df <- openxlsx::read.xlsx(metric, sheet = "A-3 On-Site Habitat Enhancement", 
                            cols = c(6,17,18,22,23,25,27,30,40), 
                            colNames = TRUE, startRow = 11)
  
  errormessages <- c()
  if(!is.na(df$Baseline.habitat[1])) {
    
    colnames(df)[colnames(df) == "X5"] <- "Distinctiveness"
    colnames(df)[colnames(df) == "X4"] <- "Acres (Ha)"
    colnames(df)[colnames(df) == "X8"] <- "Habitat Units Delivered"
    colnames(df) <- gsub("\\.", " ", colnames(df))
    
    
    #row nrow of the SECOND col with data in it, which is the last lines of the habitats, giving nrow for rest 
    df[df == ""] <- NA #change any empty values with NA 
    numberrows <- sum(!is.na(df[[2]])) #number of non NA vals
    df <- df[1:numberrows, ] #chop dataframe beyond numberrows
    df <- df[rowSums(is.na(df)) < ncol(df), ] #remove any straggling rows which are fully NA values
    
    
    #first thing that needs to be checked, is if there are any NAs in any column 
    for (i in 1:ncol(df)) { #for each column in the dataframe
      
      if (any(is.na(df[[i]]))) { #if ANY have NA, return a message - can assign this to a message object, to pass back to user
        errormessages<- paste(errormessages, "Column '", names(df)[i], "' contains NA values.\n", sep = "")
        
      }
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
#' @examples \dontrun{checked_dataset<-check_B1(metric)}     
check_B1 <- function(metric) {
  
  #read the dataset
  df <- openxlsx::read.xlsx(metric, "B-1 On-Site Hedge Baseline", cols = c(3,4,5,8,10,14,16,17,18,19,20,21), colNames = TRUE, startRow = 9)
  
  errormessages <- c()
  if(!is.na(df$Hedge.number[1])) {
    
    df$Length.retained[is.na(df$Length.retained)] <- 0
    df$Length.enhanced[is.na(df$Length.enhanced)] <- 0
    colnames(df) <- gsub("\\.", " ", colnames(df))
    
    #row nrow of the SECOND col with data in it, which is the last lines of the habitats, giving nrow for rest 
    df[df == ""] <- NA #change any empty values with NA 
    numberrows <- sum(!is.na(df[[2]])) #number of non NA vals
    df <- df[1:numberrows, ] #chop dataframe beyond numberrows
    df <- df[rowSums(is.na(df)) < ncol(df), ] #remove any straggling rows which are fully NA values
    
    
    #first thing that needs to be checked, is if there are any NAs in any column 
    for (i in 1:ncol(df)) { #for each column in the dataframe
      
      if (any(is.na(df[[i]]))) { #if ANY have NA, return a message - can assign this to a message object, to pass back to user
        errormessages<- paste(errormessages, "Column '", names(df)[i], "' contains NA values.\n", sep = "")
        
      }
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
#' @examples \dontrun{checked_dataset<-check_B2(metric)}    
check_B2 <- function(metric) {
  
  #read the dataset
  df <- openxlsx::read.xlsx(metric, "B-2 On-Site Hedge Creation", cols = c(3,4,5,6,8,10,23), colNames = TRUE, startRow = 11)
  
  errormessages <- c()
  
  if(!is.na(df$Habitat.type[1])) {
    
    colnames(df)[colnames(df) == "X7"] <- "Hedge Units Delivered"
    colnames(df) <- gsub("\\.", " ", colnames(df))
    
    df[df == ""] <- NA #change any empty values with NA 
    numberrows <- sum(!is.na(df[[2]])) #number of non NA vals
    df <- df[1:numberrows, ] #chop dataframe beyond numberrows
    df <- df[rowSums(is.na(df)) < ncol(df), ] #remove any straggling rows which are fully NA values
    
    
    #first thing that needs to be checked, is if there are any NAs in any column 
    for (i in 1:ncol(df)) { #for each column in the dataframe
      
      if (any(is.na(df[[i]]))) { #if ANY have NA, return a message - can assign this to a message object, to pass back to user
        errormessages<- paste(errormessages, "Column '", names(df)[i], "' contains NA values.\n", sep = "")
        
      }
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
#' @examples \dontrun{checked_dataset<-check_B3(metric)}   
check_B3 <- function(metric) {
  
  #read the dataset
  df <- openxlsx::read.xlsx(metric, "B-3 On-Site Hedge Enhancement", cols = c(2,3,7,16,17,19,21,34), colNames = TRUE, startRow = 11)
  
  errormessages <- c()
  
  if(!is.na(df$Baseline.habitat[1])) {
    colnames(df)[colnames(df) == "X4"] <- "Length (km)"
    colnames(df)[colnames(df) == "X8"] <- "Hedge Units Delivered"
    colnames(df) <- gsub("\\.", " ", colnames(df))
    
    df[df == ""] <- NA #change any empty values with NA 
    numberrows <- sum(!is.na(df[[2]])) #number of non NA vals
    df <- df[1:numberrows, ] #chop dataframe beyond numberrows
    df <- df[rowSums(is.na(df)) < ncol(df), ] #remove any straggling rows which are fully NA values
    
    
    #first thing that needs to be checked, is if there are any NAs in any column 
    for (i in 1:ncol(df)) { #for each column in the dataframe
      
      if (any(is.na(df[[i]]))) { #if ANY have NA, return a message - can assign this to a message object, to pass back to user
        errormessages<- paste(errormessages, "Column '", names(df)[i], "' contains NA values.\n", sep = "")
        
      }
    }
    
    
    
  }
  return(errormessages)
  
}

#' cleans C1 water metric data 
#'
#' @param metric feas metric
#'
#' @return df, a cleaned version of the input dataset - to be used as an argument in actual metric pulling function
#' @export
#'
#' @examples \dontrun{checked_dataset<-check_C1(metric)}  
check_C1 <- function(metric) {
  
  #read the dataset
  df <- openxlsx::read.xlsx(metric, "C-1 On-Site WaterC' Baseline", cols = c(4,5,6,8,10,23,24,25,26), colNames = TRUE, startRow = 9)
  
  errormessages <- c()
  if(!is.na(df$Watercourse.type[1])) {
    
  colnames(df) <- gsub("\\.", " ", colnames(df))

  #row nrow of the SECOND col with data in it, which is the last lines of the habitats, giving nrow for rest 
  df[df == ""] <- NA #change any empty values with NA 
  numberrows <- sum(!is.na(df[[1]])) #number of non NA vals
  df <- df[1:numberrows, ] #chop dataframe beyond numberrows
  df <- df[rowSums(is.na(df)) < ncol(df), ] #remove any straggling rows which are fully NA values
  
  
  #first thing that needs to be checked, is if there are any NAs in any column 
  for (i in 1:ncol(df)) { #for each column in the dataframe
    
    if (any(is.na(df[[i]]))) { #if ANY have NA, return a message - can assign this to a message object, to pass back to user
      errormessages<- paste(errormessages, "Column '", names(df)[i], "' contains NA values.\n", sep = "")
      
    }
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
#' @examples \dontrun{checked_dataset<-check_C2(metric)}    
check_C2 <- function(metric) {
  
  #read the dataset
  df <- openxlsx::read.xlsx(metric, "C-2 On-Site WaterC' Creation", cols = c(3,4,5,7,9,26,29), colNames = TRUE, startRow = 11)
  
  errormessages <- c()
  if(!is.na(df$Watercourse.type[1])) {
    
  colnames(df) <- gsub("\\.", " ", colnames(df))
  
  colnames(df)[colnames(df) == "X6"] <- "Watercourse units delivered"
  #row nrow of the SECOND col with data in it, which is the last lines of the habitats, giving nrow for rest 
  df[df == ""] <- NA #change any empty values with NA 
  numberrows <- sum(!is.na(df[[2]])) #number of non NA vals
  df <- df[1:numberrows, ] #chop dataframe beyond numberrows
  df <- df[rowSums(is.na(df)) < ncol(df), ] #remove any straggling rows which are fully NA values
  
  
  #first thing that needs to be checked, is if there are any NAs in any column 
  for (i in 1:ncol(df)) { #for each column in the dataframe
    
    if (any(is.na(df[[i]]))) { #if ANY have NA, return a message - can assign this to a message object, to pass back to user
      errormessages<- paste(errormessages, "Column '", names(df)[i], "' contains NA values.\n", sep = "")
      
    }
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
#' @examples \dontrun{checked_dataset<-check_C3(metric)}    
check_C3 <- function(metric) {
  
  #read the dataset
  df <- openxlsx::read.xlsx(metric, "C-3 On-Site WaterC' Enhancement", cols = c(3,7,14,17,18,20,22,39), colNames = TRUE, startRow = 11)
  
  errormessages <- c()
  
  if(!is.na(df$Baseline.habitat[1])) {
    
  colnames(df)[colnames(df) == "X3"] <- "Proposed Habitats"
  colnames(df)[colnames(df) == "X7"] <- "Strategic Significance"
  colnames(df) <- gsub("\\.", " ", colnames(df))
  
  #row nrow of the SECOND col with data in it, which is the last lines of the habitats, giving nrow for rest 
  df[df == ""] <- NA #change any empty values with NA 
  numberrows <- sum(!is.na(df[[2]])) #number of non NA vals
  df <- df[1:numberrows, ] #chop dataframe beyond numberrows
  df <- df[rowSums(is.na(df)) < ncol(df), ] #remove any straggling rows which are fully NA values
  
  
  #first thing that needs to be checked, is if there are any NAs in any column 
  for (i in 1:ncol(df)) { #for each column in the dataframe
    
    if (any(is.na(df[[i]]))) { #if ANY have NA, return a message - can assign this to a message object, to pass back to user
      errormessages<- paste(errormessages, "Column '", names(df)[i], "' contains NA values.\n", sep = "")
      
    }
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
#' @export
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
#' @export
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
#' @export
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