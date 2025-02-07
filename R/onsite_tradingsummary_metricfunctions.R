#' pull metadata needed, including site size, net units gained, net percentage gain, etc.
#'
#' @param metric feas metric
#'
#' @return sumdata, a list containing summarising data about the site, including potential net BNG units
#' gained, BNGUnits, and the net bng percentage increase, BNGPercentage, and Hectares, the size of the site.
#' @export
#'
#' @examples \dontrun{habitatsumdata<-pullonsitehabitatsumdata(metric)}
pullonsitehabitatsumdata<-function(metric){
  catch_habsum <-   clean_habsum_dataset(metric)

  if (is.data.frame(catch_habsum)) {
      
    #get which distinctiveness is needed for satisfying trading standards
    tradingsum <- "Trading Summary Area Habitats"
    
    Satisfied <- openxlsx::read.xlsx(ds2, sheet = tradingsum, cols = 7, rows = 5:8, colNames = FALSE, skipEmptyRows = TRUE)
    Distinctiveness <- openxlsx::read.xlsx(ds2, sheet = tradingsum, cols = 2, rows = 5:8, colNames = FALSE, skipEmptyRows = TRUE)
    Satisfied <- cbind(Distinctiveness, Satisfied)
    colnames(Satisfied) <- c("Distinctiveness", "Satisfied")
    
    for (i in 1:nrow(Satisfied)) {
      
      if ("Yes ✓" %in% Satisfied$Satisfied[i]){
        Satisfied$Satisfied[i] <- "Yes"
      } else {
        Satisfied$Satisfied[i] <- "No"
      }
      
    }
    
    #get dfs for each distinctiveness, showing hab group and project wide unit change
    
    #very high
    VHHab <- openxlsx::read.xlsx(ds2, sheet = tradingsum, cols = 2, rows = 13:32, colNames = FALSE, skipEmptyRows = TRUE)
    VHChange <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 6, rows = 13:32, colNames = FALSE, skipEmptyRows = TRUE)
    VH <- cbind(VHHab, VHChange)
    colnames(VH) <- c("HabitatGroup", "ProjectWideUnitChange")
    
    #high
    HHab <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 2, rows = 41:82, colNames = FALSE, skipEmptyRows = TRUE)
    HChange <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 6, rows = 41:82, colNames = FALSE, skipEmptyRows = TRUE)
    H <- cbind(HHab, HChange)
    colnames(H) <- c("HabitatGroup", "ProjectWideUnitChange")
    
    #medium
    MHab <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 2, rows = 89:115, colNames = FALSE, skipEmptyRows = TRUE)
    MChange <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 6, rows = 89:115, colNames = FALSE, skipEmptyRows = TRUE)
    M <- cbind(MHab, MChange)
    colnames(M) <- c("HabitatGroup", "ProjectWideUnitChange")
    
    #low
    LHab <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 2, rows = 125:162, colNames = FALSE, skipEmptyRows = TRUE)
    LChange <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 6, rows = 125:162, colNames = FALSE, skipEmptyRows = TRUE)
    L <- cbind(LHab, LChange)
    colnames(L) <- c("HabitatGroup", "ProjectWideUnitChange")
    
    sumdata<-list(TradingSatisfied = Satisfied,
                  VHNet = VH,
                  HNet = H,
                  MNet = M,
                  LNet = L)
    
    return(sumdata)
  }
  else {
    return(catch_habsum)
  }
  
}

#' pull metadata needed, including site size, net units gained, net percentage gain, etc.
#'
#' @param metric feas metric
#'
#' @return sumdata, a list containing summarising data about the site, including potential net BNG units
#' gained, BNGUnits, and the net bng percentage increase, BNGPercentage, and Hectares, the size of the site.
#' @export
#'
#' @examples \dontrun{hedgesumdata<-pullonsitehedgesumdata(metric)}
pullonsitehedgesumdata<-function(metric){
  catch_hedgesum <-   clean_hedgesum_dataset(metric)
  
  if (is.data.frame(catch_hedgesum)) {
    #get which distinctiveness is needed for satisfying trading standards
    tradingsum <- "Trading Summary Hedgerows"
    
    Satisfied <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 6, rows = 5:9, colNames = FALSE, skipEmptyRows = TRUE)
    Distinctiveness <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 2, rows = 5:9, colNames = FALSE, skipEmptyRows = TRUE)
    Satisfied <- cbind(Distinctiveness, Satisfied)
    colnames(Satisfied) <- c("Distinctiveness", "Satisfied")
    
    for (i in 1:nrow(Satisfied)) {
      
      if ("Yes ✓" %in% Satisfied$Satisfied[i]){
        Satisfied$Satisfied[i] <- "Yes"
      } else {
        Satisfied$Satisfied[i] <- "No"
      }
      
    }
    
    #get dfs for each distinctiveness, showing hab group and project wide unit change
    
    #very high
    VHHab <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 2, rows = 14, colNames = FALSE, skipEmptyRows = TRUE)
    VHChange <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 5, rows = 14, colNames = FALSE, skipEmptyRows = TRUE)
    VH <- cbind(VHHab, VHChange)
    colnames(VH) <- c("HabitatGroup", "ProjectWideUnitChange")
    
    #high
    HHab <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 2, rows = 22:24, colNames = FALSE, skipEmptyRows = TRUE)
    HChange <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 5, rows = 22:24, colNames = FALSE, skipEmptyRows = TRUE)
    H <- cbind(HHab, HChange)
    colnames(H) <- c("HabitatGroup", "ProjectWideUnitChange")
    
    #medium
    MHab <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 2, rows = 32:36, colNames = FALSE, skipEmptyRows = TRUE)
    MChange <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 5, rows = 32:36, colNames = FALSE, skipEmptyRows = TRUE)
    M <- cbind(MHab, MChange)
    colnames(M) <- c("HabitatGroup", "ProjectWideUnitChange")
    
    #low
    LHab <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 2, rows = 44:46, colNames = FALSE, skipEmptyRows = TRUE)
    LChange <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 5, rows = 44:46, colNames = FALSE, skipEmptyRows = TRUE)
    L <- cbind(LHab, LChange)
    colnames(L) <- c("HabitatGroup", "ProjectWideUnitChange")
    
    #vlow
    VLHab <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 2, rows = 54, colNames = FALSE, skipEmptyRows = TRUE)
    VLChange <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 5, rows = 54, colNames = FALSE, skipEmptyRows = TRUE)
    VL <- cbind(VLHab, VLChange)
    colnames(VL) <- c("HabitatGroup", "ProjectWideUnitChange")
    
    sumdata<-list(TradingSatisfied = Satisfied,
                  VHNet = VH,
                  HNet = H,
                  MNet = M,
                  LNet = L,
                  VLNet = VL)
    
    return(sumdata)}
  else {
    return(catch_hedgesum)
  }
    
}

#' pull metadata needed, including site size, net units gained, net percentage gain, etc.
#'
#' @param metric feas metric
#'
#' @return sumdata, a list containing summarising data about the site, including potential net BNG units
#' gained, BNGUnits, and the net bng percentage increase, BNGPercentage, and Hectares, the size of the site.
#' @export
#'
#' @examples \dontrun{watersumdata<-pullonsitewatersumdata(metric)}
pullonsitewatersumdata<-function(metric){
  catch_watersum <-   clean_watersum_dataset(metric)
  
  if (is.data.frame(catch_watersum)) {
  #get which distinctiveness is needed for satisfying trading standards
    tradingsum <- "Trading Summary WaterC's"
    
    Satisfied <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 7, rows = 4:9, colNames = FALSE, skipEmptyRows = TRUE)
    Distinctiveness <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 2, rows = 4:9, colNames = FALSE, skipEmptyRows = TRUE)
    Satisfied <- cbind(Distinctiveness, Satisfied)
    colnames(Satisfied) <- c("Distinctiveness", "Satisfied")
    
    for (i in 1:nrow(Satisfied)) {
      
      if ("Yes ✓" %in% Satisfied$Satisfied[i]){
        Satisfied$Satisfied[i] <- "Yes"
      } else {
        Satisfied$Satisfied[i] <- "No"
      }
      
    }
    
    #get dfs for each distinctiveness, showing hab group and project wide unit change
    
    #very high
    VHHab <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 2, rows = 13, colNames = FALSE, skipEmptyRows = TRUE)
    VHChange <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 5, rows = 13, colNames = FALSE, skipEmptyRows = TRUE)
    VH <- cbind(VHHab, VHChange)
    colnames(VH) <- c("HabitatGroup", "ProjectWideUnitChange")
    
    #high
    HHab <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 2, rows = 22, colNames = FALSE, skipEmptyRows = TRUE)
    HChange <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 5, rows = 22, colNames = FALSE, skipEmptyRows = TRUE)
    H <- cbind(HHab, HChange)
    colnames(H) <- c("HabitatGroup", "ProjectWideUnitChange")
    
    #medium
    MHab <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 2, rows = 30:31, colNames = FALSE, skipEmptyRows = TRUE)
    MChange <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 5, rows = 30:31, colNames = FALSE, skipEmptyRows = TRUE)
    M <- cbind(MHab, MChange)
    colnames(M) <- c("HabitatGroup", "ProjectWideUnitChange")
    
    #low
    LHab <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 2, rows = 42, colNames = FALSE, skipEmptyRows = TRUE)
    LChange <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 5, rows = 42, colNames = FALSE, skipEmptyRows = TRUE)
    L <- cbind(LHab, LChange)
    colnames(L) <- c("HabitatGroup", "ProjectWideUnitChange")
    
    # #vlow
    # VLHab <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 2, rows = 54, colNames = FALSE, skipEmptyRows = TRUE)
    # VLChange <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 5, rows = 54, colNames = FALSE, skipEmptyRows = TRUE)
    # VL <- cbind(VLHab, VLChange)
    # colnames(VL) <- c("HabitatGroup", "ProjectWideUnitChange")
    
    sumdata<-list(TradingSatisfied = Satisfied,
                  VHNet = VH,
                  HNet = H,
                  MNet = M,
                  LNet = L)
    
    return(sumdata)}
  else {
      return(catch_watersum)
      
      }
}
  

