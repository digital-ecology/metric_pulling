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
    #get which distinctiveness is needed for satisfying trading standards
    tradingsum <- "Trading Summary Area Habitats"
    
    Satisfied <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 7, rows = 5:8, colNames = FALSE, skipEmptyRows = TRUE)
    Distinctiveness <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 2, rows = 5:8, colNames = FALSE, skipEmptyRows = TRUE)
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
    VHHab <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 2, rows = 13:32, colNames = FALSE, skipEmptyRows = TRUE)
    VHGroup <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 3, rows = 13:32, colNames = FALSE, skipEmptyRows = TRUE)
    VHChange <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 6, rows = 13:32, colNames = FALSE, skipEmptyRows = TRUE)
    VHOffset <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 7, rows = 13:32, colNames = FALSE, skipEmptyRows = TRUE)
    
    if (is.null(VHOffset)) {
      VHOffset <- data.frame(X1= rep(NA, 20))
    }
    
    VH <- cbind(VHHab, VHGroup, VHChange, VHOffset)
    colnames(VH) <- c("HabitatGroup", "Group", "ProjectWideUnitChange", "Offset")
    
    #high
    HHab <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 2, rows = 41:82, colNames = FALSE, skipEmptyRows = TRUE)
    HGroup <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 3, rows = 41:82, colNames = FALSE, skipEmptyRows = TRUE)
    HChange <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 6, rows = 41:82, colNames = FALSE, skipEmptyRows = TRUE)
    
    HOffset <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 7, rows = 41:82, colNames = FALSE, skipEmptyRows = TRUE)
    
    if (is.null(HOffset)) {
      HOffset <- data.frame(X1= rep(NA, 42))
    }
    
    H <- cbind(HHab, HGroup, HChange, HOffset)
    colnames(H) <- c("HabitatGroup", "Group", "ProjectWideUnitChange", "Offset")
    
    #medium
    MHab <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 2, rows = 89:115, colNames = FALSE, skipEmptyRows = TRUE)
    MGroup <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 3, rows = 89:115, colNames = FALSE, skipEmptyRows = TRUE)
    MChange <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 6, rows = 89:115, colNames = FALSE, skipEmptyRows = TRUE)
    
    MOffset <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 7, rows = 89:115, colNames = FALSE, skipEmptyRows = TRUE)
    
    if (is.null(MOffset)) {
      MOffset <- data.frame(X1= rep(NA, 9))
    }
    
    #annoyingly, 10 groups, 9 broad chanes, cus last two are in one but diff names
    Groups<-unique(MGroup)
    Groups$X1[nrow(Groups) - 1] <- paste0(tail(Groups$X1, 2), collapse = " and ")
    Groups <- head(Groups, -1)
    
    #now make broad change df too, so can explain what is lost by type
    MGroupChange <- data.frame(HabitatGroup = Groups$X1,
                              Deficit = MOffset$X1)
    
    #now rename those groups in the dataframe, so that the code can run properly
    MGroup$X1[25:27] <- Groups$X1[9]
    
    M <- cbind(MHab, MGroup, MChange)
    colnames(M) <- c("HabitatGroup", "Group", "ProjectWideUnitChange")
    
    #low
    LHab <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 2, rows = 125:162, colNames = FALSE, skipEmptyRows = TRUE)
    LGroup <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 3, rows = 125:162, colNames = FALSE, skipEmptyRows = TRUE)
    LChange <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 6, rows = 125:162, colNames = FALSE, skipEmptyRows = TRUE)
    LOffset <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 7, rows = 125:162, colNames = FALSE, skipEmptyRows = TRUE)
    
    if (is.null(LOffset)) {
      LOffset <- data.frame(X1= rep(NA, 38))
    }
    
    L <- cbind(LHab, LGroup, LChange, LOffset)
    colnames(L) <- c("HabitatGroup", "Group", "ProjectWideUnitChange", "Offset")
    
    LSurplus <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 11, rows = 125, colNames = FALSE, skipEmptyRows = TRUE)
    MSurplus <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 11, rows = 90, colNames = FALSE, skipEmptyRows = TRUE)
    HSurplus <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 11, rows = 41, colNames = FALSE, skipEmptyRows = TRUE)
    VHSurplus <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 11, rows = 13, colNames = FALSE, skipEmptyRows = TRUE)
    
    Surplus<-rbind(LSurplus, MSurplus, HSurplus, VHSurplus)
    colnames(Surplus) <- "Surplus"
    Surplus$Type <- c("LNet", "MNet", "HNet", "VHNet")
    
    sumdata<-list(TradingSatisfied = Satisfied,
                  Surplus = Surplus,
                  MGroupChange = MGroupChange,
                  VHNet = VH,
                  HNet = H,
                  MNet = M,
                  LNet = L)
    
    return(sumdata)
  
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
    
    VLSurplus <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 9, rows = 54, colNames = FALSE, skipEmptyRows = TRUE)
    LSurplus <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 9, rows = 44, colNames = FALSE, skipEmptyRows = TRUE)
    MSurplus <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 9, rows = 33, colNames = FALSE, skipEmptyRows = TRUE)
    HSurplus <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 9, rows = 23, colNames = FALSE, skipEmptyRows = TRUE)
    VHSurplus <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 9, rows = 14, colNames = FALSE, skipEmptyRows = TRUE)
    
    Surplus<-rbind(VLSurplus, LSurplus, MSurplus, HSurplus, VHSurplus)
    colnames(Surplus) <- "Surplus"
    Surplus$Type <- c("VLNet","LNet", "MNet", "HNet", "VHNet")
    
    sumdata<-list(TradingSatisfied = Satisfied,
                  Surplus = Surplus,
                  VHNet = VH,
                  HNet = H,
                  MNet = M,
                  LNet = L,
                  VLNet = VL)
    
    return(sumdata)
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
    
    LSurplus <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 9, rows = 42, colNames = FALSE, skipEmptyRows = TRUE)
    MSurplus <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 9, rows = 30, colNames = FALSE, skipEmptyRows = TRUE)
    HSurplus <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 9, rows = 22, colNames = FALSE, skipEmptyRows = TRUE)
    VHSurplus <- openxlsx::read.xlsx(metric, sheet = tradingsum, cols = 9, rows = 13, colNames = FALSE, skipEmptyRows = TRUE)
    
    Surplus<-rbind(LSurplus, MSurplus, HSurplus, VHSurplus)
    colnames(Surplus) <- "Surplus"
    Surplus$Type <- c("LNet", "MNet", "HNet", "VHNet")
    
    sumdata<-list(TradingSatisfied = Satisfied,
                  Surplus = Surplus,
                  VHNet = VH,
                  HNet = H,
                  MNet = M,
                  LNet = L)
    
    return(sumdata)
}
  

