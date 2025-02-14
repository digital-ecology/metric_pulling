# EXAMPLE OF HOW TO REMOVE READING IN METRIC FOR EACH METRIC PULLING FUNCTION
pullonsitehabitatsumdata<-function(metric){
  
  #get which distinctiveness is needed for satisfying trading standards
  tradingsum <- "Trading Summary Area Habitats"
  
  
  # SINGLE CALL OF METRIC
  full_data <- openxlsx::read.xlsx(metric, sheet = tradingsum, colNames = FALSE, skipEmptyRows = TRUE)
  
  
  # READING IN COLS FROM WORKSHEET
  Satisfied <- full_data[5:8, 7, drop = FALSE]
  Distinctiveness <- full_data[5:8, 2, drop = FALSE]
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
  VHHab <- full_data[13:32, 2, drop = FALSE]
  VHChange <- full_data[13:32, 6, drop = FALSE]
  VH <- cbind(VHHab, VHChange)
  colnames(VH) <- c("HabitatGroup", "ProjectWideUnitChange")
  
  #high
  HHab <- full_data[41:82, 2, drop = FALSE]
  HChange <- full_data[41:82, 6, drop = FALSE]
  H <- cbind(HHab, HChange)
  colnames(H) <- c("HabitatGroup", "ProjectWideUnitChange")
  
  #medium
  MHab <- full_data[89:115, 2, drop = FALSE]
  MChange <- full_data[89:115, 6, drop = FALSE]
  M <- cbind(MHab, MChange)
  colnames(M) <- c("HabitatGroup", "ProjectWideUnitChange")
  
  #low
  LHab <- full_data[125:162, 2, drop = FALSE]
  LChange <- full_data[125:162, 6, drop = FALSE]
  L <- cbind(LHab, LChange)
  colnames(L) <- c("HabitatGroup", "ProjectWideUnitChange")
  
  sumdata<-list(TradingSatisfied = Satisfied,
                VHNet = VH,
                HNet = H,
                MNet = M,
                LNet = L)
  
  return(sumdata)
  
}