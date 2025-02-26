#' pull data regarding the net unit gains and net percentage gains, as well as if loss present
#'
#' @param metric feas metric
#'
#' @return netdata, a list containing summarising data about the site, including potential net BNG units
#' gained, BNGUnits, and the net bng percentage increase, BNGPercentage, and Hectares, the size of the site.
#' @export
#'
#' @examples \dontrun{netdata<-pullonsitenetdata(metric)}
pullonsitenetdata<-function(metric){
  resultssheet<-"Headline Results"
    
    #get on site baseline for habitat, hedgerow, and watercourse units
    BaseHabUnits<-openxlsx::read.xlsx(metric, sheet = resultssheet, cols = 8, rows = 8, colNames = FALSE, skipEmptyRows = TRUE)
    colnames(BaseHabUnits) <- "BaseUnits"
    BaseHedgeUnits<-openxlsx::read.xlsx(metric, sheet = resultssheet, cols = 8, rows = 9, colNames = FALSE, skipEmptyRows = TRUE)
    colnames(BaseHedgeUnits) <- "BaseUnits"
    BaseWaterUnits<-openxlsx::read.xlsx(metric, sheet = resultssheet, cols = 8, rows = 10, colNames = FALSE, skipEmptyRows = TRUE)
    colnames(BaseWaterUnits) <- "BaseUnits"
    
    #get on site pi for habitat, hedgerow, and watercourse units
    PIHabUnits<-openxlsx::read.xlsx(metric, sheet = resultssheet, cols = 8, rows = 12, colNames = FALSE, skipEmptyRows = TRUE)
    colnames(PIHabUnits) <- "PIUnits"
    PIHedgeUnits<-openxlsx::read.xlsx(metric, sheet = resultssheet, cols = 8, rows = 13, colNames = FALSE, skipEmptyRows = TRUE)
    colnames(PIHedgeUnits) <- "PIUnits"
    PIWaterUnits<-openxlsx::read.xlsx(metric, sheet = resultssheet, cols = 8, rows = 14, colNames = FALSE, skipEmptyRows = TRUE)
    colnames(PIWaterUnits) <- "PIUnits"
    
    #get on site net change for habitat, hedgerow, and watercourse units
    NetHabUnits <- openxlsx::read.xlsx(metric, sheet = resultssheet, cols = 8, rows = 16, colNames = FALSE, skipEmptyRows = TRUE)
    colnames(NetHabUnits) <- "NetUnits"
    NetHedgeUnits<- openxlsx::read.xlsx(metric, sheet = resultssheet, cols = 8, rows = 17, colNames = FALSE, skipEmptyRows = TRUE)
    colnames(NetHedgeUnits) <- "NetUnits"
    NetWaterUnits<- openxlsx::read.xlsx(metric, sheet = resultssheet, cols = 8, rows = 18, colNames = FALSE, skipEmptyRows = TRUE)
    colnames(NetWaterUnits) <- "NetUnits"
    
    #get on site net percentage change for habitat
    NetHabPercent <- openxlsx::read.xlsx(metric, sheet = resultssheet, cols = 10, rows = 16, colNames = FALSE, skipEmptyRows = TRUE)
    
    if (is.numeric(NetHabPercent$X1)){
      
      NetHabPercent <-round((NetHabPercent * 100), digits = 2)
      colnames(NetHabPercent) <- "NetPercent"
      
    } else {NetHabPercent <- NA}
    
    #get on site net percentage change for hedgerow
    NetHedgePercent <- openxlsx::read.xlsx(metric, sheet = resultssheet, cols = 10, rows = 17, colNames = FALSE, skipEmptyRows = TRUE)
    
    if (is.numeric(NetHedgePercent$X1)){
      
      NetHedgePercent <-round((NetHedgePercent * 100), digits = 2)
      colnames(NetHedgePercent) <- "NetPercent"
      
    } else {NetHedgePercent <- NA}
    
    #get on site net percentage change for watercourse units
    NetWaterPercent<- openxlsx::read.xlsx(metric, sheet = resultssheet, cols = 10, rows = 18, colNames = FALSE, skipEmptyRows = TRUE)
    
    if (is.numeric(NetWaterPercent$X1)){
      
      NetWaterPercent <-round((NetWaterPercent * 100), digits = 2)
      colnames(NetWaterPercent) <- "NetPercent"
      
    } else {NetWaterPercent <- NA}
    
    #get if trading standards satisfied
    TradeSatisfied <- openxlsx::read.xlsx(metric, sheet = resultssheet, cols = 6, rows = 55, colNames = FALSE, skipEmptyRows = TRUE)
    if ("Yes ✓" %in% TradeSatisfied){
      TradeSatisfied <- "Yes"
    } else {
      TradeSatisfied <- "No"
    }
    
    #unit deficit
    HabDeficit <- openxlsx::read.xlsx(metric, sheet = resultssheet, cols = 8, rows = 61, colNames = FALSE, skipEmptyRows = TRUE)
    #HabDeficit <-round((HabDeficit * 100), digits = 2)
    colnames(HabDeficit) <- "Deficit"
    HedgeDeficit <- openxlsx::read.xlsx(metric, sheet = resultssheet, cols = 8, rows = 62, colNames = FALSE, skipEmptyRows = TRUE)
    #HedgeDeficit <-round((HedgeDeficit * 100), digits = 2)
    colnames(HedgeDeficit) <- "Deficit"
    WaterDeficit <- openxlsx::read.xlsx(metric, sheet = resultssheet, cols = 8, rows = 63, colNames = FALSE, skipEmptyRows = TRUE)
    #WaterDeficit <-round((WaterDeficit * 100), digits = 2)
    colnames(WaterDeficit) <- "Deficit"
    
    nethabitat<-data.frame(Type = "Area-based",
                           BaseUnits = BaseHabUnits,
                           PIUnits = PIHabUnits,
                           NetUnits = NetHabUnits, 
                           NetPercent = NetHabPercent,
                           Deficit = HabDeficit)
    
    nethedge<-data.frame(Type = "Hedgerow",
                         BaseUnits = BaseHedgeUnits,
                         PIUnits = PIHedgeUnits,
                         NetUnits = NetHedgeUnits, 
                         NetPercent = NetHedgePercent,
                         Deficit = HedgeDeficit)
    
    netwater<-data.frame(Type = "Watercourse",
                         BaseUnits = BaseWaterUnits,
                         PIUnits = PIWaterUnits,
                         NetUnits = NetWaterUnits,
                         NetPercent = NetWaterPercent,
                         Deficit = WaterDeficit)
    
    netdata<-rbind(nethabitat, nethedge, netwater)
    
    netdata<-list(NetData = netdata,
                  TradeSatisfied = TradeSatisfied)
    
    return(netdata)
}