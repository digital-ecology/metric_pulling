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
  
  #get on site net change for habitat, hedgerow, and watercourse units
  NetHabUnits <- openxlsx::read.xlsx(metric, sheet = resultssheet, cols = 8, rows = 47, colNames = FALSE, skipEmptyRows = TRUE)
  colnames(NetHabUnits) <- "NetHabUnits"
  NetHedgeUnits<- openxlsx::read.xlsx(metric, sheet = resultssheet, cols = 8, rows = 48, colNames = FALSE, skipEmptyRows = TRUE)
  colnames(NetHedgeUnits) <- "NetHedgeUnits"
  NetWaterUnits<- openxlsx::read.xlsx(metric, sheet = resultssheet, cols = 8, rows = 49, colNames = FALSE, skipEmptyRows = TRUE)
  colnames(NetWaterUnits) <- "NetWaterUnits"
  
  #get on site net percentage change for habitat, hedgerow, and watercourse units
  NetHabPercent <- openxlsx::read.xlsx(metric, sheet = resultssheet, cols = 8, rows = 51, colNames = FALSE, skipEmptyRows = TRUE)
  NetHabPercent <-round((NetHabPercent * 100), digits = 2)
  colnames(NetHabPercent) <- "NetHabPercent"
  NetHedgePercent<- openxlsx::read.xlsx(metric, sheet = resultssheet, cols = 8, rows = 52, colNames = FALSE, skipEmptyRows = TRUE)
  NetHedgePercent <-round((NetHedgePercent * 100), digits = 2)
  colnames(NetHedgePercent) <- "NetHedgePercent"
  NetWaterPercent<- openxlsx::read.xlsx(metric, sheet = resultssheet, cols = 8, rows = 53, colNames = FALSE, skipEmptyRows = TRUE)
  NetWaterPercent <-round((NetWaterPercent * 100), digits = 2)
  colnames(NetWaterPercent) <- "NetWaterPercent"
  
  #get if trading standards satisfied
  TradeSatisfied <- openxlsx::read.xlsx(metric, sheet = resultssheet, cols = 6, rows = 55, colNames = FALSE, skipEmptyRows = TRUE)
  if ("Yes ✓" %in% TradeSatisfied){
    TradeSatisfied <- "Yes"
  } else {
    TradeSatisfied <- "No"
  }
  
  #unit deficit
  HabDeficit <- openxlsx::read.xlsx(metric, sheet = resultssheet, cols = 8, rows = 61, colNames = FALSE, skipEmptyRows = TRUE)
  colnames(HabDeficit) <- "HabitatDeficit"
  HedgeDeficit <- openxlsx::read.xlsx(metric, sheet = resultssheet, cols = 8, rows = 62, colNames = FALSE, skipEmptyRows = TRUE)
  colnames(HedgeDeficit) <- "HedgeDeficit"
  WaterDeficit <- openxlsx::read.xlsx(metric, sheet = resultssheet, cols = 8, rows = 63, colNames = FALSE, skipEmptyRows = TRUE)
  colnames(WaterDeficit) <- "WaterDeficit"
  
  netdata<-list(NetHabitatUnits = NetHabUnits,
                NetHabitatPercent = NetHabPercent,
                HabitatDeficit = HabDeficit,
                NetHedgerowUnits = NetHedgeUnits,
                NetHedgerowPercent = NetHedgePercent,
                HedgerowDeficit = HedgeDeficit,
                NetWaterUnits = NetWaterUnits,
                NetWaterPercent = NetWaterPercent,
                WaterDeficit = WaterDeficit,
                TradeSatisfied = TradeSatisfied)
  
  return(netdata)
  
}