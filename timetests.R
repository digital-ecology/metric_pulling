##### figure out where data dir is ####
if (.Platform$OS.type == "windows") {
  data_dir <- file.path(Sys.getenv("USERPROFILE"), "Nextcloud", "Shared", "ETHOSData", "Metrics")
} else {
  data_dir <- file.path(Sys.getenv("HOME"), "Nextcloud", "Shared", "ETHOSData", "Metrics")
}

data_dir <- normalizePath(data_dir, winslash = "/", mustWork = TRUE)

#filepaths <- list.files(path = data_dir, full.names = TRUE)

#for (filepath in filepaths) {
  
  metric <- file.path(data_dir, "WingRoad.xlsx")
  
  cat(paste("Metric:", metric))
  
oldcodetime <- system.time({
  habbaseline <- pullonsitehabitatbaseline(metric)
  habretain<-pullonsitehabitatretain(metric)
  habloss<- pullonsitehabitatloss(metric)
  habcreation<- pullonsitehabitatcreation(metric)
  habenhancement<- pullonsitehabitatenhancement(metric)
})

newcodetime <- system.time({
  habbaseline <- pullA1baseline(metric)
  habretain <- pullA1retain(metric)
  habloss <- pullA1loss(metric)
  habcreation<-pullA2(metric)
  habenhancement <- pullA3(metric)
})

print(oldcodetime)

print(newcodetime) 