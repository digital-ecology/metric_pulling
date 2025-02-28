##### figure out where data dir is ####
if (.Platform$OS.type == "windows") {
  data_dir <- file.path(Sys.getenv("USERPROFILE"), "Nextcloud", "Shared", "ETHOSData") 
} else {
  data_dir <- file.path(Sys.getenv("HOME"), "Nextcloud", "Shared", "ETHOSData")
}

# read in the .gdb
data_dir <- normalizePath(data_dir, winslash = "/", mustWork = TRUE)

##### simulate selection of sitename and subsetting of data ####

#sitenames list

#sitename <- "Woburn maintanence yard"
#sitename <- "Land South of Snarlton Farm, Melksham"
#sitename <- "Land South of Wing Road Stewkley"
#sitename <- "Barnett Lane, Elstree"
#sitename <- "Locksbrook Road South"
#sitename <- "Copenacre, Corsham" 
#sitename <- "Swindon J16" 
#sitename <- "Little Mill, Farringdon" 
#sitename <- "The Vale " 

sitenames <- c("Woburn maintanence yard", "Land South of Snarlton Farm, Melksham", "Land South of Wing Road Stewkley",
               "Barnett Lane, Elstree", "Locksbrook Road South", "Copenacre, Corsham", "Swindon J16", "Little Mill, Farringdon",
               "The Vale ")


for (sitename in sitenames){
  
  sitename <- sitename
  
  #### pull metric based on site name ####
  
  if (sitename == "Woburn maintanence yard"){metric <- file.path(data_dir, "Metrics", "Woburn.xlsx")}
  if (sitename == "Land South of Snarlton Farm, Melksham"){metric <- file.path(data_dir, "Metrics", "SnarltonFarm.xlsx")}
  if (sitename == "Land South of Wing Road Stewkley"){metric <- file.path(data_dir, "Metrics", "WingRoad.xlsx")}
  if (sitename == "Barnett Lane, Elstree"){metric <- file.path(data_dir, "Metrics", "Barnet Lane Elstree Statutory Metric 30.01.25.xlsx")}
  if (sitename == "Locksbrook Road South"){metric <- file.path(data_dir, "Metrics", "Locksbrook Road South  Statutory Biodiversity Metric V3 - 01 11 24.xlsx")}
  if (sitename == "Little Mill, Farringdon"){metric <- file.path(data_dir, "Metrics", "Little Farringdon Mill Metric V1.xlsx")}
  if (sitename == "Swindon J16"){metric <- file.path(data_dir, "Metrics", "Swindon J16 Statutory Biodiversity Metric V4.xlsx")}
  if (sitename == "The Vale"){metric <- file.path(data_dir, "Metrics", "The Vale Statutory Metric 24.01.25.xlsx")}
  if (sitename == "Copenacre, Corsham"){metric <- file.path(data_dir, "Metrics", "CopenacerERROR.xlsx")}
  
  cat(paste("Site:", sitename, "\n\n", metric_check(metric),"\n\n"))
  
}
