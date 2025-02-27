if (.Platform$OS.type == "windows") {
  data_dir <- file.path(Sys.getenv("USERPROFILE"), "Nextcloud", "Shared", "ETHOSData")
} else {
  data_dir <- file.path(Sys.getenv("HOME"), "Nextcloud", "Shared", "ETHOSData")
}
# read in the .gdb
data_dir <- normalizePath(data_dir, winslash = "/", mustWork = TRUE)

##### simulate selection of sitename and subsetting of data ####

#these render in app perfectly fine

sitename <- "Woburn maintanence yard"
#sitename <- "Land South of Snarlton Farm, Melksham"
#sitename <- "Land South of Wing Road Stewkley"

#these render in app perfectly fine, but with mistakes - maddie is currently checking to see if the reports in RR have similar issues

#sitename <- "Barnett Lane, Elstree"
#sitename <- "Locksbrook Road South"

#these don't render specifically because of a metric error - CHARLIE CODE IN WAYS TO NOTICE THESE BEFORE IN METRIC CHECK

#sitename <- "Copenacre, Corsham" #crashes 3% [metric-set-up] Error in `FUN()`:! non-numeric argument to binary operator
#sitename <- "Swindon J16" #crashes  3% [metric-set-up] Error in `if (nrow(broadenhance) == 0) ...`:! argument is of length zero
#sitename <- "Little Mill, Farringdon" #crashes 3% [metric-set-up]  Error in `if (nrow(broadenhance) == 0) ...`: ! argument is of length zero
#sitename <- "The Vale"  #crashes 3% [metric-set-up] Error in `if (nrow(broadenhance) == 0) ...`: ! argument is of length zero

#even if the metric was fixed, these dont render because of an ECoDS error - CHECK LATER

#sitename <- "The Vale" #crashes 13% [ecods] Error in `st_coordinates.sfc()`: ! not implemented for objects of class sfc_GEOMETRY

#### pull metric based on site name ####

if (sitename == "Woburn maintanence yard"){metric <- file.path(data_dir, "Metrics", "Woburn.xlsx")}
if (sitename == "Land South of Snarlton Farm, Melksham"){metric <- file.path(data_dir, "Metrics", "SnarltonFarm.xlsx")}
if (sitename == "Land South of Wing Road Stewkley"){metric <- file.path(data_dir, "Metrics", "WingRoad.xlsx")}
if (sitename == "Barnett Lane, Elstree"){metric <- file.path(data_dir, "Metrics", "Barnet Lane Elstree Statutory Metric 30.01.25.xlsx")}
if (sitename == "Locksbrook Road South"){metric <- file.path(data_dir, "Metrics", "Locksbrook Road South  Statutory Biodiversity Metric V3 - 01 11 24.xlsx")}
if (sitename == "Little Mill, Farringdon"){metric <- file.path(data_dir, "Metrics", "Little Farringdon Mill Metric V1.xlsx")}
if (sitename == "Swindon J16"){metric <- file.path(data_dir, "Metrics", "Swindon J16 Statutory Biodiversity Metric V4.xlsx")}
if (sitename == "The Vale"){metric <- file.path(data_dir, "Metrics", "The Vale Statutory Metric 24.01.25.xlsx")}
if (sitename == "Copenacre, Corsham"){metric <- file.path(data_dir, "Metrics", "CopenacerCLEAN.xlsx")}

metric_check(metric)