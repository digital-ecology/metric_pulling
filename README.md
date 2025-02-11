# metric pulling 

Contains: 
- missing_data_catches.R ← code to catch errors
- onsite_[net/hedge/habitat/etc.]_functions.R ← pulling data in with catches built in - returns either processed data for use in qmd, or "please check [sheet name]..."
- metric_check.R ← function to bring all above together to take ‘metric’ as the argument and be able to be run on its own within the app – outputs names of metrics sheets where there are errors