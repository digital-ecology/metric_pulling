library(openxlsx)

# load workbooks and save as rds

# hedges
wb <- loadWorkbook("inst/extdata/OnSiteHedgeEnhance.xlsx")
saveRDS(wb, "tests/testthat/fixtures/onsitehedgeenhance.rds")

# trading summary
wb_ts <- loadWorkbook("inst/extdata/OnSiteTradingSummary.xlsx")
saveRDS(wb_ts, "tests/testthat/fixtures/onsitetradingsummary.rds")

# habitats
wb_osb <- loadWorkbook("inst/extdata/OnSiteBoth.xlsx")
saveRDS(wb_osb, "tests/testthat/fixtures/onsiteboth.rds")

# off-site
wb_off <- openxlsx::loadWorkbook("inst/extdata/OffSiteExample.xlsx")
saveRDS(wb_off, "tests/testthat/fixtures/offsiteexample.rds")
