library(testthat)

# Test for pullonsitehabitatbaseline
test_that("pullonsitehabitatbaseline returns a list with correct structure", {
  metric <- "data-raw/OnSiteHedgeEnhance.xlsx"
  
  result <- pullonsitehabitatbaseline(metric)
  
  # Check if the result is a list
  expect_type(result, "list")
  
  # Check the structure of the returned list
  expect_named(result, c("habitatbaselinedata", "totalarea", "totalunits"))
  
  # Check the structure of the habitatbaselinedata
  expect_s3_class(result$habitatbaselinedata, "data.frame")
  expect_equal(ncol(result$habitatbaselinedata), 7)  # Expecting 7 columns in habitatbaselinedata
  
  # Check that totalarea and totalunits are data frames with one column
  expect_s3_class(result$totalarea, "data.frame")
  expect_equal(ncol(result$totalarea), 1)
  
  expect_s3_class(result$totalunits, "data.frame")
  expect_equal(ncol(result$totalunits), 1)
})

# Test for pullonsitehabitatretain
test_that("pullonsitehabitatretain returns a list with correct structure", {
  metric <- "data-raw/OnSiteHedgeEnhance.xlsx"  
  result <- pullonsitehabitatretain(metric)
  
  # Check if the result is a list
  expect_type(result, "list")
  
  # Check the structure of the returned list
  expect_named(result, c("habitatretaindata", "TotalRetainArea", "TotalRetainUnits"))
  
  # Check the structure of the habitatretaindata
  expect_s3_class(result$habitatretaindata, "data.frame")

  # Check that TotalRetainArea and TotalRetainUnits are data frames with one column
  expect_s3_class(result$TotalRetainArea, "data.frame")
  expect_equal(ncol(result$TotalRetainArea), 1)
  
  expect_s3_class(result$TotalRetainUnits, "data.frame")
  expect_equal(ncol(result$TotalRetainUnits), 1)
})

# Test for pullonsitehabitatloss
test_that("pullonsitehabitatloss returns a list with correct structure", {
  metric <- "data-raw/OnSiteHedgeEnhance.xlsx"  
  result <- pullonsitehabitatloss(metric)
  
  # Check if the result is a list
  expect_type(result, "list")
  
  # Check the structure of the returned list
  expect_named(result, c("habitatlostdata", "TotallostArea", "TotallostUnits"))
  
  # Check the structure of the habitatlostdata
  expect_s3_class(result$habitatlostdata, "data.frame")

  # Check that TotallostArea and TotallostUnits are data frames with one column
  expect_s3_class(result$TotallostArea, "data.frame")
  expect_equal(ncol(result$TotallostArea), 1)
  
  expect_s3_class(result$TotallostUnits, "data.frame")
  expect_equal(ncol(result$TotallostUnits), 1)
})

# Test for pullonsitehabitatcreation
test_that("pullonsitehabitatcreation returns a list with correct structure", {
  metric <- "data-raw/OnSiteHedgeEnhance.xlsx"  
  result <- pullonsitehabitatcreation(metric)
  
  # Check if the result is a list
  expect_type(result, "list")
  
  # Check the structure of the returned list
  expect_named(result, c("habitatcreationdata", "TotalCreationArea", "TotalCreationUnits"))
  
  # Check the structure of the habitatcreationdata
  expect_s3_class(result$habitatcreationdata, "data.frame")

  # Check that TotalCreationArea and TotalCreationUnits are data frames with one column
  expect_s3_class(result$TotalCreationArea, "data.frame")
  expect_equal(ncol(result$TotalCreationArea), 1)
  
  expect_s3_class(result$TotalCreationUnits, "data.frame")
  expect_equal(ncol(result$TotalCreationUnits), 1)
})
