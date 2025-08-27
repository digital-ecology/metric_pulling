#library(testthat)

# Test for pullonsitehabitatbaseline
test_that("pullonsitehabitatbaseline returns a list with correct structure", {
  
  metric <- readRDS(test_path("fixtures", "onsitehedgeenhance.rds"))
  
  result <- pullonsitehabitatbaseline(metric)
  
  # Check if the result is a list
  expect_type(result, "list")
  
  # Check the structure of the returned list
  expect_named(result, c("habitatbaselinedata", "totalarea", "totalunits"))
  
  # Check the structure of the habitatbaselinedata
  expect_s3_class(result$habitatbaselinedata, "data.frame")
  expect_equal(ncol(result$habitatbaselinedata), 7)  # Expecting 7 columns in habitatbaselinedata
  
  # Check that totalarea and totalunits are data frames with one column
  expect_type(result$totalarea, "double")
  
  expect_type(result$totalunits, "double")
  
})

# Test for pullonsitehabitatretain
test_that("pullonsitehabitatretain returns a list with correct structure", {
  
  metric <- readRDS(test_path("fixtures", "onsitehedgeenhance.rds"))
  
  result <- pullonsitehabitatretain(metric)
  
  # Check if the result is a list
  expect_type(result, "list")
  
  # Check the structure of the returned list
  expect_named(result, c("habitatretaindata", "TotalRetainArea", "TotalRetainUnits"))
  
  # Check the structure of the habitatretaindata
  expect_s3_class(result$habitatretaindata, "data.frame")
  expect_equal(ncol(result$habitatretaindata), 4)

  # Check that TotalRetainArea and TotalRetainUnits are data frames with one column
  expect_type(result$TotalRetainArea, "double")
  
  expect_type(result$TotalRetainUnits, "double")
  
})

# Test for pullonsitehabitatloss
test_that("pullonsitehabitatloss returns a list with correct structure", {
  
  metric <- readRDS(test_path("fixtures", "onsitehedgeenhance.rds"))
  
  result <- pullonsitehabitatloss(metric)
  
  # Check if the result is a list
  expect_type(result, "list")
  
  # Check the structure of the returned list
  expect_named(result, c("habitatlostdata", "TotallostArea", "TotallostUnits"))
  
  # Check the structure of the habitatlostdata
  expect_s3_class(result$habitatlostdata, "data.frame")
  expect_equal(ncol(result$habitatlostdata), 4)

  # Check that TotallostArea and TotallostUnits are data frames with one column
  expect_type(result$TotallostArea, "double")
  
  expect_type(result$TotallostUnits, "double")
  
})

# Test for pullonsitehabitatcreation
test_that("pullonsitehabitatcreation returns a list with correct structure", {
  
  metric <- readRDS(test_path("fixtures", "onsitehedgeenhance.rds"))
  
  result <- pullonsitehabitatcreation(metric)
  
  # Check if the result is a list
  expect_type(result, "list")
  
  # Check the structure of the returned list
  expect_named(result, c("habitatcreationdata", "TotalCreationArea", "TotalCreationUnits"))
  
  # Check the structure of the habitatcreationdata
  expect_s3_class(result$habitatcreationdata, "data.frame")
  expect_equal(ncol(result$habitatcreationdata), 8)

  # Check that TotalCreationArea and TotalCreationUnits are data frames with one column
  expect_type(result$TotalCreationArea, "double")
  
  expect_type(result$TotalCreationUnits, "double")
  
})
