#library(testthat)

# Test for offsitehabitatbaseline
test_that("offsitehabitatbaseline returns a list with correct structure", {
  
  metric <- readRDS(test_path("fixtures", "offsiteexample.rds"))
  
  result <- offsitehabitatbaseline(metric)
  
  # Check if the result is a list
  expect_type(result, "list")
  
  # Check the structure of the returned list
  expect_named(result, c("habitatbaselinedata", "totalarea", "totalunits"))
  
  # Check the structure of the habitatbaselinedata
  expect_s3_class(result$habitatbaselinedata, "data.frame")
  expect_equal(ncol(result$habitatbaselinedata), 7)  # Expecting 7 columns in habitatbaselinedata
  
  # Check that totalarea and totalunits are data frames with one column
  expect_type(result$totalarea, "double")
  #expect_equal(ncol(result$totalarea), 1)
  
  expect_type(result$totalunits, "double")
  #expect_equal(ncol(result$totalunits), 1)
})

# Test for offsitehabitatretain
test_that("offsitehabitatretain returns a list with correct structure", {
  
  metric <- readRDS(test_path("fixtures", "offsiteexample.rds"))
  
  result <- offsitehabitatretain(metric)
  
  # Check if the result is a list
  expect_type(result, "list")
  
  # Check the structure of the returned list
  expect_named(result, c("habitatretaindata", "TotalRetainArea", "TotalRetainUnits"))
  
  # Check the structure of the habitatretaindata
  expect_s3_class(result$habitatretaindata, "data.frame")
  expect_equal(ncol(result$habitatretaindata), 4)  # Expecting 4 columns in habitatretaindata
  
  # Check that TotalRetainArea and TotalRetainUnits are data frames with one column
  expect_type(result$TotalRetainArea, "double")
  
  expect_type(result$TotalRetainUnits, "double")

})

# Test for offsitehabitatloss
test_that("offsitehabitatloss returns a list with correct structure", {
  
  metric <- readRDS(test_path("fixtures", "offsiteexample.rds"))
  
  result <- offsitehabitatloss(metric)
  
  # Check if the result is a list
  expect_type(result, "list")
  
  # Check the structure of the returned list
  expect_named(result, c("habitatlostdata", "TotallostArea", "TotallostUnits"))
  expect_equal(ncol(result$habitatlostdata), 4)  # Expecting 4 columns in habitatlostdata
  
  # Check the structure of the habitatlostdata
  expect_s3_class(result$habitatlostdata, "data.frame")
  
  # Check that TotallostArea and TotallostUnits are data frames with one column
  expect_type(result$TotallostArea, "double")
  
  expect_type(result$TotallostUnits, "double")
  
})

# Test for offsitehabitatcreation
test_that("offsitehabitatcreation returns a list with correct structure", {
  
  metric <- readRDS(test_path("fixtures", "offsiteexample.rds"))
  
  result <- offsitehabitatcreation(metric)
  
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
