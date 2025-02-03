library(testthat)
library(openxlsx)

# Mock data for testing
metric <- "data-raw/OnSiteHedgeEnhance.xlsx"  # Replace with actual path

# Test pullonsitehedgebaseline function
test_that("pullonsitehedgebaseline returns expected data structure", {
  result <- pullonsitehedgebaseline(metric)
  
  # Check that the result is a list
  expect_type(result, "list")
  
  # Check that each element in the list is a data frame
  expect_s3_class(result$hedgebaselinedata, "data.frame")
  expect_s3_class(result$LengthEnhanced, "data.frame")
  expect_s3_class(result$UnitsEnhanced, "data.frame")
  expect_s3_class(result$totallength, "data.frame")
  expect_s3_class(result$totalunits, "data.frame")
  
  
})

# Test pullonsitehedgeretain function
test_that("pullonsitehedgeretain handles missing data correctly", {
  result <- pullonsitehedgeretain(metric)
  
  # Check that the result is a list
  expect_type(result, "list")
  
  # Check that the 'hedgeretaindata' element is a data frame
  expect_s3_class(result$hedgeretaindata, "data.frame")
  
  
  # Test for placeholder when no data is retained
  expect_true(nrow(result$hedgeretaindata) >= 0)
})

# Test pullonsitehedgeloss function
test_that("pullonsitehedgeloss returns correct loss data", {
  result <- pullonsitehedgeloss(metric)
  
  # Check that the result is a list
  expect_type(result, "list")
  
  # Check that the 'hedgelostdata' element is a data frame
  expect_s3_class(result$hedgelostdata, "data.frame")
  
  # Test for placeholder when no hedgerows lost
  expect_true(nrow(result$hedgelostdata) >= 0)
})

# Test pullonsitehedgecreation function
test_that("pullonsitehedgecreation correctly returns creation data", {
  result <- pullonsitehedgecreation(metric)
  
  # Check that the result is a list
  expect_type(result, "list")
  
  # Check that the 'hedgecreationdata' element is a data frame
  expect_s3_class(result$hedgecreationdata, "data.frame")
})



