#library(testthat)

# Test for pullonsitewaterbaseline function
test_that("pullonsitewaterbaseline returns waterbaselinedata", {
  
  metric <- readRDS(test_path("fixtures", "onsiteboth.rds"))
  
  # Call the function with the mock metric
  result <- pullonsitewaterbaseline(metric)
  
  # Check if the result is a list
  expect_true(is.list(result), info = "The result should be a list.")
  
  # Check if 'waterbaselinedata' is part of the list
  expect_true("waterbaselinedata" %in% names(result), info = "The list should contain 'waterbaselinedata'.")
  
  # Check if 'waterbaselinedata' is a data frame
  expect_true(is.data.frame(result$waterbaselinedata), info = "'waterbaselinedata' should be a data frame.")
  
  # Optionally, check if the columns in waterbaselinedata match the expected ones
  expected_columns <- c("habitattype", "baselinelength", "baselinecondition", "baseliness", "baselinelbu", "distinctiveness")
  expect_true(all(expected_columns %in% colnames(result$waterbaselinedata)), info = "Columns in 'waterbaselinedata' do not match the expected ones.")
  
})


# Test for pullonsitewaterretain function
test_that("pullonsitewaterretain returns waterretaindata", {
  
  metric <- readRDS(test_path("fixtures", "onsiteboth.rds"))
  
  # Call the function with the mock metric
  result <- pullonsitewaterretain(metric)
  
  # Check if the result is a list
  expect_true(is.list(result), info = "The result should be a list.")
  
  # Check if 'waterretaindata' is part of the list
  expect_true("waterretaindata" %in% names(result), info = "The list should contain 'waterretaindata'.")
  
  # Check if 'waterretaindata' is a data frame
  expect_true(is.data.frame(result$waterretaindata), info = "'waterretaindata' should be a data frame.")
  
})


# Test for pullonsitewaterloss function
test_that("pullonsitewaterloss returns waterlostdata", {
  
  metric <- readRDS(test_path("fixtures", "onsiteboth.rds"))
  
  # Call the function with the mock metric
  result <- pullonsitewaterloss(metric)
  
  # Check if the result is a list
  expect_true(is.list(result), info = "The result should be a list.")
  
  # Check if 'waterlostdata' is part of the list
  expect_true("waterlostdata" %in% names(result), info = "The list should contain 'waterlostdata'.")
  
  # Check if 'waterlostdata' is a data frame
  expect_true(is.data.frame(result$waterlostdata), info = "'waterlostdata' should be a data frame.")
  

 
})

# Test for pullonsitewatercreation function
test_that("pullonsitewatercreation returns watercreationdata", {
  
  metric <- readRDS(test_path("fixtures", "onsiteboth.rds"))
  
  # Call the function with the mock metric
  result <- pullonsitewatercreation(metric)
  
  # Check if the result is a list
  expect_true(is.list(result), info = "The result should be a list.")
  
  # Check if 'watercreationdata' is part of the list
  expect_true("watercreationdata" %in% names(result), info = "The list should contain 'watercreationdata'.")
  
  # Check if 'watercreationdata' is a data frame
  expect_true(is.data.frame(result$watercreationdata), info = "'watercreationdata' should be a data frame.")
  
  # Check if the total created water length and units are present
  expect_true("TotalCreatedwaterLength" %in% names(result), info = "'TotalCreatedwaterLength' should be present in the result.")
  expect_true("TotalCreatedwaterUnits" %in% names(result), info = "'TotalCreatedwaterUnits' should be present in the result.")
  
})


# Test for pullonsitewaterenhancement function
test_that("pullonsitewaterenhancement returns waterenhancementdata", {
  
  metric <- readRDS(test_path("fixtures", "onsiteboth.rds"))
  
  # Call the function with the mock metric
  result <- pullonsitewaterenhancement(metric)
  
  # Check if the result is a list
  expect_true(is.list(result), info = "The result should be a list.")
  
  # Check if 'waterenhancementdata' is part of the list
  expect_true("waterenhancementdata" %in% names(result), info = "The list should contain 'waterenhancementdata'.")
  
  # Check if 'waterenhancementdata' is a data frame
  expect_true(is.data.frame(result$waterenhancementdata), info = "'waterenhancementdata' should be a data frame.")
  
  # Optionally, check if the columns in waterenhancementdata match the expected ones
  expected_columns <- c("habitattype", "enhancedlength", "enhancedcond", "enhancedss", "distinctiveness", "enhancedunits", "basehabitattype", "basecondition")
  expect_true(all(expected_columns %in% colnames(result$waterenhancementdata)), info = "Columns in 'waterenhancementdata' do not match the expected ones.")
  
  
})
