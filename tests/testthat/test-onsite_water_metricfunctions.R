library(testthat)

test_that("pullonsitewaterbaseline returns correct structure", {
  # Assuming 'metric' is a valid file path or input
  metric <- "data-raw/OnSiteHedgeEnhance.xlsx"
  
  result <- pullonsitewaterbaseline(metric)
  
  # Check if the result is a list
  expect_type(result, "list")
  
  # Check if the waterbaselinedata is a data frame
  expect_true(is.data.frame(result$waterbaselinedata))
  
  # Check for required columns in the output data frame
  expect_true("habitattype" %in% colnames(result$waterbaselinedata))
  expect_true("baselinelength" %in% colnames(result$waterbaselinedata))
  expect_true("baselinecondition" %in% colnames(result$waterbaselinedata))
  expect_true("baseliness" %in% colnames(result$waterbaselinedata))
  expect_true("baselinelbu" %in% colnames(result$waterbaselinedata))
  expect_true("distinctiveness" %in% colnames(result$waterbaselinedata))
  
  # Check if the total length and total units are numeric
  expect_true(is.numeric(result$totallength))
  expect_true(is.numeric(result$totalunits))
})

test_that("pullonsitewaterbaseline handles missing data", {
  metric <- "data-raw/OnSiteBoth.xlsx"
  
  result <- pullonsitewaterbaseline(metric)
  
  # Check if the function can handle missing data and return default values
  expect_equal(result$waterbaselinedata$habitattype, "No Existing Watercourses")
})
