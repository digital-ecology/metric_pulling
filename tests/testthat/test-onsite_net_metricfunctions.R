# library(testthat)
# library(openxlsx)

test_that("pullonsitenetdata processes the net data correctly", {
  
  metric <- readRDS(test_path("fixtures", "onsitehedgeenhance.rds"))
  
  # Run the function with the mock metric file
  result <- pullonsitenetdata(metric)
  
  # Check that the result is a list
  expect_type(result, "list")
  
  # Check that the list contains the expected elements
  expect_true("NetData" %in% names(result))
  expect_true("TradeSatisfied" %in% names(result))
  
  # Check that each data frame contains the expected column names
  expect_true("Type" %in% colnames(result$NetData))
  expect_true("BaseUnits" %in% colnames(result$NetData))
  expect_true("PIUnits" %in% colnames(result$NetData))
  expect_true("NetUnits" %in% colnames(result$NetData))
  expect_true("NetPercent" %in% colnames(result$NetData))
  expect_true("Deficit" %in% colnames(result$NetData))
  
  # Check that TradeSatisfied is either "Yes" or "No"
  expect_true(result$TradeSatisfied %in% c("Yes", "No"))
  
})
