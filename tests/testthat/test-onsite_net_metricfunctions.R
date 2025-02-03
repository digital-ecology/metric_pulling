library(testthat)
library(openxlsx)

test_that("pullonsitenetdata processes the net data correctly", {
  
  # Mock file path (you will need to use a valid path to an actual metric file for real tests)
  metric <- "data-raw/OnSiteHedgeEnhance.xlsx"
  
  # Run the function with the mock metric file
  result <- pullonsitenetdata(metric)
  
  # Check that the result is a list
  expect_is(result, "list")
  
  # Check that the list contains the expected elements
  expect_true("NetHabitatUnits" %in% names(result))
  expect_true("NetHabitatPercent" %in% names(result))
  expect_true("HabitatDeficit" %in% names(result))
  expect_true("NetHedgerowUnits" %in% names(result))
  expect_true("NetHedgerowPercent" %in% names(result))
  expect_true("HedgerowDeficit" %in% names(result))
  expect_true("NetWaterUnits" %in% names(result))
  expect_true("NetWaterPercent" %in% names(result))
  expect_true("WaterDeficit" %in% names(result))
  expect_true("TradeSatisfied" %in% names(result))
  
  # Check that each data frame contains the expected column names
  expect_true("NetHabUnits" %in% colnames(result$NetHabitatUnits))
  expect_true("NetHabPercent" %in% colnames(result$NetHabitatPercent))
  expect_true("HabitatDeficit" %in% colnames(result$HabitatDeficit))
  expect_true("NetHedgeUnits" %in% colnames(result$NetHedgerowUnits))
  expect_true("NetHedgePercent" %in% colnames(result$NetHedgerowPercent))
  expect_true("HedgeDeficit" %in% colnames(result$HedgerowDeficit))
  expect_true("NetWaterUnits" %in% colnames(result$NetWaterUnits))
  expect_true("NetWaterPercent" %in% colnames(result$NetWaterPercent))
  expect_true("WaterDeficit" %in% colnames(result$WaterDeficit))
  
  # Check that TradeSatisfied is either "Yes" or "No"
  expect_true(result$TradeSatisfied %in% c("Yes", "No"))
  
  # Check that the values are rounded correctly
  expect_equal(result$NetHabitatPercent, round(result$NetHabitatPercent, 2))
  expect_equal(result$NetHedgerowPercent, round(result$NetHedgerowPercent, 2))
  expect_equal(result$NetWaterPercent, round(result$NetWaterPercent, 2))
  
 
})
