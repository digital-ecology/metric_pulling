library(testthat)
library(openxlsx)

test_that("pullonsitehabitatsumdata processes the metric file correctly", {
  
  # Mock file path (you will need to use a valid path to an actual metric file for real tests)
  metric <- "data-raw/OnSiteTradingSummary.xlsx"
  
  # Assuming the function is being tested with a mock metric file
  result <- pullonsitehabitatsumdata(metric)
  
  # Check that the returned result is a list
  expect_is(result, "list")
  
  # Check that the list contains the expected elements
  expect_true("TradingSatisfied" %in% names(result))
  expect_true("VHNet" %in% names(result))
  expect_true("HNet" %in% names(result))
  expect_true("MNet" %in% names(result))
  expect_true("LNet" %in% names(result))
  
  # Check that the "TradingSatisfied" data frame contains the correct columns
  expect_true(all(c("Distinctiveness", "Satisfied") %in% colnames(result$TradingSatisfied)))
  
  # Check that the VHNet, HNet, MNet, and LNet data frames contain the correct columns
  expect_true(all(c("HabitatGroup", "ProjectWideUnitChange") %in% colnames(result$VHNet)))
  expect_true(all(c("HabitatGroup", "ProjectWideUnitChange") %in% colnames(result$HNet)))
  expect_true(all(c("HabitatGroup", "ProjectWideUnitChange") %in% colnames(result$MNet)))
  expect_true(all(c("HabitatGroup", "ProjectWideUnitChange") %in% colnames(result$LNet)))
  
  
  
})


library(testthat)
library(openxlsx)

test_that("pullonsitehedgesumdata processes the hedgerow metric file correctly", {
  
  # Mock file path (you will need to use a valid path to an actual metric file for real tests)
  metric <- "data-raw/OnSiteTradingSummary.xlsx"
  
  # Run the function with the mock metric file
  result <- pullonsitehedgesumdata(metric)
  
  # Check that the result is a list
  expect_is(result, "list")
  
  # Check that the list contains the expected elements
  expect_true("TradingSatisfied" %in% names(result))
  expect_true("VHNet" %in% names(result))
  expect_true("HNet" %in% names(result))
  expect_true("MNet" %in% names(result))
  expect_true("LNet" %in% names(result))
  expect_true("VLNet" %in% names(result))
  
  # Check that the "TradingSatisfied" data frame contains the correct columns
  expect_true(all(c("Distinctiveness", "Satisfied") %in% colnames(result$TradingSatisfied)))
  
  # Check that the VHNet, HNet, MNet, LNet, and VLNet data frames contain the correct columns
  expect_true(all(c("HabitatGroup", "ProjectWideUnitChange") %in% colnames(result$VHNet)))
  expect_true(all(c("HabitatGroup", "ProjectWideUnitChange") %in% colnames(result$HNet)))
  expect_true(all(c("HabitatGroup", "ProjectWideUnitChange") %in% colnames(result$MNet)))
  expect_true(all(c("HabitatGroup", "ProjectWideUnitChange") %in% colnames(result$LNet)))
  expect_true(all(c("HabitatGroup", "ProjectWideUnitChange") %in% colnames(result$VLNet)))
  
})


test_that("pullonsitewatersumdata processes the water metric file correctly", {
  
  # Mock file path (you will need to use a valid path to an actual metric file for real tests)
  metric <- "data-raw/OnSiteTradingSummary.xlsx"
  
  # Run the function with the mock metric file
  result <- pullonsitewatersumdata(metric)
  
  # Check that the result is a list
  expect_is(result, "list")
  
  # Check that the list contains the expected elements
  expect_true("TradingSatisfied" %in% names(result))
  expect_true("VHNet" %in% names(result))
  expect_true("HNet" %in% names(result))
  expect_true("MNet" %in% names(result))
  expect_true("LNet" %in% names(result))
  
  # Check that the "TradingSatisfied" data frame contains the correct columns
  expect_true(all(c("Distinctiveness", "Satisfied") %in% colnames(result$TradingSatisfied)))
  
  # Check that the VHNet, HNet, MNet, and LNet data frames contain the correct columns
  expect_true(all(c("HabitatGroup", "ProjectWideUnitChange") %in% colnames(result$VHNet)))
  expect_true(all(c("HabitatGroup", "ProjectWideUnitChange") %in% colnames(result$HNet)))
  expect_true(all(c("HabitatGroup", "ProjectWideUnitChange") %in% colnames(result$MNet)))
  expect_true(all(c("HabitatGroup", "ProjectWideUnitChange") %in% colnames(result$LNet)))
  
})
