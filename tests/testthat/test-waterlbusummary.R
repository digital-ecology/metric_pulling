library(testthat)

# Create a mock data frame for testing
waterbaselinedata <- data.frame(
  habitattype = c("River", "Stream"),
  baselinelength = c(15.2, 10.3),
  distinctiveness = c("High", "Medium"),
  baseliness = c("Stable", "Changing"),
  baselinecondition = c("Good", "Condition Assessment N/A")
)

# Test 1: Basic check on output for a single row
test_that("Prints correct summary for each row", {
  sectioncounter <- 1
  output <- capture_output(waterlbusummary(waterbaselinedata, sectioncounter))
  
  # Check if the first watercourse prints the expected output
  expect_true(grepl("**River**", output))
  expect_true(grepl("5.2.2 The watercourse consists of a 15.2 km length of river", output))
  expect_true(grepl("This watercourse is in good condition. It meets this condition because of x, y, and z.", output))
  
  # Check if the second watercourse prints the expected output
  expect_true(grepl("**Stream**", output))
  expect_true(grepl("5.2.3 The watercourse consists of a 10.3 km length of stream", output))
  expect_true(grepl("This watercourse is in an unassessable condition.", output))
})

# Test 2: Check behavior when baseline condition is "N/A"
test_that("Handles 'N/A' condition correctly", {
  waterbaselinedata$baselinecondition[2] <- "N/A - Other"
  sectioncounter <- 1
  output <- capture_output(waterlbusummary(waterbaselinedata, sectioncounter))
  
  # Ensure that the "N/A" condition is handled as expected
  expect_true(grepl("This watercourse is in an unassessable condition.", output))
})

# Test 3: Check if section counter increments properly
test_that("Section counter increments correctly", {
  sectioncounter <- 5
  output <- capture_output(waterlbusummary(waterbaselinedata, sectioncounter))
  
  # Check if the section counter increases as expected
  expect_true(grepl("5.2.6", output))  # First watercourse should print 5.2.6
  expect_true(grepl("5.2.7", output))  # Second watercourse should print 5.2.7
})

# Test 4: Edge case: empty dataset
test_that("Handles empty watercourse data", {
  empty_data <- data.frame(
    habitattype = character(0),
    baselinelength = numeric(0),
    distinctiveness = character(0),
    baseliness = character(0),
    baselinecondition = character(0)
  )
  sectioncounter <- 1
  output <- capture_output(waterlbusummary(empty_data, sectioncounter))
  
  # Ensure no output is printed
  expect_equal(output, "")
})
