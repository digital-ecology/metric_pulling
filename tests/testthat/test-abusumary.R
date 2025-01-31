library(testthat)

# Create a mock data frame for testing
habitatbaselinedata <- data.frame(
  habitattype = c("Forest", "Grassland"),
  baselinearea = c(10.5, 20.3),
  distinctiveness = c("High", "Medium"),
  baseliness = c("Stable", "Changing"),
  baselinecondition = c("Good", "Condition Assessment N/A")
)

# Test 1: Basic check on output for a single row
test_that("Prints correct summary for each row", {
  sectioncounter <- 1
  output <- capture_output(abusummary(habitatbaselinedata, sectioncounter))
  
  # Check if the first habitat prints the expected output
  expect_true(grepl("### Forest", output))
  expect_true(grepl("5.2.2 The site consists of a 10.5 ha area of forest", output))
  expect_true(grepl("This habitat is in good condition. It meets this condition because of x, y, and z.", output))
  
  # Check if the second habitat prints the expected output
  expect_true(grepl("### Grassland", output))
  expect_true(grepl("5.2.3 The site consists of a 20.3 ha area of grassland", output))
  expect_true(grepl("This habitat is in an unassessable condition.", output))
})

# Test 2: Check behavior when baseline condition is "N/A"
test_that("Handles 'N/A' condition correctly", {
  habitatbaselinedata$baselinecondition[2] <- "N/A - Other"
  sectioncounter <- 1
  output <- capture_output(abusummary(habitatbaselinedata, sectioncounter))
  
  # Ensure that the "N/A" condition is handled as expected
  expect_true(grepl("This habitat is in an unassessable condition.", output))
})

# Test 3: Check if section counter increments properly
test_that("Section counter increments correctly", {
  sectioncounter <- 5
  output <- capture_output(abusummary(habitatbaselinedata, sectioncounter))
  
  # Check if the section counter increases as expected
  expect_true(grepl("5.2.6", output))  # First habitat should print 5.2.6
  expect_true(grepl("5.2.7", output))  # Second habitat should print 5.2.7
})

# Test 4: Edge case: empty dataset
test_that("Handles empty habitat data", {
  empty_data <- data.frame(
    habitattype = character(0),
    baselinearea = numeric(0),
    distinctiveness = character(0),
    baseliness = character(0),
    baselinecondition = character(0)
  )
  sectioncounter <- 1
  output <- capture_output(abusummary(empty_data, sectioncounter))
  
  # Ensure no output is printed
  expect_equal(output, "")
})
