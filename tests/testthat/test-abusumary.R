library(testthat)

# Example data for habitat baseline
habitatbaselinedata <- data.frame(
  habitattype = c("Wetland", "Woodland"),
  baselinearea = c(15.5, 25.0),
  distinctiveness = c("High", "Medium"),
  baseliness = c("Strategic", "Non-Strategic"),
  baselinecondition = c("Good", "Condition Assessment N/A")
)

# Test abusummary function
test_that("abusummary generates the correct output format and handles data types properly", {
  sectioncounter <- 1
  
  # Redirecting output to capture printed text
  output <- capture.output(abusummary(habitatbaselinedata, sectioncounter))
  
  # Check if the sectioncounter increments and generates numeric outputs (e.g., area values)
  expect_true(any(grepl("5.2.[0-9]+", output)))  # Check if sectioncounter increments as numbers
  expect_true(any(grepl("\\d+\\.\\d+ha", output)))  # Check if area is printed as a float with 'ha' suffix
  
  # Check if distinctiveness is printed in the expected format (string)
  expect_true(any(grepl("high distinctiveness", output, ignore.case = TRUE)))  # Test if 'high distinctiveness' is printed
  expect_true(any(grepl("medium distinctiveness", output, ignore.case = TRUE)))  # Test if 'medium distinctiveness' is printed
  
  # Check if the condition is processed and outputs the correct string type
  expect_true(any(grepl("good condition", output, ignore.case = TRUE)))  # Expect 'good condition' for Wetland
  expect_true(any(grepl("an unassessable condition", output, ignore.case = TRUE)))  # Expect 'unassessable condition' for Woodland
  
  # Ensure correct handling of numeric data for distinctiveness, condition, and area
  expect_true(any(grepl("\\d+\\.\\d+", output)))  # Check for numeric values (like area)
  expect_true(any(grepl("strategic", output, ignore.case = TRUE)))  # Check if 'strategic' appears for Wetland
  
  # Check that data is rendered correctly for multiple rows (handle row-wise iteration)
  expect_equal(length(grep("5.2.", output)), nrow(habitatbaselinedata) * 2)  # Ensure the correct number of paragraphs (sectioncounter increments twice for each row)
})
