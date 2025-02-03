library(testthat)

# Define a mock dataset
mock_data <- data.frame(
  habitattype = c("River", "Stream"),
  baselinelength = c(2.5, 1.2),
  distinctiveness = c("High", "Medium"),
  baseliness = c("Strategic", "Not Strategic"),
  baselinecondition = c("Good", "Condition Assessment N/A"),
  stringsAsFactors = FALSE
)

# Define a helper function to capture printed output
capture_output <- function(expr) {
  output <- capture.output(expr)
  paste(output, collapse = "\n")
}

test_that("waterlbusummary produces output", {
  sectioncounter <- 1
  output <- capture_output(waterlbusummary(mock_data, sectioncounter))
  print(output)
  expect_true(nchar(output) > 0, "The function should produce some output.")
})
