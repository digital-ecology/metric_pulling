library(testthat)

# Test 1: Check that the function renders the correct section title
test_that("Correct section title rendered for survey", {
  sectionCounter <- 5
  output <- capture_output({
    sectionCounter <- rendersurveymethodology("Grass", sectionCounter)
  })
  
  # Check that the correct title is rendered
  expect_true(grepl("## 3.6 Grassland Survey Methodology", output))
  
  # Check if the sectionCounter is updated correctly
  expect_equal(sectionCounter, 6)
})

# Test 2: Check rendering for unknown survey type
test_that("Renders correct message for unknown survey type", {
  sectionCounter <- 5
  output <- capture_output({
    sectionCounter <- rendersurveymethodology("UnknownSurvey", sectionCounter)
  })
  
  # Check that the correct fallback title is rendered
  expect_true(grepl("### 3.6 Unknown Section UnknownSurvey", output))
  expect_true(grepl("No Unknown Section UnknownSurvey template found.", output))
  
  # Check if the sectionCounter is updated correctly
  expect_equal(sectionCounter, 6)
})

# Test 3: Check handling when template file exists
test_that("Renders content from file when template exists", {
  # Create a mock file for testing
  survey <- "Grass"
  sectionCounter <- 5
  filename <- paste0(survey, ".qmd")
  
  # Create a temporary file for testing
  writeLines(c("This is sectionNumber content for Grass survey."), filename)
  
  # Capture the output of the function
  output <- capture_output({
    sectionCounter <- rendersurveymethodology(survey, sectionCounter)
  })
  
  # Check that the sectionCounter is updated
  expect_equal(sectionCounter, 6)
  
  # Check that the section content has been rendered with correct section number
  expect_true(grepl("This is 6 content for Grass survey.", output))
  
  # Clean up the file after test
  file.remove(filename)
})

# Test 4: Check handling when template file doesn't exist
test_that("Renders correct message when file does not exist", {
  sectionCounter <- 5
  output <- capture_output({
    sectionCounter <- rendersurveymethodology("Hedge", sectionCounter)
  })
  
  # Check that the function handles missing template file correctly
  expect_true(grepl("### 3.6 Hedgerow Survey Methodology", output))
  expect_true(grepl("No Hedgerow Survey Methodology template found.", output))
  
  # Check if the sectionCounter is updated correctly
  expect_equal(sectionCounter, 6)
})
