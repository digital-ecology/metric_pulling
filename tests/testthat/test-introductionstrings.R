library(testthat)

# Test data
baselinedata <- tibble::tibble(
  habitat = c("riparian", "wetland", "forest", "wetland"),
  description = c("good condition", "moderate condition", "poor condition", "moderate condition")
)

lpa_names <- tibble::tibble(name = c("District 1", "District 2"))

# Test for intro_habbaselinelist
test_that("intro_habbaselinelist returns a list of strings", {
  result <- intro_habbaselinelist(baselinedata)
  expect_type(result, "list")
  expect_true(all(sapply(result, is.character)))  # Check that all elements are character strings
  expect_equal(length(result), 3)  # There should be 3 unique habitats (after removing duplicates)
})

# Test for intro_linearbaselinelist
test_that("intro_linearbaselinelist returns a list of strings", {
  result <- intro_linearbaselinelist(baselinedata)
  expect_type(result, "list")
  expect_true(all(sapply(result, is.character)))  # Check that all elements are character strings
  expect_equal(length(result), 3)  # There should be 3 unique habitats (after removing duplicates)
})

# Test for lpastring
test_that("lpastring returns a correct LPA string", {
  result <- lpastring(lpa_names)
  expect_type(result, "character")
  expect_equal(result, "District 1, and District 2")  # Check concatenation of multiple LPA names
})

# Test for lpastring with a single LPA name
test_that("lpastring returns a correct string for a single LPA", {
  single_lpa <- tibble::tibble(name = c("District 1"))
  result <- lpastring(single_lpa)
  expect_type(result, "character")
  expect_equal(result, "District 1")  # Should return the single LPA name
})
