library(testthat)

# Simple test cases for abupilost, abupiretain, abupicreate, and abupienhance

# Example data for habitat loss, retain, create, and enhance
habitatlostdata <- data.frame(
  broadhabitat = c("Grassland", "Woodland"),
  habitattype = c("Neutral Grassland", "Deciduous Woodland"),
  arealost = c(10, 20)
)

habitatretaindata <- data.frame(
  broadhabitat = c("Grassland", "Woodland"),
  habitattype = c("Neutral Grassland", "Deciduous Woodland"),
  arearetained = c(5, 10)
)

habitatcreationdata <- data.frame(
  broadhabitat = c("Grassland", "Woodland"),
  habitattype = c("Neutral Grassland", "Deciduous Woodland"),
  createdarea = c(15, 25),
  createdcondition = c("Condition N/A", "Good"),
  distinctiveness = c("Medium", "High"),
  createdss = c("Strategic", "Non-Strategic")
)

habitenhancementdata <- data.frame(
  broadhabitat = c("Grassland", "Woodland"),
  habitattype = c("Neutral Grassland", "Deciduous Woodland"),
  enhancedarea = c(5, 8),
  basehabitattype = c("Neutral Grassland", "Deciduous Woodland"),
  distinctiveness = c("Low", "High"),
  enhancedss = c("Strategic", "Non-Strategic"),
  enhancedcondition = c("Good", "Excellent")
)

# Test abupilost function
test_that("abupilost increments sectioncounter correctly", {
  sectioncounter <- 1
  new_sectioncounter <- abupilost(habitatlostdata, sectioncounter)
  expect_equal(new_sectioncounter, 2)
})

# Test abupiretain function
test_that("abupiretain increments sectioncounter correctly", {
  sectioncounter <- 1
  new_sectioncounter <- abupiretain(habitatretaindata, sectioncounter)
  expect_equal(new_sectioncounter, 2)
})

# Test abupicreate function
test_that("abupicreate increments sectioncounter correctly", {
  sectioncounter <- 1
  new_sectioncounter <- abupicreate(habitatcreationdata, sectioncounter)
  expect_equal(new_sectioncounter, 2)
})

# Test abupienhance function
test_that("abupienhance increments sectioncounter correctly", {
  sectioncounter <- 1
  new_sectioncounter <- abupienhance(habitenhancementdata, sectioncounter)
  expect_equal(new_sectioncounter, 2)
})

