library(testthat)

# Test for exsum_baselinestring function
test_that("exsum_baselinestring works correctly", {
  habitattype_1 <- c("Other neutral grassland", "Wetland", "Woodland")
  habitattype_2 <- c("No Existing Watercourses")
  habitattype_3 <- c("Other neutral grassland", "Other neutral grassland")
  
  expect_equal(exsum_baselinestring(habitattype_1), "other neutral grassland, wetland and woodland")
  expect_equal(exsum_baselinestring(habitattype_2), "nothing")
  expect_equal(exsum_baselinestring(habitattype_3), "other neutral grassland")
})

# Test for exsum_creationstring function
test_that("exsum_creationstring works correctly", {
  createdhabitattype_1 <- c("Other neutral grassland", "Wetland", "Woodland")
  createdhabitattype_2 <- c("No Habitats Created")
  createdhabitattype_3 <- c("Other neutral grassland", "Other neutral grassland")
  
  expect_equal(exsum_creationstring(createdhabitattype_1), "other neutral grassland, wetland and woodland")
  expect_equal(exsum_creationstring(createdhabitattype_2), "nothing")
  expect_equal(exsum_creationstring(createdhabitattype_3), "other neutral grassland")
})

# Test for exsum_enhancestring function
test_that("exsum_enhancestring works correctly", {
  enhancedhabitattype_1 <- c("Other neutral grassland", "Wetland", "Woodland")
  enhancedhabitattype_2 <- c("No Habitats Enhanced")
  enhancedhabitattype_3 <- c("Other neutral grassland", "Other neutral grassland")
  
  expect_equal(exsum_enhancestring(enhancedhabitattype_1), "other neutral grassland, wetland and woodland")
  expect_equal(exsum_enhancestring(enhancedhabitattype_2), "nothing")
  expect_equal(exsum_enhancestring(enhancedhabitattype_3), "other neutral grassland")
})

# Test for exsum_loststring function
test_that("exsum_loststring works correctly", {
  losthabitattype_1 <- c("Other neutral grassland", "Wetland", "Woodland")
  losthabitattype_2 <- c("No Habitats Lost")
  losthabitattype_3 <- c("Other neutral grassland", "Other neutral grassland")
  
  expect_equal(exsum_loststring(losthabitattype_1), "other neutral grassland, wetland and woodland")
  expect_equal(exsum_loststring(losthabitattype_2), "nothing")
  expect_equal(exsum_loststring(losthabitattype_3), "other neutral grassland")
})
