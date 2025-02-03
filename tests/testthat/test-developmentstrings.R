library(testthat)

# Test for developmentstring function
test_that("developmentstring works correctly", {
  
  # Test case 1: Lost habitat, hedge, and water
  losthabitattype_1 <- c("Other neutral grassland", "Wetland")
  losthedgetype_1 <- c("Hedgerow")
  lostwatertype_1 <- c("River")
  
  expect_equal(developmentstring(losthabitattype_1, losthedgetype_1, lostwatertype_1), "other neutral grassland, wetland, hedgerow and river")
  
  # Test case 2: Only habitat loss
  losthabitattype_2 <- c("Other neutral grassland")
  losthedgetype_2 <- c("No Hedges Lost")
  lostwatertype_2 <- c("No Watercourses Lost")
  
  expect_equal(developmentstring(losthabitattype_2, losthedgetype_2, lostwatertype_2), "other neutral grassland")
  
  # Test case 3: Only hedge loss
  losthabitattype_3 <- c("No Habitats Lost")
  losthedgetype_3 <- c("Hedgerow")
  lostwatertype_3 <- c("No Watercourses Lost")
  
  expect_equal(developmentstring(losthabitattype_3, losthedgetype_3, lostwatertype_3), "hedgerow")
  
  # Test case 4: Only water loss
  losthabitattype_4 <- c("No Habitats Lost")
  losthedgetype_4 <- c("No Hedges Lost")
  lostwatertype_4 <- c("River")
  
  expect_equal(developmentstring(losthabitattype_4, losthedgetype_4, lostwatertype_4), "river")
  
  # Test case 5: No habitats, hedges, or water lost
  losthabitattype_5 <- c("No Habitats Lost")
  losthedgetype_5 <- c("No Hedges Lost")
  lostwatertype_5 <- c("No Watercourses Lost")
  
  expect_equal(developmentstring(losthabitattype_5, losthedgetype_5, lostwatertype_5), "nothing")
  
  # Test case 6: More than two unique types
  losthabitattype_6 <- c("Other neutral grassland", "Woodland")
  losthedgetype_6 <- c("Hedgerow", "No Hedges Lost")
  lostwatertype_6 <- c("River", "Stream")
  
  expect_equal(developmentstring(losthabitattype_6, losthedgetype_6, lostwatertype_6), 
               "other neutral grassland, woodland, hedgerow, no hedges lost, river and stream")
  # Test case 7: Only one unique type
  losthabitattype_7 <- c("Other neutral grassland")
  losthedgetype_7 <- c("No Hedges Lost")
  lostwatertype_7 <- c("No Watercourses Lost")
  
  expect_equal(developmentstring(losthabitattype_7, losthedgetype_7, lostwatertype_7), "other neutral grassland")
})
