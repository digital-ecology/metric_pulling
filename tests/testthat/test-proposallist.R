library(testthat)

# Define mock datasets
mock_enhancedata <- list(
  BroadEnhance = data.frame(V1 = c("Woodland", "Grassland")),
  EnhanceHabitatType = data.frame(V1 = c("Oak Forest", "Wildflower Meadow"))
)

mock_creationdata <- list(
  BroadCreation = data.frame(V1 = c("Wetland", "Scrubland")),
  CreationHabitatType = data.frame(V1 = c("Marsh", "Shrubland"))
)

test_that("feas_proposedlist returns a non-empty list", {
  result <- feas_proposedlist(mock_enhancedata, mock_creationdata)
  
  expect_type(result, "list")
  expect_true(length(result) > 0, "The function should return a non-empty list.")
  print(result)
})
