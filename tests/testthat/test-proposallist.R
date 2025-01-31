library(testthat)

# Test 1: Check the correct merging of creation and enhancement lists
test_that("Correctly generates proposed habitat list", {
  # Sample data for creation and enhancement
  creationdata <- list(
    BroadCreation = data.frame(c("Site1", "Site2")),
    CreationHabitatType = data.frame(c("TypeA", "TypeB"))
  )
  enhancedata <- list(
    BroadEnhance = data.frame(c("Site3", "Site4")),
    EnhanceHabitatType = data.frame(c("TypeC", "TypeD"))
  )
  
  # Call the function
  proposedlist <- feas_proposedlist(enhancedata, creationdata)
  
  # Check that the function returns a combined list with unique entries
  expected_list <- c(
    "Site1 - TypeA \n",
    "Site2 - TypeB \n",
    "Site3 - TypeC \n",
    "Site4 - TypeD \n"
  )
  
  expect_equal(proposedlist, expected_list)
})

# Test 2: Ensure unwanted entries are removed
test_that("Removes unwanted 'No Habitats' entries", {
  # Sample data with unwanted entries
  creationdata <- list(
    BroadCreation = data.frame(c("Site1", "No Habitats Created")),
    CreationHabitatType = data.frame(c("TypeA", "No Habitats Created"))
  )
  enhancedata <- list(
    BroadEnhance = data.frame(c("Site2", "No Habitats Enhanced")),
    EnhanceHabitatType = data.frame(c("TypeB", "No Habitats Enhanced"))
  )
  
  # Call the function
  proposedlist <- feas_proposedlist(enhancedata, creationdata)
  
  # Check that the unwanted entries are removed
  expected_list <- c(
    "Site1 - TypeA \n",
    "Site2 - TypeB \n"
  )
  
  expect_equal(proposedlist, expected_list)
})

# Test 3: Ensure duplicates are removed
test_that("Removes duplicate habitat types", {
  # Sample data with duplicate entries
  creationdata <- list(
    BroadCreation = data.frame(c("Site1", "Site1")),
    CreationHabitatType = data.frame(c("TypeA", "TypeA"))
  )
  enhancedata <- list(
    BroadEnhance = data.frame(c("Site2", "Site2")),
    EnhanceHabitatType = data.frame(c("TypeB", "TypeB"))
  )
  
  # Call the function
  proposedlist <- feas_proposedlist(enhancedata, creationdata)
  
  # Check that the duplicate entry is removed
  expected_list <- c(
    "Site1 - TypeA \n",
    "Site2 - TypeB \n"
  )
  
  expect_equal(proposedlist, expected_list)
})

# Test 4: Check handling of empty input data
test_that("Handles empty input data gracefully", {
  # Empty data inputs
  creationdata <- list(
    BroadCreation = data.frame(),
    CreationHabitatType = data.frame()
  )
  enhancedata <- list(
    BroadEnhance = data.frame(),
    EnhanceHabitatType = data.frame()
  )
  
  # Call the function
  proposedlist <- feas_proposedlist(enhancedata, creationdata)
  
  # Ensure the function returns an empty list
  expect_equal(proposedlist, character(0))
})

# Test 5: Check handling of input data with only one type of entry
test_that("Handles single type of data correctly", {
  # Only creation data
  creationdata <- list(
    BroadCreation = data.frame(c("Site1", "Site2")),
    CreationHabitatType = data.frame(c("TypeA", "TypeB"))
  )
  enhancedata <- list(
    BroadEnhance = data.frame(),
    EnhanceHabitatType = data.frame()
  )
  
  # Call the function
  proposedlist <- feas_proposedlist(enhancedata, creationdata)
  
  # Check that only the creation list is included
  expected_list <- c(
    "Site1 - TypeA \n",
    "Site2 - TypeB \n"
  )
  
  expect_equal(proposedlist, expected_list)
})
