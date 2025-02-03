library(testthat)

# Simulating the data for testing purposes
hedgelostdata <- data.frame(
  habitattype = c("Type A", "Type B", "Type A"),
  hedgenumber = c("H1", "H2", "H3"),
  lengthlost = c(1.5, 2.3, 0.8)
)

hedgeretaindata <- data.frame(
  habitattype = c("Type C", "Type B"),
  hedgenumber = c("H4", "H5"),
  lengthretained = c(2.0, 3.5)
)

hedgecreationdata <- data.frame(
  habitattype = c("Type D", "Type E"),
  hedgenumber = c("H6", "H7"),
  createdlength = c(1.2, 2.5),
  createdcondition = c("Good", "Fair"),
  createdss = c("High", "Medium"),
  distinctiveness = c("High", "Low")
)

hedgeenhancementdata <- data.frame(
  habitattype = c("Type F", "Type G"),
  hedgenumber = c("H8", "H9"),
  enhancedlength = c(1.3, 2.8),
  basecondition = c("Fair", "Poor"),
  basehabitattype = c("Type H", "Type I"),
  enhancedcond = c("Good", "Fair"),
  enhancedss = c("Medium", "High"),
  distinctiveness = c("Medium", "Low")
)

# Test function to check if output is generated
test_that("hedgelbupilost generates output", {
  sectioncounter <- 0
  expect_output(hedgelbupilost(hedgelostdata, sectioncounter), "7.1 Lost Hedgerow Biodiversity Units")
})

test_that("hedgelbupiretain generates output", {
  sectioncounter <- 1
  expect_output(hedgelbupiretain(hedgeretaindata, sectioncounter), "7.2 Retained Hedgerow Biodiversity Units")
})

test_that("hedgelbupicreate generates output", {
  sectioncounter <- 2
  expect_output(hedgelbupicreate(hedgecreationdata, sectioncounter), "7.3 Created Hedgerow Biodiversity Units")
})

test_that("hedgelbupienhance generates output", {
  sectioncounter <- 3
  expect_output(hedgelbupienhance(hedgeenhancementdata, sectioncounter), "7.4 Enhanced Hedgerow Biodiversity Units")
})
