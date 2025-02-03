library(testthat)
library(dplyr)
library(tibble)

# Test data setup
waterlostdata <- tibble::tibble(habitattype = c("riparian", "wetland"),
                                lengthlost = c(1.5, 2.3))

waterretaindata <- tibble::tibble(habitattype = c("riparian", "wetland"),
                                  lengthretained = c(1.5, 2.3))

watercreationdata <- tibble::tibble(habitattype = c("riparian", "wetland"),
                                    createdlength = c(1.5, 2.3),
                                    createdcondition = c("good", "moderate"),
                                    createdss = c("high", "low"),
                                    distinctiveness = c("unique", "common"))

waterenhancementdata <- tibble::tibble(habitattype = c("riparian", "wetland"),
                                       enhancedlength = c(1.5, 2.3),
                                       basecondition = c("good", "fair"),
                                       basehabitattype = c("riparian", "wetland"),
                                       enhancedcond = c("improved", "enhanced"),
                                       enhancedss = c("high", "medium"),
                                       distinctiveness = c("high", "low"))

# Tests
test_that("waterlbupilost produces output", {
  expect_output(waterlbupilost(waterlostdata, 1), "##")
})

test_that("waterlbupiretain produces output", {
  expect_output(waterlbupiretain(waterretaindata, 1), "##")
})

test_that("waterlbupicreate produces output", {
  expect_output(waterlbupicreate(watercreationdata, 1), "##")
})

test_that("waterlbupienhance produces output", {
  expect_output(waterlbupienhance(waterenhancementdata, 1), "##")
})