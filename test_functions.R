# use a test dataset to make sure each function does what is expected

library(testthat)
library(stringr)
library(rapportools)

source("scripts/functions_SRS.r")
source("scripts/functions_EKB.r")

context("Checks all functions used in analysis")

testdat = read.csv("data/test_data_prepared.csv", stringsAsFactors = FALSE)

test_that("Check that tag cells without a tag get a unique id", {
  expect_equal(nrow(filter(id_unknowns(testdat, 16), grepl('\\d{7}', tag))), 4)
})


# Not working below this line --------------------------------------------------------------------


test_that("Add 's' to create unique id where star was", {
  expect_equal(str_detect("s", starred_tags(testdat, 16)[c(14,15,44,45), "tag"]), c(T,T,T,T))
})