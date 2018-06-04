# use a test dataset to make sure each function does what is expected

library(testthat)
library(stringr)
source(scripts/functions_SRS.r)
source(scripts/functions_EKB.r)

context("checks all functions used in analysis")

testdat = "data/test_data_prepared.csv"

# examples from testing rodents functions
test_that("Check that tag numbers match between the sheets and scanner", {
  expect_that(id_unknowns(testdat, 16), str_detect("s", testdat[c(14,15,44,45),tag]) = TRUE)
})

test_that("look for missing data ", {
  expect_equal(check_missing_data(testdat,c('month','day','year','period','plot')),c(5,15))
  expect_equal(check_missing_data(testdat[is.na(testdat$note1),],fields=c('stake','species','sex','hfl','wgt')),11)
})