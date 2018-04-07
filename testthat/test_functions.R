# use a test dataset to make sure each function does what is expected

library(testthat)
source(functions_SRS.r)
source(functions_EKB.r)

context("checks all functions used in analysis")

testdat = "data/test_data.csv"

# examples from testing rodents functions
test_that("Check that tag numbers match between the sheets and scanner", {
  expect_equal(compare_tags(testdat,scannerfile),data.frame(where=c('scan','sheet'),tag=c('B267E8','B267EB')))
})

test_that("look for missing data ", {
  expect_equal(check_missing_data(testdat,c('month','day','year','period','plot')),c(5,15))
  expect_equal(check_missing_data(testdat[is.na(testdat$note1),],fields=c('stake','species','sex','hfl','wgt')),11)
})