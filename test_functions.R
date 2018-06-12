### use a test dataset to make sure each function does what is expected

# LIBRARIES, SOURCES, DATA
library(testthat)
library(dplyr)
library(stringr)
library(rapportools)
source("scripts/functions_SRS.r")
source("scripts/functions_EKB.r")
testdat = read.csv("data/test_data_prepared.csv", stringsAsFactors = FALSE)

context("Checks all functions used in analysis")

### get versions of data to be tested for all functions

# id_unknowns
testdat2 = id_unknowns(testdat, 16)

# starred_tags
tags2 = unique(testdat2$tag)
testdat3 = starred_tags(testdat2, tags2, 9, 16)

# find dead individuals
tags3 = unique(testdat3$tag)
testdat4 = is_dead(testdat3, tags3, 9, 16)

# find duplicate tags that stars and deaths don't fix
tags4 = unique(testdat4$tag)
dups = is_duplicate_tag(testdat4, tags4, 9, 16) 
duptags = unique(dups$bad$tag)
testdat5 = dups$data[-which(dups$data$tag %in% duptags),] 

# remove those caught in the same period 
tags5 = unique(testdat5$tag)
same = same_period(testdat5, tags5)
sametags = unique(same$tag)
testdat6 = testdat5[-which(testdat5$tag %in% sametags),]

# remember to test find_bad_data2()

# subset the data to remove data we don't want
testdat7 = subsetDat(testdat6)


### testthat functions

test_that("Check id_unknowns() function", {
  expect_equal(nrow(dplyr::filter(testdat2, grepl('\\d{7}', tag))), 4)
})

test_that("Check starred_tags() function", {
  expect_equal(length(unique(starred_tags(testdat2, tags2, 9, 16)$tag)), 37)
})

test_that("Check is_dead() function", {
  expect_equal(length(unique(is_dead(testdat3, tags3, 9, 16)$tag)), 38)
})

test_that("Check is_duplicate_tag() function", {
  expect_equal(unique(is_duplicate_tag(testdat4, tags4, 9, 16)$bad$tag), c("0003DM", "2509"))
})

test_that("Check same_period() function", {
  expect_equal(unique(same_period(testdat5, tags5)$tag), c("2520", "0004DM9s"))
})
 
testthat("Check SubsetDat() function", { # add a row in which spp is undetermined for this
  expect_equal()
})

# what about create_trmt_history function? Should that have a test?