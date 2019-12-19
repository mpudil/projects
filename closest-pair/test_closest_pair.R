#!/usr/bin/env Rscript

## To run the tests, just run
## Rscript test_closest_pair.R
library(here)
source(here("closest-pair", "closest_pair.R"))
library(testthat)
library(tidyverse)


# Closest Point Subset Tests ----------------------------------------------

# Normal case

test_that("Closest point subset finds closest point in small dataframe", {
  df <- data.frame(x=c(1:3), y=c(4.2,4.1,1))
  cps <- closest_points_subset(df)
  expect_equal(round(cps$dist, 2), 1)
  expect_equal(cps$points, data.frame(x=c(1,2), y=c(4.2,4.1)))
})

# Edge case 1: Multiple closest points

test_that("Closest point subset returns only one pair of points", {
  df <- data.frame(x=c(1:3), y=c(1:3))
  cps <- closest_points_subset(df)
  expect_equal(round(cps$dist, 2), 1.41)
  expect_equal(cps$points, data.frame(x=c(1,2), y=c(1,2)))
})


# Subset Points Function --------------------------------------------------

# Normal Case

test_that("Subset Points separates points correctly", {
  df <- data.frame(x=c(1:10), y=c(1:10))
  sep <- subset_points(df)
  rownames(sep$L) <- rownames(sep$R) <- NULL
  expect_equal(sep$m, 5)
  expect_equal(sep$L, data.frame(x=c(1:5), y=c(1:5)))
  expect_equal(sep$R, data.frame(x=c(6:10), y=c(6:10)))
})

# Edge Cases

test_that("Subset Points separates odd number of points", {
  df <- data.frame(x=c(1:11), y=c(1:11))
  sep <- subset_points(df)
  rownames(sep$R) <- NULL
  expect_equal(sep$m, 6)
  expect_equal(sep$L, data.frame(x=c(1:6), y=c(1:6)))
  expect_equal(sep$R, data.frame(x=c(7:11), y=c(7:11)))
})


test_that("Subset Points does not attempt to separate single row", {
  df <- data.frame(x=1, y=1)
  expect_error(subset_points(df))
})

test_that("Subset Points requires x has more than 1 unique value", {
  df <- data.frame(x=rep(1,5), y=c(1:5))
  expect_error(subset_points(df))
})




# Middle Points Function --------------------------------------------------

# Normal Case

test_that("Middle points finds and separates points within boundary", {
  left <- data.frame(x=c(1:10), y=c(4,5,3,4,55,2,1,5.6,-7,3))
  right <- data.frame(x=c(11:20), y=c(4,5,3,4,55,2,1,5.6,-7,3))
  mid <- middle_points(left, right, cutoff=10.5, width = 2 )
  expect_equal(mid$left_middle, data.frame(x=c(10,9), y=c(3,-7)))
  expect_equal(mid$right_middle, data.frame(x=c(11,12), y=c(4,5)))
})


# Edge Cases

test_that("Points on edge of cutoff included", {
  left <- data.frame(x=c(1:11), y=c(4,5,3,4,55,2,1,5.6,-7,3, .4))
  right <- data.frame(x=c(11:20), y=c(4,5,3,4,55,2,1,5.6,-7,3))
  m <- middle_points(left, right, cutoff=11, width=2)
  m$left_middle <- data.frame(x=c(11:9), y=c(0.4, 3, -7))
  m$right_middle <- data.frame(x=c(11:13), y=c(4,5,3))
})


test_that("Returns NA if no points within the cutoff for left OR right OR both", {
  left <- data.frame(x=c(1:10), y=c(4,5,3,4,55,2,1,5.6,-7, .4))
  right <- data.frame(x=c(12:20), y=c(4,5,3,4,55,2,1,-7,3))
  m <- middle_points(left, right, cutoff=11, width=0.1)
  expect_true(is.na(m))
  
  right <- data.frame(x=c(11.01:20.1), y=c(4,5,3,4,55,2,1,-7,3, 7))
  m <- middle_points(left, right, cutoff=11, width=0.1)
  expect_true(is.na(m))
})


test_that("Returns same df if all data is within bounds (though left is reordered", {
  left <- data.frame(x=c(1:10), y=c(4,5,3,4,55,2,1,5.6,-7, .4))
  right <- data.frame(x=c(12:20), y=c(4,5,3,4,55,2,1,-7,3))
  m <- middle_points(left, right, cutoff=11, width=100)
  lexpected <- left[order(left$x, decreasing=T),]
  rownames(lexpected) <- NULL
  expect_equal(m$left_middle, lexpected )
  expect_equal(m$right_middle, right)
})



# Compare Middle ----------------------------------------------------------

# Normal case

test_that("Compare middle finds closest points across boundaries", {
  mleft <- data.frame(x=c(1:3), y=c(7.5, 4.1, 4.4))
  mright <- data.frame(x=c(4:6), y=c(4,5,3))
  mclose <- compare_middle(mleft, mright)
  expect_equal(mclose$points, data.frame(x=c(3,4), y=c(4.4,4.0)))
  expect_equal(round(mclose$dist, 2), 1.08)
})

# Edge cases
# Finds closest point across boundaries, even if there is a closer point between two points on one side

test_that("Does not compare points on same side of line", {
  mleft <- data.frame(x=c(1, 0.8), y=c(5, 5.1))
  mright <- data.frame(x=2, y=10)
  mclose <- compare_middle(mleft, mright)
  lrclose <- closest_points_subset(rbind(mleft, mright))
  expect_gt(mclose$dist, lrclose$dist)
})


test_that("Finds closes point even when there is only one point on each side", {
  mleft <- data.frame(x=1, y=2)
  mright <- data.frame(x=4, y=4)
  mclose <- compare_middle(mleft, mright)
  expect_equal(round(mclose$dist, 2), 3.61)
})



# ClosestPairs (Main) Function -------------------------------------------------

# Normal Case

test_that("Finds closest point in small dataframe", {
  df <- data.frame(x=c(1:10), y=c(4.2,4.1,1,0.5,0.9,4,2,1.7,1.9, 5.5))
  cps <- closest(df)
  expect_equal(round(cps$dist, 2), 1)
  expect_equal(cps$points, data.frame(x=c(1,2), y=c(4.2,4.1)))
})


# Edge cases


test_that("Requires correct input assumptions", {
  # Does not allow NA
  df <- data.frame(x=c(1, NA), y=c(1, NA))
  expect_error(closestPairs(df))
  
  # Not df
  notdf <- c(1,2,3,4)
  expect_error(closestPairs(notdf))
  
  # ncol not 2
  bigdf <- data.frame(x=c(1:3), y=c(2,4.3,1), z=c(4.5,2.2,5))
  expect_error(closestPairs(bigdf))
  smalldf <- data.frame(x=c(1:3))
  expect_error(closestPairs(smalldf))
  
})


test_that("Finds closest point in large dataframe quickly and accurately", {
  challenge <- read.table(here("closest-pair", "Data", "closest-pairs1.txt")) %>%
    data.frame
  colnames(challenge) <- c("x", "y")
  
  start_time <- Sys.time()
  cp <- closestPairs(challenge)
  end_time <- Sys.time()
  closetime <- end_time - start_time
  
  expect_gte(3, closetime)
  
})


test_that("Only reports one pair, even if multiple points have same dist", {
  df <- data.frame(x=1:10, y=1:10)
  cp <- closestPairs(df)
  expect_equal(round(cp$dist, 2), 1.41)
  expect_equal(cp$points$x, c(1,2))
  expect_equal(cp$points$y, c(1,2))
})
