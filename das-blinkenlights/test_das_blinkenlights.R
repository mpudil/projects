#!/usr/bin/env Rscript

## To run the tests, just run
## Rscript test_das_blinkenlights.R
library(testthat)

source("das_blinkenlights.R")

library(testthat)
library(magrittr)

set.seed(12)


test_that("Random inputs for naiive and fast versions give same output", {
  # Note that we will only be testing up to 10^5 since the naiive version gets slow after that
  blink_fails <- data.frame(matrix(ncol=2))
  colnames(blink_fails) <- c("state", "B")
  
  n_changes <- sample(3:10^5, 100, replace=TRUE)
  state_lengths <- sample(3:16, 100, replace=TRUE)
  for(i in 1:length(state_lengths)) {
    config <- sample(c(0,1), state_lengths[i], replace=TRUE)
    if(any(blink(config, n_changes[i]) != blink_naiive(config, n_changes[i]))) {
      blink_fails <- rbind(blink_fails, c(config %>% vec_to_string, n_changes[i]))
    }
  }
  
  blink_fails <- na.omit(blink_fails)
  
  if(nrow(blink_fails) > 0){
    print(blink_fails)
  }
  
  expect_equal(nrow(blink_fails), 0)
  
})


test_that("If pattern is 0 blink returns a vector of 0's with same length as B (number of bulbs)", {
  for(i in 3:16){
    expect_true(all(blink(rep(0,i), 100) == rep(0,i)))
  }
})


test_that("blink is correct even if the number of bulbs is perfectly divisible by length of pattern - distance to pattern", {
  b_quick <- blink(c(1,0,0,1,0,0), 111)
  b_slow <- blink_naiive(c(1,0,0,1,0,0), 111)
  
  expect_true(all(b_quick == b_slow))
})


test_that("Bad inputs produce error", {
  expect_error(blink(c(1,1,0,0), 0)) # Number of bulb changes must be between 1 and 10^15
  expect_error(blink(c(1,1,0,0), 10^16)) # Number of bulb changes must be between 1 and 10^15
  expect_error(blink(rep(0, 1), 10))# Number of bulbs must be between 3 and 16
  expect_error(blink(rep(0, 100), 10))# Number of bulbs must be between 3 and 16
})


test_that("blink works when number of bulbs is less than the length of the pattern", {
  s <- c(0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 1, 1)
  expect_true(all(blink(s, 2) == blink_naiive(s,2)))
})