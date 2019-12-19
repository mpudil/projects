#!/usr/bin/env Rscript

## To run the tests, just run
## Rscript test_tree_builder.R
library(testthat)

source("tree_builder.R")


example <- new_treemap(4, "ducks")
treemap_insert(example, 3, "walruses")
treemap_insert(example, 3.5, "penguins")
treemap_insert(example, 2, "seals")
treemap_insert(example, 5, "whales")
treemap_insert(example, 4.5, "otters")

test_that("treemap_search finds items", {
  expect_equal(treemap_search(example, 3), "walruses")
  expect_equal(treemap_search(example, 2), "seals")
})

test_that("treemap_search_between finds items", {
  expect_equal(treemap_search_between(example, 2, 3.5),
               list("seals", "walruses"))
})



# Edge cases

# Edge case 1: item in treemap_search does not exist

test_that("treemap_search provides NA if item does not exist", {
  expect_equal(treemap_search(example,-600), NA)
})


# Edge case 2: low and high values are outside the ranges of the keys of any nodes


test_that("treemap_search_between provides empty list if values are outside range ", {
  expect_equal(treemap_search_between(example,-600, -500), list())
  expect_equal(treemap_search_between(example,500, 600), list())
})


# Edge case 3: low and high values from treemap_search_between are included as keys 

test_that("treemap_search_between provides empty list if values are outside range ", {
  expect_equal(treemap_search_between(example,-5, 5), list("seals", "walruses", "penguins", "ducks", "otters"))
})



# Edge case 4: treemap_insert wants to insert a node that has the same key as another node

test_that("treemap_insert allows for duplicate keys and nodes", {
  treemap_insert(example, 4.5, "otters")
  expect_equal(treemap_search(example, 4.5), "otters")
})

