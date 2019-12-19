#!/usr/bin/env Rscript

## Your assignment code goes here.
## Build trees.
##
## tree_builder_util.R defines a thing called a Node. A node stores a key, a
## value, and left and right children. You can access and change these with
## ordinary R dollar-sign notation, and the new_treemap function below creates a
## Node with a key and a value.
##
## Example:
# n <- new_treemap(4, "walruses")
# n$key   #=> 4
# n$value #=> "walruses"

# n2 <- new_treemap(5, "ducks")
# n$right <- n2


##
## A node's $left and $right are NULL until they are set; you can check this
## with is.null(node$left) and is.null(node$right).
##
## Consult test_tree_builder.R for example uses of the functions below.

library(R6)
library(assertthat)

source("tree_builder_util.R")

#' Initialize a treemap with one key and one value.
#'
#' @param key A key. The keys must be R values that can be compared with < and
#'     ==.
#' @param value A value. Treemaps can store any type of value.
#'
#' @return A treemap containing one key-value pair.
new_treemap <- function(key, value) {
  return(Node$new(key, value))
}

#' Insert one item into a treemap.
#'
#' @param treemap The treemap (e.g. one made with new_treemap()) to insert into.
#' @param key The key.
#' @param value The value
#'
#' @return The treemap, updated with the new item.
treemap_insert <- function(treemap, key, value) {
  if (key < treemap$key) {
    if (is.null(treemap$left)) {
      treemap$left <- new_treemap(key, value)
    } else {
      treemap_insert(treemap$left, key, value)
    }
    
  } else {
    if (is.null(treemap$right)) {
      treemap$right <- new_treemap(key,value)
    } else {
      treemap_insert(treemap$right, key, value)
    }
  }
  
  ## Return the new treemap, but without printing it to the console.
  invisible(treemap)
}

#' Find an item in the treemap, using its key, and return its value.
#'
#' @param treemap The treemap to search.
#' @param key The key to search for.
#'
#' @return The value corresponding to the key, or NA if no such key is stored in
#'     the treemap.
treemap_search <- function(treemap, key) {
  if (is.null(treemap)) {
    return(NA)
  }
  
  if (key < treemap$key) {
    tree_val <- treemap_search(treemap$left, key)
    return(tree_val)
  } 
  
  else if (key == treemap$key) {
    return(treemap$value)
  } 
  
  else {
    tree_val <- treemap_search(treemap$right, key)
  }
}

#' Find all items in the treemap such that key_lo <= key < key_hi.
#'
#' @param treemap The treemap to search.
#' @param key_lo Find keys greater than or equal to this key.
#' @param key_hi Find keys strictly less than this key.
#'
#' @return A list of all values with keys in this range, in order of their keys.
#'     If no values are found, returns an empty list.
treemap_search_between <- function(treemap, key_lo, key_hi) {
  assert_that(key_lo < key_hi)
  
  if (is.null(treemap)) {
    return(list())
  }
  
  if (treemap$key >= key_hi) {
    return(treemap_search_between(treemap$left, key_lo, key_hi))
  }
  
  if (treemap$key < key_lo) {
    return(treemap_search_between(treemap$right, key_lo, key_hi))
  }
  
  return(
    c(treemap_search_between(treemap$left, key_lo, key_hi),
      treemap$value,
      treemap_search_between(treemap$right, key_lo, key_hi))
  )
}



