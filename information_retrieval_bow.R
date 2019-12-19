# Vignette Part 1

library(readtext)
library(tidyverse)
library(stringr)
library(data.table)
library(dplyr)
library(tibble)
library(stringi)

#' Checks if the string provided is a text file
#'
#' @param doc A string or .txt file
#' @return A boolean for whether or not the string is a .txt file
#'
#' @examples
#' is_txt("I love pizza")
#' is_txt("foo.txt")

is_txt <- function(doc) {
  grepl("\\.txt$", doc)
}


#' Creates vector of counts associated with the different tokens in a document
#'
#' @param doc A string or txt file to be tokenized
#' @return A vector of words in the document in the order in which they appear
#'
#' @examples
#' tokenize("I don't like to chew gum; do you?")
#' tokenize("foo.txt")


tokenize <- function(doc) {
  
  # Determine if document or .txt file
  ifelse(is_txt(doc), readtext(doc)$text[[1]] %>% 
           tolower %>%
           gsub("[\r\n]", " ", .) %>%
           str_split(" "), doc) %>% unlist %>%
    # Get rid of punctuation, but keep apostophes where appropriate
    strsplit(" ") %>% gsub('[[:punct:] ]+','',.) %>% 
    stri_remove_empty
}

#' Creates bag of words vector from a vector of strings
#' @param tokenlist A vector where each element is a single word
#' @param lexicon Ordered vector representing the lexicon
#' @return Frequencies for each word in the tokenlist vector
#'
#' @examples
#' makeBoW(c("I", "don't", "drink"))


makeBoW <- function(tokenlist, lexicon=NA) {
  lexicon <- ifelse(is.na(lexicon), tokenlist %>%
                      unique %>% str_sort, lexicon)
  bow <- tokenlist %>% table %>% data.frame %>%
    arrange(factor(., levels = lexicon)) %>%
    tibble::column_to_rownames(".") %>% t
  return(bow)
}


# Part 2 Functions --------------------------------------------------------



#' Creates a dataframe of token counts for each file
#' @param filenames a list of .txt files
#' @return Dataframe where row is the basename of the file
#' and the columns are the unique tokens from the filenames.
#'
#' @examples
#' analyzeCorpus(c("foo.txt", "bar.txt"))


analyzeCorpus <- function(filenames) {
  assertthat::assert_that(is.character(filenames))
  assertthat::assert_that(length(filenames) > 1)
  filenames %>% purrr::walk(.x = ., .f = ~assertthat::is.readable(.x))
  
  bow_df <- sapply(filenames, function(x) makeBoW(tokenize(x)))
  sep_dfs <- lapply(bow_df, function(x) data.frame(x))
  merged_dfs <- data.frame(rbindlist(sep_dfs, fill=TRUE))
  merged_dfs[is.na(merged_dfs)] <- 0
  rownames(merged_dfs) <- basename(filenames)
                    
  return(merged_dfs)
}



# Part 3 Functions --------------------------------------------------------

#' Calculates Euclidean Distance between two vectors
#' @param vec1 numeric vector
#' @param vec2 numeric vector of the same length as vec1
#' @return Euclidean Distance between the two vectors
#'
#' @examples
#' x <- c(1:10)
#' y <- c(11:20)
#' eucDistance(x, y)

eucDistance <- function(vec1, vec2){
  assertthat::are_equal(ncol(vec1), ncol(vec2))
  sum((vec1-vec2)^2) %>% sqrt
}

#' Calculates Max Distance between two vectors
#' @param vec1 numeric vector
#' @param vec2 numeric vector of the same length as vec1
#' @return Max Distance between the two vectors
#'
#' @examples
#' x <- c(1:10)
#' y <- c(11:20)
#' maxDistance(x, y)

maxDistance <- function(vec1, vec2){
  max(abs(vec1-vec2))
}


#' Produces a symmetric matrix from the output of analyzeCorpus
#' @param corpusmatrix Dataframe or matrix of counts of unique tokens 
#' @param metric The distance metric to calculate the rows. Can be "euclidean"
#' or "max"
#' for each document
#' @return Symmetric matrix of pairwise distances between documents in the corpus


distMatrix <- function(corpusmatrix, metric="euclidean") {
  assertthat::assert_that(min(corpusmatrix) >= 0)
  if(metric=="euclidean"){
  m <- sapply(1:nrow(corpusmatrix), function(i) sapply(1:nrow(corpusmatrix), 
                                                  function(j) eucDistance(corpusmatrix[i,], corpusmatrix[j,])))
  }
  
  if(metric == "max"){
    m <- sapply(1:nrow(corpusmatrix), function(i) sapply(1:nrow(corpusmatrix), 
                                                         function(j) maxDistance(corpusmatrix[i,], corpusmatrix[j,])))
  }
  
  return(m)
}