#!/usr/bin/env Rscript
## Rscript test_information_retrieval_bow.R
library(testthat)
library(here)

source("information_retrieval_bow.R")
artfiles <- paste0("Data/nyt-collection-text/art/", 
                   list.files(here("Data", "nyt-collection-text", "art")))

musicfiles <- paste0("Data/nyt-collection-text/music/", 
                     list.files(here("Data", "nyt-collection-text", "music")))

punctuation <- paste0("Data/Punctuation/", list.files(here("Data", "Punctuation")))

# Vignette pt 1 tests -----------------------------------------------------

# Normal cases (with punctuation)

foo <- "I like %$@to*&, chew;: gum, but don't like|}{[] bubble@#^)( gum!?"

test_that("bag of words counts tokens", {
  
  bow <- tokenize(foo) %>% makeBoW
  gum_indx <- which(names(bow)=="gum")
  like_indx <- which(names(bow)=="gum")
  expect_equal(bow[gum_indx], bow[like_indx])
  
})


# Edge cases


bar <- "foo"

test_that("tokenize function keeps character length 1 the same", {
  expect_equal(tokenize(bar), c("foo"))
})

test_that("bag of words for character length 1 returns token as name with count of 1", {
  bow <- makeBoW(tokenize(bar), "foo")
  expect_equal(which(colnames(bow)=="foo"), 1)
  expect_equal(bow[1],1) 
})


test_that("makeBoW leaves out words in lexicon but not in tokenlist", {
  lex <- tokenize(foo) %>% unique
  bow_superman <- makeBoW(tokenize(foo), c(lex, "superman"))
  bow_reg <- makeBoW(tokenize(foo))
  expect_equal(ncol(bow_superman), ncol(bow_reg))
})

test_that("makeBoW can handle punctuation", {
  p <- makeBoW(tokenize(punctuation[1]))
  expect_equal(colnames(p), c("and", "cook", "do", "eating", "enjoy", "food", 
                              "i", "it", "like", "make", "to", "too", "you"))
  expect_equal(as.vector(p), c(2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 2, 1, 1))
  
  p2 <- makeBoW(tokenize(punctuation[2]))
  expect_equal(colnames(p2), c("are", "both", "can", "cook", "do", "good", "hamburgers", "i", "like", 
                               "make", "or", "pizza", "really", "to", "yes", "you"))
  expect_equal(as.vector(p2), c(1, 1, 1, 2, 1, 1, 1, 2, 2, 1, 1, 1, 1, 2, 1, 1))
  
  
  p3 <- makeBoW(tokenize(punctuation[3]))
  expect_equal(colnames(p3), c("and", "classs", "cook", "does", "food", "likes", "mom", "my", "she", "to",     
                               "took", "well"))
  expect_equal(as.vector(p3), c(1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1))
  
})

# Vignette pt 2 tests -----------------------------------------------------

test_that("No duplicate words and no missing words (even with punctuation", {
  pac <- analyzeCorpus(punctuation)
  expect_equal(colnames(pac), c("and", "cook", "do", "eating", "enjoy", "food", "i", "it", "like",      
                                "make", "to", "too", "you", "are", "both", "can", "good", "hamburgers",
                                "or", "pizza", "really", "yes", "classs", "does", "likes", "mom", "my",        
                                "she", "took", "well"))
})


test_that("Counts are correct in analyzeCorpus", {
  pac <- analyzeCorpus(punctuation)
  expect_equal(pac$food == c(1,0,1), rep(TRUE, 3))
  expect_equal(pac$i, c(1,2,0))
  expect_equal(pac$enjoy, c(1,0,0))
  expect_equal(dim(pac), c(3,30))
})

test_that("rownames are basenames of the file without the extension", {
  expect_equal(rownames(analyzeCorpus(musicfiles[1:3])), c("0023931.txt", "0075170.txt", "0170797.txt"))
})


test_that("No missing values or non-numeric or negative values in dataframe", {
  # Note: Converting to numeric via as.numeric(as.character(.)) will convert non-numbers to NA.
  expect_equal(analyzeCorpus(musicfiles)[1,] %>% as.character %>% as.numeric %>% is.na %>% sum, 0)
})

test_that("analyzeCorpus produces appropriate error when an illigitimate filename is used as input", {
  expect_error(analyzeCorpus(c("doesnotexist.txt", "notafile.txt")))
  
  # And even when only one of the filenames does not exist
  expect_error(analyzeCorpus(c(artfiles[1], "doesnotexist.txt")))
})
# Vignette #3 Tests

## To run the tests, just run
## Rscript test_information_retrieval_bow.R
library(testthat)

source("information_retrieval_bow.R")


# Normal Cases ------------------------------------------------------------


mat <- matrix(nrow=3, ncol=3, data=c(1:9))


test_that("distMatrix function reports accurate euclidean distance between 2 vectors", {
  expect_equal(round(distMatrix(mat),2), matrix(nrow=3, ncol=3, byrow=T, data=c(0, 1.73, 3.46,
                                                                                1.73, 0, 1.73,
                                                                                3.46, 1.73, 0)))
  expect_equal(distMatrix(mat, metric="max"), matrix(nrow=3, ncol=3, byrow=T, data=c(0, 1, 2,
                                                                                     1, 0, 1,
                                                                                     2, 1, 0)))
  
})

test_that("Diagonals are 0", {
  expect_equal(diag(distMatrix(mat)), rep(0,3))
  expect_equal(diag(distMatrix(mat, metric="max")), rep(0,3))
})

test_that("Matrices are symmetric",{
  expect_true(isSymmetric.matrix(distMatrix(mat)))
  expect_true(isSymmetric.matrix(distMatrix(mat, metric="max")))
})




# Edge Cases --------------------------------------------------------------


test_that("Distance Matrix of Zero matrix is a Zero matrix", {
  zeromat <- matrix(nrow=3, ncol=3, data=rep(0, 9))
  expect_equal(distMatrix(zeromat), zeromat)
  expect_equal(distMatrix(zeromat, metric = "max"), zeromat)
})


test_that("Small matrices work", {
  smallmat <- matrix(nrow=2, ncol=1, data=c(1,2))
  
  expect_equal(distMatrix(smallmat), 
               matrix(nrow=2, ncol=2, byrow=T, data=c(0,1,1,0)))
  
  expect_equal(distMatrix(smallmat, metric = "max"), 
               matrix(nrow=2, ncol=2, byrow=T, data=c(0,1,1,0)))
})


test_that("Negative numbers throw an error", {
  negmatrix <- matrix(nrow=2, ncol=2, data=c(-4, -3, -1, 3))
  expect_error(distMatrix(negmatrix))
})



# Vignette pt 4 Tests -----------------------------------------------------

punct1 <- as.data.frame(makeBoW(tokenize(punctuation[1])))
rownames(punct1) <- basename(punctuation[1])
punct23 <- analyzeCorpus(punctuation[2:3])

music110 <- analyzeCorpus(musicfiles[1:10])
music1 <- makeBoW(tokenize(musicfiles[1])) %>% data.frame
rownames(music1) <- musicfiles[1]
music210 <- analyzeCorpus(musicfiles[2:10])


test_that("Inputs are valid", {
  expect_error(IrSearch(punct1, punct23, k=0))
  expect_error(IrSearch(punct1, punct23, k=10))
  expect_error(IrSearch(punct1, punct23, method = "invalid_method"))
  expect_error(IrSearch(punct1, c(1:ncol(punct1))))
})

test_that("Correct searches from punct files", {
  expect_equal(IrSearch(punct1, punct23), "punct3.txt")
  expect_equal(IrSearch(punct1, punct23, method = "maximum"), "punct2.txt")
  expect_equal(IrSearch(punct1, punct23, k=2), c("punct3.txt", "punct2.txt"))
})


test_that("Doesn't matter if doc_bow is a row in corp_bow", {
  expect_equal(IrSearch(music1, music110), IrSearch(music1, music210))
  expect_true(all(IrSearch(music1, music110, k=2) == IrSearch(music1, music210, k=2)))
  m1 <- IrSearch(music1, music110, k=2, method="maximum")
  m2 <-  IrSearch(music1, music210, k=2, method="maximum")
  expect_true(all(m1 == m2))
})