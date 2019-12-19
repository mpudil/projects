library(tidyverse)


#' Find distance and smallest points by comparing all points in dataframe
#'
#' @param sub_points dataframe of row length 2 or 3
#' @return smallest distance and related points

closest_points_subset <- function(sub_points){
  smallest_d <- Inf
  for(i in 1:nrow(sub_points)){
    for(j in 1:nrow(sub_points)){
      if((sub_points[i,]==sub_points[j,]) %>% sum > 0){
        next
      }
      
      distance <- dist(rbind(sub_points[i,], sub_points[j,])) %>% as.numeric()
      if(distance < smallest_d){
        smallest_d <- distance
        closest_pair <- rbind(sub_points[i,], sub_points[j,])
      }
    }
  }
  
  return(list(dist = smallest_d, points = closest_pair))
  
}

#' Breaks dataset into Left and Right  by finding the median of the x points.
#' IMPORTANT: Dataframe must be pre-sorted by x.
#'
#' @param df_points dataframe with colnames "x" and "y" 
#' @return median, Left side points, Right side points


subset_points <- function(df_points) {
  assertthat::assert_that(ncol(df_points) > 1)
  assertthat::assert_that(df_points$x %>% unique %>% length > 1)
  
  m <- df_points[round(nrow(df_points)/2), "x"]
  m.index <- which(df_points$x == m)[1]
  L <- df_points[c(1:m.index),] # Left points are all points up to m index
  
  right.start <- m.index + 1
  R <- df_points[right.start:nrow(df_points),]
  return(list(m=m, L=L, R=R))
}



#' Subsets df to points that fall within a certain boundary. 
#' A huge shortcut here is to realize that if the left points are 
#' ordered highest to lowest by x value and the right points are 
#' ordered lowest to highest by x value, then all that is needed
#' is to take the first points of each df until the x-values fall
#' outside the cutoff +- width boundary.
#' 
#' IMPORTANT: left and right must be sorted beforehand
#'
#' @param left data points left of the cutoff
#' @param right data points right of the cutoff
#' @param cutoff cutoff point 
#' @param width width from one side of the cutoff
#' @return Left middle points, Right middle points


middle_points <- function(left, right, cutoff, width){
  
  # Check to see that there is at least one point in the middle for the left and right sides
  if(left$x[nrow(left)] < cutoff - width | 
     right$x[1] > cutoff + width){
    return(NA)
  }
  
  
  left_middle_rows <- NA
  i <- nrow(left) # Starts at end and works backwards
  
  while(i > 0 && left$x[i] >= cutoff-width){
    left_middle_rows <- c(left_middle_rows, i)
    i <- i-1
  }
  
  left_middle <- left[left_middle_rows[-1],]
  
  
  right_middle_rows <- NA
  i <- 1
  while(i <= length(right$x) && right$x[i] <= cutoff+width){
    right_middle_rows <- c(right_middle_rows, i)
    i <- i+1
  }
  right_middle <- right[right_middle_rows[-1],]
  rownames(left_middle) <- rownames(right_middle) <- NULL
  
  return(list(left_middle = left_middle, right_middle = right_middle))
  
}


#' Compares points that are across the cutoff from each other 
#'
#'EDIT TO MAKE: SORT BY Y AND ONLY COMPARE POINTS TO NEXT 3 POINTS
#'
#' @param middle_left middle left data points
#' @param middle_right middle right data points
#' @return closest points across boundary & distance btwn

compare_middle <- function(middle_left, middle_right){
         
  lowest_middle_dist <- 'inf'
  closest_points <- data.frame(x=c(NA, NA), y=c(NA, NA))
  
  for(lmp in 1:nrow(middle_left)){
    for(rmp in 1:nrow(middle_right)){
      distance <- dist(rbind(middle_left[lmp,], middle_right[rmp,])) %>%
        as.numeric()
      if(distance < lowest_middle_dist){
        lowest_middle_dist <- distance
        closest_points <- rbind(middle_left[lmp,], middle_right[rmp,])
      }
    }
  }
  rownames(closest_points) <- NULL
  return(list(points=closest_points, dist=lowest_middle_dist))
}



#' Uses recursion to find smallest distance and related points
#'
#' @param df_points dataframe with colnames "x" and "y"
#' @return smallest distance and related points


closest <- function(df_points) {
  
  if(nrow(df_points) == 2 | nrow(df_points) == 3) {
    return(closest_points_subset(df_points))
  } else { 
    # Subset the data
    s <- subset_points(df_points)
    med <- s$m
    L1 <- s$L
    R1 <- s$R
    
    # Use recursion to break up dataset even more
    L2 <- closest(L1) # list with dist and points attributes
    R2 <- closest(R1)
    
    
    l <- L2$points
    r <- R2$points
    
    lr_min_idx <- which.min(c(dist(l),dist(r)))
    lr_min_dist <- ifelse(lr_min_idx==1, L2$dist, R2$dist)
    
    if(lr_min_idx==1) {
      closest_lr_points <- l
    }
    if(lr_min_idx==2){
      closest_lr_points <- r
    }
    
    # Look at middle points from subset
    m <- L1[round(nrow(L1)/2), 'x'] # Median is in the middle row in L1 since L1 is ordered by x
    middle_info <- middle_points(l, r, m, lr_min_dist)
    
    
    # Compare middle with left/right
    if(is.na(middle_info[[1]]) || is.na(middle_info[[2]])) {
      middledist <- Inf} else {
        lmiddle <- middle_info$left_middle
        rmiddle <- middle_info$right_middle
        across_info <- compare_middle(lmiddle, rmiddle)
        middlepoints <- across_info$points
        middledist <- across_info$dist
      }
    
    
    lrm_min_dist_idx <- which.min(c(lr_min_dist, middledist))
    
    
    if(lrm_min_dist_idx==1) {
      lrm_close_points <- closest_lr_points
    }
    
    if(lrm_min_dist_idx==2) {
      lrm_close_points <- middlepoints
    }
    
    lrm_min_dist <- ifelse(lrm_min_dist_idx==1, lr_min_dist, middledist)
  }
  
  return(list(dist=lrm_min_dist, points = lrm_close_points))
  
}

## MAIN FUNCTION: closestPairs
closestPairs <- function(df_points) {
  assertthat::assert_that(is.data.frame(df_points))
  assertthat::assert_that(ncol(df_points)==2 && nrow(df_points > 1))
  assertthat::assert_that(nrow(df_points) >= 2)
  assertthat::assert_that(all(!is.na(df_points)))
  
  df_points <- df_points[order(df_points$x),] # Order by x column
  
  closest(df_points)
}
