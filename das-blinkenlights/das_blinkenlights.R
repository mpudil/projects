# Command Line Wrapper:
#!/usr/bin/env Rscript

# Magrittr for %>%, stringr for str_split()
library(magrittr)
library(stringr)

#' Change State function
#' Changes the state of the lightbulb from time t-1 to time t (1 is on, 0 is off)
#' Turn on bulb if bulb to left is turned on. 
#' @param statet1 Configuration of lightbulbs at time t-1 
#' @return State of bulbs at time t

change_state <- function(statet1) {
  # Initialize statet as state t-1 so we can use the same dimensions
  statet <- statet1 
  for(i in 2:length(statet1)){
    # If bulb to left is on, change the state of the bulb 
    if(statet1[i-1] == 1){ 
      statet[i] <- 1-statet1[i]
    }
  }
  # Do same for first bulb in array (since the bulbs are to be in a circular shape)
  if(statet1[length(statet1)] == 1){
    statet[1] <- 1-statet1[1]
  }
  
  return(statet)
}


#' Naiive blink function
#' Changes the state of the lightbulb B number of times (one at a time)
#' @param states Configuration of lightbulbs at time t 
#' @return State of bulbs after B reconfigurations/turns
# Naiive blink function


blink_naiive <- function(states, B) {
  for(i in 1:B){
    newstates <- change_state(states)
    states <- newstates
  }
  return(states)
}


# Paste and concatenate function (vector to string)
vec_to_string <- function(vec) {
  return(paste(as.character(vec), collapse=''))
}

# Opposite of above function (string to vector)
string_to_vec <- function(string) {
  return(as.numeric(strsplit(string, "")[[1]]))
}


#' Main blink function
#' Algorithm design: This function shortcuts the naiive blink function by using modular arithmetic.
#' At its core, the function uses the fact that if a list of repeated elements (in this case the array of the 
#' configuration of light bulbs after subtracting off any non-repeating configurations at the beginning) of 
#' length L is given, then the Bth (here, the number of bulbs B) element will be the same as the 
#' ((B + L - D) mod L)+1 th position, where D is the position at which the "pattern" begins (equivalent to the 
#' number of elements the was stripped away from L originally, minus 1. The function additionally takes into 
#' consideration results when B <= L and where the pattern is only 0's (although in reality this part wasn't 
#' necessary but was part of the assignment).
#' 
#' @param states Configuration of lightbulbs at time t 
#' @return State of bulbs after B reconfigurations/turns
# Naiive blink function



blink <- function(states, B) {
  assertthat::assert_that(B > 0 && B < 10^15 && length(states) >= 3 && 
                            length(states) <= 16)
  

  # Create array that includes pattern and any other configurations leading up to the pattern

  state_t <- change_state(states)
  patterns <- c()
  i <- 1
  
  while(vec_to_string(state_t) %in% patterns == FALSE && sum(state_t) > 0 && i <= B) {
    patterns <- c(patterns, vec_to_string(state_t))
    state_t <- change_state(state_t)
    i <- i+1
  }
  
  # If B <= the length of patterns then just return the Bth element of patterns list
  if(B <= length(patterns)){
    final_pattern <- patterns[B] %>% string_to_vec
  } else {
    
  next_state <- state_t
  
  # if pattern is all 0's, then that will always be the pattern (once it gets there)
    if(sum(next_state) == 0 && B > length(patterns)) {
      final_pattern <- rep(0, length(states))
    }
  
  # If B is greater than the length of patterns (and the next part of the pattern is not all 0's)
    if(B > length(patterns) && sum(next_state) > 0) {
      D <- which(vec_to_string(next_state) == patterns)  # Distance to pattern
     
      # Strip of elements of the pattern that do not repeat (if they exist)
      if(D==1){
        patterns_repeat <- patterns
      } else {
        patterns_repeat <- patterns[-c(1:D-1)]
      }
      
      L <- length(patterns_repeat) # Length of patterns array (may include stuff before pattern starts)
      #B <- B-D+1
      
      final_pattern <- patterns_repeat[((B + L - D) %% L)+1] %>% string_to_vec
    }
  }
  return(final_pattern)
}


# Command Line Wrapper (commented out because the test* file doesn't know how to deal with command line wrappers)
# Example: Rscript das_blinkenlights.R 1000101 50

# args = commandArgs(trailingOnly=TRUE)
# x1=args[1]
# x2=args[2]

# blink(as.numeric(str_split(as.character(x1), "")[[1]]), as.numeric(x2))
