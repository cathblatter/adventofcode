# Day 6
# source function to get input
# source("2021/advent_input.R")

library(tidyverse)

day6 <- readLines("2021/inputs/day06.txt") %>% strsplit(",") %>% unlist() %>% readr::parse_number()


# Test case ---------------------------------------------------------------

ini <- c(3,4,3,1,2)

ini <- ini-1

for (i in 1:79) {
  
  if (0 %in% ini) {
    
    ini <- ini-1
    
    cnew <- sum(ini == -1)
    
    ini[ini == -1] <- 6 
    
    ini <- append(ini, rep(8, cnew))
    
    cnew <- NA
    
  } else {
    
    ini <- ini-1
  }
  
}


# Part A ------------------------------------------------------------------


ini <- day6

ini <- ini-1

for (i in 1:79) {
  
  if (0 %in% ini) {
    
    ini <- ini-1
    
    cnew <- sum(ini == -1)
    
    ini[ini == -1] <- 6 
    
    ini <- append(ini, rep(8, cnew))
    
    cnew <- NA
    
  } else {
    
    ini <- ini-1
  }
  
}

length(ini)



# Part B ------------------------------------------------------------------

# After failing for-loop I needed to look for alternatives - 
# beautiful solution by Clare Horscroft



## Get the initial counts for each age of fish
# I started out with this one but
# couldnt get my head around how to calculate this further...
init_counts<-c(0, tabulate(day6, nbins = 8))

## Define function for interating x days
## Each day, the age of fish gets one smaller
## Fish who were on 0 reset to 6 days (index 7)
## New fish are generated at 8 days (index 9)
iterateFish<-function(counts,days){
  for (i in 1:days){
    newFish<-counts[1]
    counts[1:8]<-counts[2:9]
    counts[9]<-newFish
    counts[7]<-counts[7]+newFish
  }
  return(sum(counts))
}

options(scipen = 999)
iterateFish(init_counts, 256)


