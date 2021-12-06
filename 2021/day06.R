# Day 6
# source function to get input
# source("2021/advent_input.R")

library(tidyverse)

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

day6 <- readLines("2021/inputs/day06.txt") %>% strsplit(",") %>% unlist() %>% readr::parse_number()

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

start <- c(0, tabulate(day6, nbins = 8))

