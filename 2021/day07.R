# Day 7

library(tidyverse)

day7 <- readr::parse_number(unlist(stringr::str_split(readLines("2021/inputs/day07.txt"), ",")))

# Test case ---------------------------------------------------------------

test <- c(16,1,2,0,4,2,7,1,2,14)

# closest with using median
sum(abs(test - median(test)))


# using mean for part 2
gg <- abs(test - ceiling(mean(test)))
gg <- data.frame(gg)

gg %>% 
  rowwise() %>% 
  mutate(test = sum(1:gg)) %>% 
  ungroup() %>% 
  summarise(sum = sum(test))



# Part A ------------------------------------------------------------------

sum(abs(day7 - median(day7)))


# Part B ------------------------------------------------------------------

# using floor mean for part 2 (I started with ceiling and failed, so witched to floor)
gg <- abs(day7 - floor(mean(day7)))
gg <- data.frame(gg)

gg %>% 
  rowwise() %>% 
  mutate(test = sum(1:gg)) %>% 
  ungroup() %>% 
  summarise(sum = sum(test))
