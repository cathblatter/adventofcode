# Day 3
source("2021/advent_input.R")

day3 <- advent_input(day = 3, year = 2021)

library(tidyverse)

# part A ------------------------------------------------------------------

str_split_fixed(day3$x, "", 12) %>% 
  as_tibble() %>% 
  mutate(across(.cols = everything(), 
                .fns = ~as.numeric(.))) %>% 
  summarise(across(.cols = everything(), ~sum(.))) %>% 
  mutate(across(.cols = everything(), 
                .fns = ~if_else(. > 500, 1, 0))) %>% 
  unite(col = x, everything(), sep = "") %>% as.character() %>% strtoi(., base = 2) -> gamma

str_split_fixed(day3$x, "", 12) %>% 
  as_tibble() %>% 
  mutate(across(.cols = everything(), 
                .fns = ~as.numeric(.))) %>% 
  summarise(across(.cols = everything(), ~sum(.))) %>% 
  mutate(across(.cols = everything(), 
                .fns = ~if_else(. < 500, 1, 0))) %>% 
  unite(col = x, everything(), sep = "") %>% as.character() %>% strtoi(., base = 2) -> epsilon

prod(gamma, epsilon)


# part B ------------------------------------------------------------------

day3_num <- 
  str_split_fixed(day3$x, "", 12) %>% 
  as_tibble() %>% 
  mutate(across(.cols = everything(), 
                .fns = ~as.numeric(.)))



# code from Hannes Oberreiter - I could not solve this one 
# but took his code to work through the solution

# Part 1
fHelper <- function(x, t = "min") {
  if (t == "min") {
    r <- names(table(x))[which.min(table(x))]
  } else {
    r <- names(table(x))[which.max(table(x))]
  }
  as.integer(r)
}

toBinary <- function(x) {
  as.character(x) %>%
    paste0(collapse = "") %>%
    strtoi(base = 2)
}


fHelper2 <- function(x, t = "min") {
  zero <- sum(x == 0)
  one <- sum(x == 1)
  if (t == "min") {
    binary <- 0
    if (one < zero) {
      binary <- 1
    }
  } else {
    binary <- 1
    if (zero > one) {
      binary <- 0
    }
  }
  binary
}


fRec <- function(x, pos = 1, t = "max") {
  if (nrow(x) == 1) {
    return(x)
  }
  r <- x[, pos] %>%
    unlist() %>%
    fHelper2(., t = t)
  fRec(x[x[, pos] == r, ], pos + 1, t = t)
}

oxygen <- fRec(day3_num, t = "max") %>%
  toBinary()

co2 <- fRec(day3_num, t = "min") %>%
  toBinary()


oxygen * co2
