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


