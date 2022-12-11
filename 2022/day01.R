# load pkgs
library(tidyverse)

# get input
source("advent_input.R")
input <- advent_input(day = 1, year = 2022)

split(input, cumsum(is.na(input)))


# part 1
input %>% 
  mutate(grp = cumsum(is.na(x))) %>% 
  filter(!is.na(x)) %>% 
  rename(n_cal = x) %>% 
  group_by(grp) %>% 
  summarise(n = sum(n_cal)) %>% arrange(-n)


# neater option for part 1
input %>% 
  mutate(grp = cumsum(is.na(x))) %>% 
  drop_na(x) %>% 
  group_by(grp) %>% 
  tally(., wt = x, sort = T)




# part 2
input %>% 
  mutate(grp = cumsum(is.na(x))) %>% 
  filter(!is.na(x)) %>% 
  rename(n_cal = x) %>% 
  group_by(grp) %>% 
  summarise(n = sum(n_cal)) %>% 
  ungroup() %>% 
  arrange(-n) %>% 
  slice_head(n=3) %>% 
  tally(n)



