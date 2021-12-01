# Day 1
# source function to get input
source("2021/advent_input.R")

library(dplyr)

# get data
day1 <- advent_input(day = 1, year = 2021)


# part A ------------------------------------------------------------------

# count number of lags < x
day1 %>% 
  mutate(ans = if_else(lag(x) < x, "inc", "dec")) %>% 
  count(ans)


# part B ------------------------------------------------------------------

# manual option (corresponds to align = center below)
day1 %>% 
  mutate(slide = lag(x) + x + lead(x), 
         prev_slide = lag(slide) < slide) %>% 
  count(prev_slide)

#  (corresponds to align = left below)
day1 %>% 
  mutate(slide = lag(x, n = 2) + lag(x) + x, 
         prev_slide = lag(slide) < slide) %>%
  count(prev_slide)

#  (corresponds to align = right below)
day1 %>% 
  mutate(slide = x + lead(x) + lead(x, n = 2), 
         prev_slide = lag(slide) < slide) %>% 
  count(prev_slide)


# option using rollsums from zoo-package
day1 %>% 
  mutate(test = zoo::rollsum(x, k = 3, align = "center", fill = NA), 
         test2 = if_else(lag(test) < test, "inc", "dec")) %>% 
  count(test2)
