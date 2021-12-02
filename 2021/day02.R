# Day 2
# source function to get input
source("2021/advent_input.R")

library(tidyverse)

# get data
day2 <- advent_input(day = 2, year = 2021) %>% 
  separate(col = x, into = c("direction", "value")) %>% 
  mutate(value = as.numeric(value))



# part a ------------------------------------------------------------------
 
day2 %>% 
  group_by(direction) %>% 
  summarise(sum = sum(value)) %>% 
  ungroup()

# horizontal * (down - up)
2003 * (1941 - 1069)

# other option
day2 %>% 
  mutate(value = if_else(direction == "up", value * -1, value), 
         direction1 = if_else(direction == "forward", "h", "o")) %>%
  group_by(direction1) %>% 
  summarise(r = sum(value)) %>% 
  pull(r) %>% 
  prod()


# part b ------------------------------------------------------------------

# using test data
test <- 
  tibble::tribble(~direction, ~value,
      "forward", 5,
        "down", 5,
     "forward", 8,
          "up", 3,
        "down", 8,
     "forward", 2
     )


test %>% 
  mutate(
        value = ifelse(direction == "up", value * -1, value),
        aim = ifelse(direction == "forward", 0, value) %>% cumsum(),
        depth = ifelse(direction == "forward", value * aim, value)
        ) %>%
  filter(direction == "forward") %>%
  summarise(
          position = sum(value),
          depth = sum(depth) 
          ) %>%
  as.numeric() %>%
  prod()

day2 %>% 
  mutate(
    value = ifelse(direction == "up", value * -1, value),
    aim = ifelse(direction == "forward", 0, value) %>% cumsum(),
    depth = ifelse(direction == "forward", value * aim, value)
  ) %>%
    filter(direction == "forward") %>%
    summarise(
      position = sum(value),
      depth = sum(depth) 
    ) %>%
    as.numeric() %>%
    prod()
