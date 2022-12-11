library(tidyverse)

# get input
source("advent_input.R")
input <- advent_input(day = 4, year = 2022)


# part 1
input |> 
  extract(
    col = x, 
    into = c("one1", "one2", "two1", "two2"), 
    regex = "(\\d*)\\-(\\d*)\\,(\\d*)\\-(\\d*)",
    convert = TRUE
  ) |> 
  mutate(check = case_when(one1 <= two1 & 
                             one2 >= two2 ~ 1L, 
                           one1 >= two1 &
                             one2 <= two2 ~ 1L, 
                           .default = 0L)) |> 
  tally(check)


# part 2
input |> 
  extract(
    col = x, 
    into = c("one1", "one2", "two1", "two2"), 
    regex = "(\\d*)\\-(\\d*)\\,(\\d*)\\-(\\d*)",
    convert = TRUE
  ) |> 
  mutate(
    e1 = purrr::map2(one1,one2,~.x:.y),
    e2 = purrr::map2(two1,two2,~.x:.y),
    intersect = purrr::map2(e1,e2,intersect)
  ) |> 
  filter(lengths(intersect) > 0) |> 
  nrow()
  



