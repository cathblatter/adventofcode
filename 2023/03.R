# load pkgs
library(tidyverse)

# get input
source("advent_input.R")
input <- advent_input(day = 3, year = 2023) 

test <- "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.." |> 
  strsplit("\n") |> 
  unlist() |> 
  enframe() |> 
  rename(x = value) |> select(-name)


test |> 
  adventdrob::grid_tidy(x) |> 
  mutate(is_number = str_detect(value, "\\d")) |> 
  mutate(num_grp = consecutive_id(is_number))
