# load pkgs
library(tidyverse)

# get input
source("advent_input.R")
input <- advent_input(day = 2, year = 2023) 


# test 

test <- 
  "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green" |> 
  strsplit(split = "\n") |> 
  unlist() |> 
  enframe() |> 
  mutate(name = NULL, 
         x = value, 
         value = NULL)

# create a maxima vector - stolen from drob
maxima <- c(red = 12, green = 13, blue = 14)

# very neat to use object-subsetting within mutate object[vector]
test |> 
  mutate(game = row_number()) |> 
  mutate(cubes = str_extract_all(x, "\\d+ [a-z]+")) |> 
  unnest(cubes) |> 
  separate(cubes, c("number", "color"), 
           sep = " ", convert = TRUE) |> 
  mutate(maximum = maxima[color]) |> 
  summarise(playable = !any(number > maximum), 
            .by = game) |> 
  filter(playable)

# part 1



# a lot of rearranging, then filtering the ids to leave out
input |> 
  mutate(id = row_number(), 
         x = str_remove(x, "Game [0-9]{1,2}\\: ")) |> 
  separate_longer_delim(cols = x, delim = ";") |> 
  mutate(x = str_squish(x), 
         set_id = row_number(), 
         .by = id) |>
  separate_longer_delim(x, delim = ",") |> 
  mutate(x = str_squish(x), 
         num = as.integer(str_extract(x, '[0-9]+')),
         col = str_extract(x, '[A-z]+')) |> 
  pivot_wider(id_cols = c(id, set_id), 
              names_from = col, 
              values_from = num) |> 
  filter(
  if_any(red, ~ . > 12) |
    if_any(blue, ~ . > 14) |
    if_any(green, ~ . > 13),
  .by = id
  ) |> pull(id) -> ids

# finally filter the target ids and sum...
input |> 
  mutate(id = row_number()) |> 
  filter(
    !(id %in% ids)
  ) |> 
  pull(id) |> sum()


# part 2
# same idea as before, and summarise by max per group
input |> 
  mutate(id = row_number(), 
         x = str_remove(x, "Game [0-9]{1,2}\\: ")) |> 
  separate_longer_delim(cols = x, delim = ";") |> 
  mutate(x = str_squish(x), 
         set_id = row_number(), 
         .by = id) |>
  separate_longer_delim(x, delim = ",") |> 
  mutate(x = str_squish(x), 
         num = as.integer(str_extract(x, '[0-9]+')),
         col = str_extract(x, '[A-z]+')) |> 
  pivot_wider(id_cols = c(id, set_id), 
              names_from = col, 
              values_from = num, 
              values_fill = 0) |> 
  summarise(across(c(green, blue, red), max),
            .by = id) |> 
  mutate(score = green*blue*red) |> 
  pull(score) |> sum()
  
