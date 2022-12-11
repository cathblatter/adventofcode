# pkgs
library(tidyverse)

# # input
# source("advent_input.R")
# input <- advent_input(day = 2, year = 2022)

crates <- 
  read.fwf("2022/day05-test.txt", 
                  widths = rep(4,3), n = 3)  %>% 
  map(., ~rev(str_extract(.x, "[:upper:]"))) %>% 
  lapply(., .[!. %in% c("", NA)])
  mutate(across(.cols = everything(), 
                .fns = ~rev(str_extract(., "[:upper:]"))),
                # .fns = ~if_else(!is.na(.), str_extract(., "[:upper:]"), NA_character_)),
         across(.cols = everything(), rev)) %>%
  as.list() 


operations <- read.table(
  "2022/day05-test.txt", sep = " ", skip = 4,
  col.names = c("v1", "move", "v2", "from", "v3", "to")
) %>% select(2,4,6)


operations


crates[[1]]


crates[[2]][[3]]

c(crates[[1]], rev(tail(crates[[2]],1)))




#### Part 1 ####
for (i in seq(nrow(ops))) {
  # remove rev for Part 2
  crates[[ops$to[i]]] <- c(crates[[ops$to[i]]], rev(tail(crates[[ops$from[i]]], ops$move[i])))
  crates[[ops$from[i]]] <- head(crates[[ops$from[i]]], -ops$move[i])
}
vapply(crates, tail, character(1L), n = 1) |> paste(collapse = "")


#### Part 1 ####
for (i in seq(nrow(ops))) {
  # remove rev for Part 2
  crates[[ops$to[i]]] <- c(crates[[ops$to[i]]], tail(crates[[ops$from[i]]], ops$move[i])))
  crates[[ops$from[i]]] <- head(crates[[ops$from[i]]], -ops$move[i])
}
vapply(crates, tail, character(1L), n = 1) |> paste(collapse = "")



read.fwf("2022/day05-test.txt", widths = rep(4, 3), n = 3) |>
  lapply(\(x) rev(gsub("\\W", "", x))) |>
  lapply(\(x) x[!x %in% c("", NA)])
# 
# 
# ops <- read.table(
#   "2022/day05-test-input.txt", sep = " ", skip = 10,
#   col.names = c("x1", "move", "x2", "from", "x3", "to")
# )