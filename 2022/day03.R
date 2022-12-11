# day 03
input <- readLines("2022/day03-input.txt") |>
  strsplit(split = "")

test <- 
  c("vJrwpWtwJgWrhcsFMMfFFhFp", 
    "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL", 
    "PmmdzqPrVvPwwTWBwg", 
    "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn", 
    "ttgJtRGJQctTZtZT", 
    "CrZsJsPPZsGzwwsLwLmpwMDw") |> 
  strsplit(split = "")


score <- c(letters, LETTERS)

common_split <- function(x) {
  half <- length(x) / 2
  intersect(
    head(x, half), tail(x, half)
  )
}


map_chr(input, common_split) |>
  match(score) |>
  sum()
# part 1

