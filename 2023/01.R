# load pkgs
library(tidyverse)

# get input
source("advent_input.R")
input <- advent_input(day = 1, year = 2023) 

# part 1
input |> 
  mutate(digits = str_remove_all(x, "[A-z]"),
         first = str_extract(digits, "\\d"), 
         last = str_extract(digits, "\\d$"), 
         both = as.integer(paste0(first, last))) |> 
  summarise(total = sum(both))


# part 2

# construct a matcher to replace words by digits first
word_digits <- c("one" = "1", 
                 "two" = "2", 
                 "three" = "3", 
                 "four" = "4", 
                 "five" = "5", 
                 "six" = "6", 
                 "seven" = "7", 
                 "eight" = "8", 
                 "nine" = "9")

test <- 
  "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen"

# code below originally from ella kayes 
# but I got very (!) frustrated 😅

string_reverse <- function(x) {
  strsplit(x, "") |> unlist() |> rev() |> paste0(collapse = "")
}



nums <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
nums_pattern <- paste(nums, collapse = "|")
nums_digit_pattern <- paste0(nums_pattern, "|\\d")

nums_pattern_rev <- string_reverse(nums_pattern)
nums_digit_pattern_rev <- paste0(nums_pattern_rev, "|\\d")


library(stringr)
get_last_digit <- function(x) {
  str_extract(string_reverse(x), nums_digit_pattern_rev) |> string_reverse() 
}

convert_to_digit <- function(x) {
  if (nchar(x) == 1) {
    x <- as.numeric(x)
  } else {
    x <- match(x, nums)
  }
  x
}


get_value2 <- function(x) {
  
  first <- str_extract(x, nums_digit_pattern)
  
  last <- get_last_digit(x)
  
  first_digit <- convert_to_digit(first)
  last_digit <- convert_to_digit(last)
  
  10*first_digit + last_digit
}

sapply(input$x, get_value2) |> sum()




