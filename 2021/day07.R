# Day 7

day7 <- readr::parse_number(unlist(stringr::str_split(readLines("2021/inputs/day07.txt"), ",")))




# Test case ---------------------------------------------------------------

test <- c(16,1,2,0,4,2,7,1,2,14)

# closest with using median
sum(abs(test - median(test)))



# Part A ------------------------------------------------------------------

sum(abs(day7 - median(day7)))




# Part B ------------------------------------------------------------------


