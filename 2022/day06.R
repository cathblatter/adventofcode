

library(dplyr)
library(slider)

input <- readLines("2022/day06-input.txt") |> tibble::enframe()

x <- stringr::str_split(input$value, "")[[1]]

which(slide_dbl(x, n_distinct, .before = 3) == 4)[1]

which(slide_dbl(x, n_distinct, .before = 13) == 14)[1]



test_vec <- stringr::str_split("bvwbjplbgvbhsrlpgdmjqwftvncz", "")[[1]]

which(slide_dbl(test_vec, n_distinct, .before = 3) == 4)[1]
