# load pkgs
library(tidyverse)

# get input
source("advent_input.R")
input <- advent_input(day = 2, year = 2022)


# testing
tibble::tribble(
  ~x,
  "A Y",
  "B X",
  "C Z"
  )


# part 1 
input %>%   
  separate(x, into = c("elf", "you")) %>% 
  mutate(you = case_when(you == "X" ~ "A", 
                         you == "Y" ~ "B", 
                         .default = as.character("C")), 
         score_tool = case_when(
           you == "A" ~ 1L, 
           you == "B" ~ 2L, 
           .default = 3L
         ),
         score_win = case_when(
           elf == you ~ 3L, 
           elf == "A" & you == "B" ~ 6L, 
           elf == "A" & you == "C" ~ 0L, 
           elf == "B" & you == "A" ~ 0L,
           elf == "B" & you == "C" ~ 6L,
           elf == "C" & you == "A" ~ 6L, 
           elf == "C" & you == "B" ~ 0L
         ), 
         sumscore = score_tool + score_win) %>% 
  tally(sumscore)


# part 2
# input
  tibble::tribble(
    ~x,
    "A Y",
    "B X",
    "C Z"
  ) %>% 
  separate(x, into = c("elf", "you")) %>% 
  mutate(you = case_when(you == "X" ~ "A", 
                         you == "Y" ~ "B", 
                         .default = as.character("C")),
         you2 = case_when(
           you == "B" ~ elf,
           elf == "A" & you == "A" ~ "C",
           elf == "A" & you == "C" ~ "B",
           elf == "B" & you == "A" ~ "A",
           elf == "B" & you == "C" ~ "C",
           elf == "C" & you == "A" ~ "B", 
           elf == "C" & you == "C" ~ "A",
         ),
         score_tool = case_when(
           you2 == "A" ~ 1L, 
           you2 == "B" ~ 2L, 
           .default = 3L
         ),
         score_out = case_when(
           you2 == "A" ~ 0L,
           you2 == "B" ~ 3L, 
           you2 == "C" ~ 6L
         ), 
         score = score_tool + score_out)


# part 2
  input <- input$x
  
  dict <- c(A = "rock", B = "paper", C = "scissors",
            X = "lose", Y = "tie", Z = "win")
  
  you <- dict[substr(input, 1, 1)]
  me <- dict[substr(input, 3, 3)]
  
  mat <- matrix(
    c("scissors", "rock", "paper",
      "paper", "scissors", "rock",
      "rock", "paper", "scissors"),
    byrow = TRUE, ncol = 3,
    dimnames = list(c("lose", "win", "tie"),
                    c("rock", "paper", "scissors"))
  )
  mat
  
  scores <- c(win = 6, tie = 3, lose = 0, rock = 1, paper = 2, scissors = 3)
  
  sum(scores[mat[cbind(me, you)]]) + sum(scores[me])
  
  