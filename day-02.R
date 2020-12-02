library(tidyverse)

passwords <- read_csv("inputs/day-02", col_names = "policy")

passwords %>% 
  extract(col = policy, 
          into = c("min", "max", "letter", "password"), 
          regex = "(\\d+)-(\\d+) (.): (.*)", convert = T) %>% 
  mutate(check_letter = map2_int(password, letter, str_count)) %>% 
  mutate(check = case_when(check_letter >= min & check_letter <= max ~ TRUE, 
                           TRUE ~ FALSE)) %>% 
  summarise(sum = sum(check == TRUE))

         