library(tidyverse)

numbers <- read_csv("inputs/day-01", col_names = "num")

numbers %>% 
  mutate(num2 = num) %>% 
  expand(num, num2) %>% 
  filter(num+num2 == 2020) %>% 
  summarise(product = num*num2) %>% unique()


prod(intersect(numbers, 2020-numbers))


prod(intersect(numbers, 2020 - outer(numbers, numbers, FUN = "+")))




