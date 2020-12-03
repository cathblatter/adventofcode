library(tidyverse)

numbers <- read_csv("cath_inputs/day-01", col_names = "num")

# part 1
numbers %>% 
  mutate(num2 = num) %>% 
  expand(num, num2) %>% 
  filter(num+num2 == 2020) %>% 
  summarise(product = num*num2) %>% unique()

# short version
prod(intersect(numbers, 2020-numbers))



# part 2
numbers %>% 
  mutate(num2 = num,
         num3 = num) %>% 
  expand(num, num2, num3) %>% 
  filter(num+num2+num3 == 2020) %>% 
  summarise(product = num*num2*num3) %>% unique()

# 
# prod(intersect(numbers, 2020-outer(numbers, numbers, "+")))




