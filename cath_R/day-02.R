library(tidyverse)

passwords <- 
  read_csv("cath_inputs/day-02b", col_names = "policy") %>% 
  extract(col = policy, 
          into = c("min", "max", "letter", "password"), 
          regex = "(\\d+)-(\\d+) (.): (.*)", convert = T) %>% 
  mutate(ID = row_number())

# part 1
passwords %>% 
  mutate(count_letter = map2_int(password, letter, str_count)) %>% 
  mutate(check = case_when(count_letter >= min & count_letter <= max ~ TRUE, 
                           TRUE ~ FALSE)) %>%
  summarise(sum = sum(check == TRUE))

# part 2
passwords %>% 
  mutate(mini = str_sub(password, min, min),
         maxi = str_sub(password, max, max),
         check = case_when(mini == letter & maxi != letter ~ TRUE, 
                           mini != letter & maxi == letter ~ TRUE, 
                           TRUE ~ FALSE)) %>% 
  summarise(sum = sum(check == TRUE))
  
passwords %>% 
  mutate(check3 = simplify_all(map2(password, letter, str_locate_all)), 
         checkmin2 = case_when(str_detect(check3, as.character(min)) ~ 1, 
                               TRUE ~ 0),
         checkmax2 = case_when(str_detect(check3, as.character(max)) ~ 1, 
                               TRUE ~ 0),
         total2 = map2_dbl(checkmin2, checkmax2, sum)) %>% 
  filter(total2 == 1) %>% pull(ID)


setdiff(right, wrong)

# 
#          checkmin = if_else(str_detect(check3, as.character(min)), TRUE, FALSE),
#          checkmax = if_else(str_detect(check3, as.character(max)), TRUE, FALSE),
#          total = pmap_dbl(list(checkmin, checkmax), sum)) %>% 
#   filter(total == 1)


# 1-3 a: abcde is valid: position 1 contains a and position 3 does not.
# 1-3 b: cdefg is invalid: neither position 1 nor position 3 contains b.
# 2-9 c: ccccccccc

tibble(min = c(1, 1, 3), 
             max = c(3, 3, 9),
             letter = c("a", "b", "c"),
             password = c("abcde","cdefg", "ccccccccc")) %>% 
  mutate(check2 = map2(password, letter, str_locate_all),
         check3 = simplify_all(check2),
         checkmin = if_else(str_detect(check3, as.character(min)), TRUE, FALSE),
         checkmax = if_else(str_detect(check3, as.character(max)), TRUE, FALSE),
         total = pmap_dbl(list(checkmin, checkmax), sum)) %>% 
  filter(total == 1) %>% nrow()
