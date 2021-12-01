library(tidyverse)

input <- read_lines("cath_inputs/day-04ex")

passport <- c("byr", "iyr", "eyr", "hgt",
              "hcl", "ecl", "pid", "cid")

northpolecreds <- c("byr", "iyr", "eyr", "hgt",
                    "hcl", "ecl", "pid")

input %>% 
  enframe() %>% 
  # mutate(value2 = str_split(value, " ")) %>%
  mutate(grp = cumsum(!nzchar(value)),
         len = nchar(value)) %>% 
  filter(len != 0) %>% 
  select(-len, -name) %>% 
  group_by(grp) %>% 
  mutate(row_id = row_number()) %>% 
  pivot_wider(id_cols = grp, 
              names_from = row_id, 
              names_prefix = "v",
              values_from = value) %>% 
  ungroup() %>% 
  # unnest_wider(col = v1, names_repair = "v")
  mutate(total = paste(v1, v2, v3, v4, sep = " "),
         ex = str_extract_all(total, "[a-z]{3}:")) %>% 
  unnest_wider(col = ex, names_sep = "v") %>% 
  transmute(across(exv1:exv8, ~str_remove(., ":"))) %>% 
  mutate(check = paste(exv1, exv2, exv3, exv4, exv5, exv6, exv7, exv8, sep = " "))
