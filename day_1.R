## Advent of code day 1

library(tidyverse)

df <- read_csv('input1.txt', col_names = F,
               col_types = 'n')


df %>% 
  mutate(diff = X1 - dplyr::lag(X1),
         flag = ifelse(diff > 0,
                       1,
                       -1)) %>% 
  filter(flag == 1) %>% 
  nrow()


# Part 2 ------------------------------------------------------------------


df %>% 
  mutate(sum_3_win = X1 +
                         lead(X1) +
                         lead(X1, 2),
    diff = sum_3_win - dplyr::lag(sum_3_win),
         flag = ifelse(diff > 0,
                       1,
                       -1)) %>% 
  filter(flag == 1) %>% 
  nrow()
