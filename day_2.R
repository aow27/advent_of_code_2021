## Advent of code day 2

library(tidyverse)

df <- tibble(value = read_lines('input2.txt'))

df_1 <- df %>% 
  separate(value, c('dir', 'value'), sep = ' ') %>% 
  mutate(value = as.numeric(value),
         
         horiz_change = case_when(dir == 'forward' ~ value,
                               TRUE ~ 0),
         depth_change = case_when(dir == 'down' ~  value,
                                  dir == 'up'   ~ -value,
                                  TRUE ~ 0),
         
         horiz = cumsum(horiz_change),
         depth = cumsum(depth_change),
         move = row_number())

df_1 %>% 
  filter(move == max(move)) %>% 
  mutate(answer = horiz*depth)


# Part 2 ------------------------------------------------------------------


df_2 <- df %>% 
  separate(value, c('dir', 'value'), sep = ' ') %>% 
  mutate(value = as.numeric(value),
         
         horiz_change = case_when(dir == 'forward' ~ value,
                                  TRUE ~ 0),
         aim_change = case_when(dir == 'down' ~  value,
                                  dir == 'up'   ~ -value,
                                  TRUE ~ 0),
         
         horiz = cumsum(horiz_change),
         aim = cumsum(aim_change),
         
         depth_change = case_when(dir == 'forward' ~ aim*value,
                                  TRUE ~ 0),
         depth = cumsum(depth_change),
         
         move = row_number())

df_2 %>% 
  filter(move == max(move)) %>% 
  mutate(answer = horiz*depth)
