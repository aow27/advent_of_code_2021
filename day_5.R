library(tidyverse)

source('AoC functions.R')

download_advent(2021,
                5)

df <- input %>% 
  separate(value, c('x_1', 'y_1', 'x_2', 'y_2')) %>% 
  mutate(across(everything(),
                as.numeric),
         x_same = x_1 == x_2,
         y_same = y_1 == y_2)

h_v_only <- df %>% 
  filter(x_same == T |
           y_same ==T)


for(i in 1:nrow(h_v_only)){
  if(i == 1) points <- tibble(x = numeric(), 
                              y = numeric())
  
  points <- points %>% 
    rbind(tibble(x = seq(h_v_only$x_1[i],
                         h_v_only$x_2[i]),
                 y = seq(h_v_only$y_1[i],
                         h_v_only$y_2[i])))
  
}

points %>% 
  group_by(x, y) %>% 
  summarise(numb = n()) %>% 
  filter(numb > 1) %>% 
  nrow()


# Part 2 ------------------------------------------------------------------

for(i in 1:nrow(df)){
  if(i == 1) points <- tibble(x = numeric(), 
                              y = numeric())
  
  points <- points %>% 
    rbind(tibble(x = seq(df$x_1[i],
                         df$x_2[i]),
                 y = seq(df$y_1[i],
                         df$y_2[i])))
  
}

points %>% 
  group_by(x, y) %>% 
  summarise(numb = n()) %>% 
  filter(numb > 1) %>% 
  nrow()
 
