library(tidyverse)
library(tictoc)

input = tibble(value = read_delim("Data/inpu6.txt",
                                  ',',
                                  col_names = F)) 
{state_start <- input_test %>% 
  t() %>% 
  as_tibble() %>%
  rename(start = V1)

record <<- tibble(fish = state$start %>% 
                    paste0(collapse = ','))

state <- state_start

adding_fish <- 0}

for(days in 1:80){
  
  state <- state %>% 
    mutate(step = case_when(start == 0 ~ 6,
                            TRUE       ~ start - 1)) %>% 
    transmute(start = step) %>% 
    rbind(tibble(start = rep(8, adding_fish)))
  
  record <<- record %>% 
    rbind(tibble(fish = state$start %>% 
                   paste0(collapse = ',')))
  
  adding_fish <- sum(state$start == 0)
  
}

nrow(state)

# Part 2 ------------------------------------------------------------------

## Setting up dataframes at the start
## Realised that tracking every fish won't work if example has over 26 billion of them! Just track number of fish with x days left

summarised_state <-  state_start %>% 
  group_by(start) %>% 
  summarise(number = n()) 

for (i in 1:256){
  
  print(i)
  
  adding_fish <- filter(summarised_state, start == 0) %>% pull(number)
  
  summarised_state <<- summarised_state %>% 
    mutate(start = case_when(start == 0 ~ 6,
                             TRUE       ~ start - 1)) %>% 
    group_by(start) %>% 
    summarise(number = sum(number)) %>%
    ungroup() %>% 
    rbind(tibble(start = 8,
                 number = adding_fish)) %>% 
    filter(number != 0) %>% 
    arrange(start)
}

summarised_state %>% 
  summarise(sum(number))

