library(tidyverse)

input <- tibble(value = read_delim('Data/input7.txt',
                                   delim = ',', col_names = F)) %>% 
  t() %>% 
  as_tibble() %>%
  rename(value = V1) 

all_positions <- tibble(value = 1:max(input$value)) 

# This is looping across all frogs and comparing their current position with all of the possible distances
differences_pt1 <- 1:nrow(input) %>% 
  map_dfc( ~ all_positions %>%
         mutate(!!glue::glue('diff_{.x}') := abs(value - input$value[.x])) %>% 
           select(-value)) 

differences_pt1 %>% 
  mutate(sum = rowSums(select(.,starts_with('diff')))) %>% 
  filter(sum == min(sum)) %>% 
  distinct(sum) %>% 
  pull()

# 349769

# Part 2 ------------------------------------------------------------------

# Same as above but changes the formula to calculate the fuel needed
# Used function to define triangle numbers (x*(x+1)/2) as this is what the new rule essentially is
differences <- 1:nrow(input) %>% 
  map_dfc( ~ all_positions %>%
             mutate(move = abs(value - input$value[.x]),
                    !!glue::glue('fuel_{.x}') := (move *(move + 1))/2) %>% 
             select( -c(value, move))
  )
  
differences  %>% 
  mutate(sum = rowSums(select(.,starts_with('fuel')))) %>% 
  filter(sum == min(sum)) %>% 
  distinct(sum) %>% 
  pull()

