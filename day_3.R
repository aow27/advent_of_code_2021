library(tidyverse)
library(glue)


df <- tibble(value = read_lines('Data/input3.txt'))

n_bits <- nchar(df[1,])


bits <- df %>% 
  separate(value,
           sep = '(?<=(0|1))',
           into = paste0('col_', 1:n_bits)) %>% 
  mutate(across(everything(),
                as.numeric))


df_1 <- bits %>% 
  summarise(across(everything(),
                   sum)) %>% 
  t() %>% 
  as_tibble(rownames = 'bit') %>% 
  mutate(most_common = ifelse(V1 > nrow(df)/2,
                                   1,
                                   0),
         least_common = (most_common + 1) %% 2)

gamma <- df_1 %>% 
  pull(most_common) %>% 
  paste0(collapse = "") %>% 
  strtoi(base = 2)

epsilon <- df_1 %>% 
  pull(least_common) %>% 
  paste0(collapse = "") %>% 
  strtoi(base = 2)


gamma*epsilon


# Part 2 ------------------------------------------------------------------

for (i in 1:n_bits) {
  
  if(i==1) {
    oxy_number <- bits
    co_number <- bits
  }
  
  if (nrow(oxy_number) != 1) {  
    oxy_number <<- oxy_number %>% 
      mutate(col_sum = sum(!!sym(glue("col_{i}"))),
             col_filter = ifelse(col_sum/n() >= 0.5,
                                  1,
                                 0)) %>% 
               
      filter(across(glue::glue('col_{i}'),
                    ~ . ==  col_filter)) %>% 
      select(-c(col_sum,
                col_filter))
    
  }
  
  if (nrow(co_number) != 1) {  
    co_number <<- co_number %>% 
      mutate(col_sum = sum(!!sym(glue("col_{i}"))),
             col_filter = ifelse(col_sum/n() >= 0.5,
                                 0,
                                 1)) %>% 
      
      filter(across(glue::glue('col_{i}'),
                    ~ . ==  col_filter))%>% 
      select(-c(col_sum,
                col_filter))
    
    
  }

}

  oxy_output <- oxy_number %>% 
    paste0(collapse = "")%>% 
    strtoi(base = 2)
  
  co_output <- co_number %>% 
    paste0(collapse = "")%>% 
    strtoi(base = 2)
  
  print(co_output*oxy_output)





