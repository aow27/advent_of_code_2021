library(tidyverse)


df <- tibble(value = read_lines('Data/input3.txt'))


nchar(df[1,])
#12 bits

bits <- df %>% 
  separate(value,
           sep = '(?<=(0|1))',
           into = paste0('col_', 1:12)) %>% 
  mutate(across(everything(),
                as.numeric))


df_1 <- bits %>% 
  summarise(across(everything(),
                   sum)) %>% 
  t() %>% 
  as_tibble(rownames = 'bit') %>% 
  mutate(most_common = ifelse(V1 > 500,
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


# Part 2 - still not correct - saving thoughts ------------------------------------------------------------------

oxy_number <- bits
co_number <- bits

for (i in 1:12) {
  
  if (nrow(oxy_number) != 1) {  
    oxy_number <<- oxy_number %>% 
      filter(across(glue::glue('col_{i}'),
                    ~ . ==  df_1$most_common[i]))
    
  }
  
  if (nrow(co_number) != 1) {  
    co_number <<- co_number %>% 
      filter(across(glue::glue('col_{i}'),
                    ~ . ==  df_1$least_common[i]))
    
    
  }

}

  oxy_output <- oxy_number %>% 
    paste0(collapse = "")%>% 
    strtoi(base = 2)
  
  co_output <- co_number %>% 
    paste0(collapse = "")%>% 
    strtoi(base = 2)
  
  print(co_output*oxy_output)





