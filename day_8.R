library(tidyverse)

source('AoC functions.R')

download_advent(2021,
                8)

input %>% 
  separate(value, sep = ' \\| ', into = c('sig_patterns', 'output_val')) %>% 
  mutate(across(c('sig_patterns', 'output_val'),
                str_trim),
    
    output_num = str_count(output_val, '(?<=(^| ))([a-z]{2}|[a-z]{4}|[a-z]{3}|[a-z]{7})(?=($| ))')) %>% # represents numbers 1 4 7 8
  summarise(sum(output_num))


# Part 2 ------------------------------------------------------------------


# Compare 9 and 4, removing pos a to gether pos g


string_compare <- function(char_1, 
                         char_2,
                         overlap = FALSE,
                         remove = NA){
  
  char_1_split <- str_split(char_1, '') %>% unlist()
  char_2_split <- str_split(char_2, '') %>% unlist()
  
  if(length(char_1_split) < length(char_2_split)){
    chr_2_temp <- char_1_split
    char_1_split <- char_2_split
    char_2_split <- chr_2_temp
    
    rm(chr_2_temp)
  }
  if(!is.na(remove) %>% sum > 0){
    char_1_split <- char_1_split[!char_1_split %in% remove]
    char_2_split <- char_2_split[!char_2_split %in% remove]
    
  }
  
  if(overlap){
    char_1_split[char_1_split %in% char_2_split]
  } else{
    char_1_split[!char_1_split %in% char_2_split]
  }
}

mult_options <- function(base_var,
                         vars,
                         remove,
                         output = 'letter'){
  
  if(length(string_compare(base_var, 
                           vars[1],
                           remove = remove)) == 1) {
    
    output_f <- string_compare(base_var, 
                   vars[1],
                   remove = remove)
    
    numb_pos <- vars[1]
  } else if(length(string_compare(base_var, 
                                  vars[2],
                                  remove = remove)) == 1) {
    
    output_f <- string_compare(base_var, 
                   vars[2],
                   remove = remove)
    numb_pos <- vars[2]
    
  } else if(length(string_compare(base_var, 
                                  vars[3],
                                  remove = remove)) == 1) {
    
    output_f <- string_compare(base_var, 
                   vars[3],
                   remove = remove)
    numb_pos <- vars[3]
    
  }
  
  
  if(output == 'letter') output <- output_f
  if(output == 'pos') output <- numb_pos
  
  output 
}

df <- input %>% 
  separate(value, sep = ' \\| ', into = c('sig_patterns', 'output_val')) %>% 
  mutate(across(c('sig_patterns', 'output_val'),
                str_trim),
         pattern = str_extract_all(sig_patterns, '[a-z]+'),
         output_num = str_extract_all(output_val, '[a-z]+')) %>% 
  rowwise() %>% 
  
  mutate(pattern = list(pattern[pattern %>% nchar %>% order])) %>% # Orders from shortest string to longest
  unnest_wider(pattern, names_sep = '_') %>% 
  unnest_wider(output_num, names_sep = '_')  %>% 
  select(-c(sig_patterns, output_val)) %>% 
  rename(num_1   = pattern_1,
         num_7   = pattern_2,
         num_4   = pattern_3,
         pattern_5_1 = pattern_4,
         pattern_5_2 = pattern_5,
         pattern_5_3 = pattern_6,
         pattern_6_1 = pattern_7,
         pattern_6_2 = pattern_8,
         pattern_6_3 = pattern_9,
         num_8   = pattern_10,
         )

df_1 <- df %>% 
  rowwise() %>% 
  mutate(pos_a = string_compare(num_7,
                                num_1), # Logic: compare 7 and 1 to get top bar
         
         pos_g = mult_options(num_4,
                                   c(pattern_6_1,
                                     pattern_6_2,
                                     pattern_6_3),
                                   pos_a), # Compare 9 and 4, removing pos a to get pos g
         num_9 = mult_options(num_4,
                              c(pattern_6_1,
                                pattern_6_2,
                                pattern_6_3),
                              pos_a,
                              output = 'pos'), # Compare 8 and 9 to get pos e
         
         pos_e = string_compare(num_8,
                                num_9),
         
         pos_d = mult_options(num_1,
                              c(pattern_5_1,
                                pattern_5_2,
                                pattern_5_3),
                              c(pos_a, pos_g)), # Compare 3 to 1 removing pos a and pos g to  get pos d
         
         
         pos_b = string_compare(num_4,
                                num_1,
                                remove = pos_d), # compare 1 and 4 removing d to get b
         
         pos_c = mult_options(num_8,
                              c(pattern_6_1,
                                pattern_6_2,
                                pattern_6_3),
                              c(pos_e, pos_d)), # compare 8 and those with 6 lines, removing lines that are used to get to 9 and 0
         
         num_6 = mult_options(num_8,
                              c(pattern_6_1,
                                pattern_6_2,
                                pattern_6_3),
                              c(pos_e, pos_d),
                              output = 'pos'),
         
         num_0 = mult_options(num_8,
                              c(pattern_6_1,
                                pattern_6_2,
                                pattern_6_3),
                              c(pos_e, pos_c),
                              output = 'pos'), # compare 8 and those with 6 lines, removing lines that are used to get to 9 and 
         
         pos_f = mult_options(num_8,
                              c(pattern_5_1,
                                pattern_5_2,
                                pattern_5_3),
                              c(pos_b, pos_c, pos_e)), # compare 8 with 5 lines removing lines that make 5 2 and 3
         
         
         num_3 = mult_options(num_1,
                              c(pattern_5_1,
                                pattern_5_2,
                                pattern_5_3),
                              c(pos_a, pos_g),
                              output = 'pos'),
         
         num_2 = mult_options(num_8,
                              c(pattern_5_1,
                                pattern_5_2,
                                pattern_5_3),
                              c(pos_b, pos_c, pos_e),
                              output = 'pos'),
         
         num_5 = mult_options(num_8,
                              c(pattern_5_1,
                                pattern_5_2,
                                pattern_5_3),
                              c(pos_b, pos_e, pos_f),
                              output = 'pos'),
         
         
         
  ) %>% 
  ungroup()  


df_1 %>% 
  select(starts_with('pos'),
         starts_with('num')) %>% 
  mutate(check = str_c( pos_a, pos_g, pos_e, pos_d, pos_b, pos_c, pos_f),
         checks = str_detect(check, '([a-z]).*\1')) %>% 
  pull(checks) %>% sum()


df_2 <- df_1 %>% 
  select(starts_with('num')) %>% 
  mutate(id = row_number()) %>% 
  rowwise() %>% 
  mutate(across(1:10,
                ~ str_split(., pattern = '')),
         across(1:10,
                ~ list(str_sort(., pattern = ''))),
         across(1:10,
                ~ list(str_c(., collapse = '')))) %>% 
  unnest(num_1:num_5)

numbers <- colnames(df_2)[1:10] %>% 
  str_remove('num_') %>% 
  as.numeric()

output_nums <- input %>% 
  separate(value, sep = ' \\| ', into = c('sig_patterns', 'output_val')) %>% 
  mutate(across(c('sig_patterns', 'output_val'),
                str_trim)
  ) %>% 
  transmute(output_num = str_extract_all(output_val, '[a-z]+'),
            id = row_number()) %>% 
  unnest_wider(output_num, names_sep = '_') %>% 
  rowwise() %>% 
  mutate(across(1:4,
                ~ str_split(., pattern = '')),
         across(1:4,
                ~ list(str_sort(., pattern = ''))),
         across(1:4,
                ~ list(str_c(., collapse = '')))) %>% 
  unnest(output_num_1:output_num_4)

ouput_nums_1 <- df_2 %>% 
  left_join(output_nums, by = 'id') %>% 
  nest(data = c(num_1, num_7, num_4, num_8, num_9, num_6, num_0, num_3, num_2, num_5)) %>% 
  rowwise() %>% 
  mutate(which_1 = (numbers[which(output_num_1 == unlist(data))]),
         which_2 = (numbers[which(output_num_2 == data)]),
         which_3 = (numbers[which(output_num_3 == data)]),
         which_4 = (numbers[which(output_num_4 == data)])) %>% 
  mutate(number = str_c(which_1, which_2, which_3, which_4, collapse = '')) %>% 
  ungroup() %>% 
         mutate(across(number,
                as.numeric)) 

ouput_nums_1 %>% 
  summarise(sum(number))


