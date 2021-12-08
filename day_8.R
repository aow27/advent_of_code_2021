library(tidyverse)

input <- tibble(value = read_lines('Data/input8.txt'))


input %>% 
  separate(value, sep = ' \\| ', into = c('sig_patterns', 'output_val')) %>% 
  mutate(across(c('sig_patterns', 'output_val'),
                str_trim),
    
    output_num = str_count(output_val, '(?<=(^| ))([a-z]{2}|[a-z]{4}|[a-z]{3}|[a-z]{7})(?=($| ))')) %>% # represents numbers 1 4 7 8
  summarise(sum(output_num))


# Part 2 - NOT COMPLETE ------------------------------------------------------------------


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
  rename(pattern_7   = pattern_2,
         pattern_4   = pattern_3,
         pattern_5_1 = pattern_4,
         pattern_5_2 = pattern_5,
         pattern_5_3 = pattern_6,
         pattern_6_1 = pattern_7,
         pattern_6_2 = pattern_8,
         pattern_6_3 = pattern_9,
         pattern_8   = pattern_10,
         )

df_1 <- df %>% 
  rowwise() %>% 
  mutate(pos_a = string_compare(pattern_7,
                                pattern_1), # Logic: compare 7 and 1 to get top bar
         
         pos_g = mult_options(pattern_4,
                                   c(pattern_6_1,
                                     pattern_6_2,
                                     pattern_6_3),
                                   pos_a), # Compare 9 and 4, removing pos a to gether pos g
         num_9 = mult_options(pattern_4,
                              c(pattern_6_1,
                                pattern_6_2,
                                pattern_6_3),
                              pos_a,
                              output = 'pos'),
         
         pos_d = mult_options(pattern_1,
                              c(pattern_5_1,
                                pattern_5_2,
                                pattern_5_3),
                              c(pos_a, pos_g)), # Compare 3 to 1 removing pos a and pos g to  get pos d
         num_3 = mult_options(pattern_1,
                              c(pattern_5_1,
                                pattern_5_2,
                                pattern_5_3),
                              c(pos_a, pos_g),
                              output = 'pos'), 
         
         pos_e = string_compare(pattern_8,
                                num_9), # Compare 8 and 9 to get pos e
         
         ######### ADD OTHER POSITIONS AND THEN WORK OUT NUMBERS
  ) 
