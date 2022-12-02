library(dplyr)
library(readr)
  
calories <- read_csv(col_names = "cal", skip_empty_rows = FALSE,
"1000
 2000
 3000

 4000

 5000
 6000
  
 7000
 8000
 9000
  
 10000")
  
# calories per elf
elf_df <- calories %>% 
  # create elf ids as cumulative sum of elf separators
  mutate(elf_separator = ifelse(is.na(cal), 1L, 0L)) %>%
  mutate(elf_id = cumsum(elf_separator) + 1L) %>%
  group_by(elf_id) %>%
  summarise(elf_cal = sum(cal, na.rm = TRUE)) 

# best elf
print(elf_df %>% filter(elf_cal == max(elf_cal)))

# best 3 elves
print(elf_df %>% top_n(3, elf_cal) %>% summarise(total_cal = sum(elf_cal)))
  