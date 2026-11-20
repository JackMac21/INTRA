# Calcs check
library(dplyr)

orig <- read.csv("full14Cdata.csv")
orig_ex <- orig %>% filter(
  Reported_Data_Type == "Fmodern+14CAge") %>%
  group_by(First_Author, Year_Pub) %>%
  slice_head (n=1) %>%
  mutate(across(where(is.numeric), ~ round(.x, digits = 1))) %>%
  ungroup()


calc_df <- read.csv("INTRAWithCalculations_AH.csv")[-1]

test_ex <- calc_df %>% filter(
  Reported_Data_Type == "Fmodern+14CAge") %>%
  group_by(First_Author, Year_Pub) %>%
  slice_head (n=1) %>%
  mutate(across(where(is.numeric), ~ round(.x, digits = 1)))
  
  
result <- orig_ex %>%
  inner_join(test_ex, by = "Key") %>%
  filter(Age_Corrected_D14C.x != Age_Corrected_D14C.y) %>%
  select(First_Author.x, Year_Pub.x, Age_Corrected_D14C.x, Age_Corrected_D14C.y)


