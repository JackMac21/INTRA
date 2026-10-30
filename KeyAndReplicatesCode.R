library(dplyr)
library(stringr)
library(validate)

rm(list = ls())

data <- read.csv("https://raw.githubusercontent.com/JackMac21/INTRA/refs/heads/main/full14Cdata.csv")

data_temp <- data[ , -which (names(data) %in% c("Key", "Rep_No"))] #remove Key and Rep_No columns from repo


# QUALITY CONTROL 

# Remove leading, trailing spaces from character fields
data_temp <- data_temp %>% 
  mutate(across(where(is.character), trimws))

# Check for duplicate rows
data_temp[duplicated(data_temp), ]

# Check for right class of columns (adjust so that this works for the right data cols 14C age etc)
class_rules <- validator(
  is.numeric(Age_Corrected_D14C),
  is.numeric (Age_Corrected_D14C_Error),
  is.numeric(Dated_Year)
)
confront(data_temp, class_rules)

# Validity: check the right range for D14C and error (other cols?)
value_rules <- validator(
  NAS = !is.na(Age_Corrected_D14C),
  D14CRange = Age_Corrected_D14C <= 250 & Age_Corrected_D14C >= -50,
  D14CERange = Age_Corrected_D14C_Error <= 10 & Age_Corrected_D14C_Error >= 0
)

results <- confront(data_temp, value_rules)
summary(results)
violating(data_temp, value_rules)

# CALCULATIONS, ADDED COLS

# Create a column for named Replicate_No
dataWithReps <- data_temp %>%
  group_by(Tree_Series_ID, Dated_Year, EW_LW_WW) %>%
  mutate(Replicate_No = sprintf("rep_%02d", row_number())) %>%
  ungroup()

# make some duplicates that would result in duplicate keys
data_dups <- rbind(dataWithReps, dataWithReps[c(1:5), ])

# Create a primary key using First_Author, Year_Pub, Tree_Series_ID, Dated_Year, WW, Replicate

dataWithKey <- dataWithReps %>% mutate(PrimaryKey = paste0(
  str_pad(str_sub(Tree_Series_ID, 1, 12), 12, side = "right", pad = "@"),
  "_",
  str_pad(str_sub(Dated_Year, 1, 8), 8, side = "right", pad = "@"),
  "_",
  str_pad(str_sub(EW_LW_WW, 1, 2), 2, side = "right", pad = "@"),
  "_",
  str_pad(str_sub(Replicate_No, -2, -1), 2, side = "right", pad = "@")
)
)

# Check for duplicates?

duplicates <- dataWithKey %>%
  count(PrimaryKey) %>%
  filter(n > 1)

if (nrow(duplicates) == 0) {
  message("No duplicates found.")
} else {
  message(nrow(duplicates), " key(s) appear more than once:")
  print(duplicates, n = Inf)
}

write.csv(dataWithKey, "C:/Users/jackm/Downloads/dataWithKey.csv")
