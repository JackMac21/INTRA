#read in data
df <- read.csv("https://raw.githubusercontent.com/JackMac21/INTRA/refs/heads/main/full14Cdata.csv")

#new columns to track which values were calculated (applies to the value and its error)
df$d14c_calculated <- FALSE
df$fm_calculated   <- FALSE
df$age_calculated  <- FALSE

#auto calculation loop
for (i in seq_len(nrow(df))) {
  
  #Allow for extraction of unit types from "reported_data_type" strings
  str_d14c <- "D14C"
  str_fm <- "Fmodern"
  str_14cage <- "14CAge"
  
  reported <- df$Reported_Data_Type[i]
  
  #Which units were reported, based on the type column
  has_d14c <- grepl(str_d14c, reported)
  has_fm   <- grepl(str_fm,   reported)
  has_age  <- grepl(str_14cage,  reported)
  
  #skip unrecognized rows, or rows where all three were reported
  if (!(has_d14c || has_fm || has_age)) {
    message(sprintf("Error: unable to perform calculations for row %d (no D14C, Fm, or 14C Age found)", i))
    next
  }
  if (has_d14c && has_fm && has_age) next
  
  #d14c not reported: calculate value and error from fmodern, else age
  if (!has_d14c) {
    if (has_fm) {
      result <- fm_to_delta14c(df$Fraction_Modern[i], df$Fraction_Modern_Error[i], df$Dated_Years_BP[i])
    } else if (has_age) {
      result <- radAge_to_delta14C(df$Radiocarbon_Age[i], df$Radiocarbon_Age_Error[i], df$Dated_Years_BP[i])
    }
    df$Age_Corrected_D14C[i] <- result$Delta14C
    df$Age_Corrected_D14C_Error[i] <- result$Delta14C_error
    df$d14c_calculated[i] <- TRUE
  }
  
  #fmodern not reported: calculate value and error from d14c, else age
  if (!has_fm) {
    if (has_d14c) {
      result <- delta14c_to_fm(df$Age_Corrected_D14C[i], df$Age_Corrected_D14C_Error[i], df$Dated_Years_BP[i])
    } else if (has_age) {
      result <- radAge_to_fm(df$Radiocarbon_Age[i], df$Radiocarbon_Age_Error[i])
    }
    df$Fraction_Modern[i] <- result$Fm
    df$Fraction_Modern_Error[i] <- result$Fm_error
    df$fm_calculated[i] <- TRUE
  }
  
  #14c age not reported: calculate value and error from d14c, else fmodern
  if (!has_age) {
    if (has_d14c) {
      result <- delta14C_to_radAge(df$Age_Corrected_D14C[i], df$Age_Corrected_D14C_Error[i], df$Dated_Years_BP[i])
    } else if (has_fm) {
      result <- fm_to_radAge(df$Fraction_Modern[i], df$Fraction_Modern_Error[i])
    }
    df$Radiocarbon_Age[i] <- result$RadAge
    df$Radiocarbon_Age_Error[i] <- result$RadAge_error
    df$age_calculated[i] <- TRUE
  }
}

#replace with intended output path
write.csv(df, "C:/Users/jackm/Downloads/INTRAWithCalculations2.csv")
