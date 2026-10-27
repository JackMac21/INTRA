library(dplyr)
library(ggplot2)

data <- read.csv("C:/Users/jackm/Downloads/Test_Data.csv")
filtered_data <- data %>% filter(data$Reported_Data_Type == "D14C+Fmodern+14CAge")
filtered_data
write.csv(filtered_data, "C:/Users/jackm/Downloads/filtered14Cdata.csv")

testbp <- calendar_to_bp(filtered_data$Dated_Year)
resbp <- abs(filtered_data$Dated_Years_BP - testbp$YearsBP)
max(resbp, na.rm = TRUE)
#some residuals of 0.05 are present, almost certainly due to .xlsx rounding - calculation should be good, but QC rows with this issue

testbcece <- bp_to_calendar(filtered_data$Dated_Years_BP)
resbcece <- abs(filtered_data$Dated_Year - testbcece$DatedCalendarYear)
max(resbcece, na.rm = TRUE)
#same issue here as above - should be good

testd14Cfromfm <- fm_to_delta14c(filtered_data$Fraction_Modern, filtered_data$Fraction_Modern_Error, filtered_data$Dated_Years_BP)
resd14Cfromfm <- abs(filtered_data$Age_Corrected_D14C - testd14Cfromfm$Delta14C)
resd14CfromfmError <- abs(filtered_data$Age_Corrected_D14C_Error - testd14Cfromfm$Delta14C_error)
max(resd14Cfromfm, na.rm = TRUE)
max(resd14CfromfmError, na.rm = TRUE)
#Largest residual is 0.129 - expected
#Largest error residual (excluding NaN) is 0.125 - expected

testfmfromd14C <- delta14c_to_fm(filtered_data$Age_Corrected_D14C, filtered_data$Age_Corrected_D14C_Error, filtered_data$Dated_Years_BP)
resfmfromd14C <- abs(filtered_data$Fraction_Modern - testfmfromd14C$Fm)
resfmfromd14CError <- abs(filtered_data$Fraction_Modern_Error - testfmfromd14C$Fm_error)
max(resfmfromd14C, na.rm = TRUE)
max(resfmfromd14CError, na.rm = TRUE)
#Largest residual is 0.000112 - expected
#Largest error residual is 0.000108 - expected

testd14Cfrom14cage <- radAge_to_delta14C(filtered_data$Radiocarbon_Age, filtered_data$Radiocarbon_Age_Error, filtered_data$Dated_Years_BP)
resd14cfrom14cage <- abs(filtered_data$Age_Corrected_D14C - testd14Cfrom14cage$Delta14C)
resd14cfrom14cageError <- abs(filtered_data$Age_Corrected_D14C_Error - testd14Cfrom14cage$Delta14C_error)
max(resd14cfrom14cage, na.rm = TRUE)
max(resd14cfrom14cageError, na.rm = TRUE)
#Largest residual is 0.722 (quite large)
#Largest error residual is 0.357 (quite large)
#Test if residuals are correlated to dated year
testGraph <- filtered_data %>% mutate(res = resd14cfrom14cage, res2 = resd14cfrom14cageError)
residGraph <- ggplot(data = testGraph, aes(x = Dated_Year, y = resd14cfrom14cage))+
  geom_point()
residGraph
errorresidgraph <- ggplot(data = testGraph, aes(x = Dated_Year, y = resd14cfrom14cageError))+
  geom_point()
errorresidgraph
#residuals are uncorrelated with dated year (clustered around high-replication 774 CE and 993 CE)
#Some studies, namely Black_23, round 14c age to the nearest 10 years, and 14c age error to the nearest 5 years.
#According to Claude, it is very likely that this rounding is the cause of the error (up to ~0.62 per mil d14C residuals and ~0.31 per mil d14C error residuals are expected).
#for conversion to delta14C, fmodern should always be attempted first; leave note in protocol/manuscript about this issue and notify technicians.

test14cagefromd14c <- delta14C_to_radAge(filtered_data$Age_Corrected_D14C, filtered_data$Age_Corrected_D14C_Error, filtered_data$Dated_Years_BP)
res14cagefromd14c <- abs(filtered_data$Radiocarbon_Age - test14cagefromd14c$RadAge)
res14cagefromd14cError <- abs(filtered_data$Radiocarbon_Age_Error - test14cagefromd14c$RadAge_error)
max(res14cagefromd14c, na.rm = TRUE)
max(res14cagefromd14cError, na.rm = TRUE)
#Largest residual is 6 years
#Largest error residual is 3 years
#Shouldn't be an issue, as these calculated 14c ages should actually be more accurate than the rounded ages.
#Potentially calculate 14c ages with decimals instead of rounding to the nearest year for more accuracy?

test14cagefromfm <- fm_to_radAge(filtered_data$Fraction_Modern, filtered_data$Fraction_Modern_Error)
res14cagefromfm <- abs(filtered_data$Radiocarbon_Age - test14cagefromfm$RadAge)
res14cagefromfmError <- abs(filtered_data$Radiocarbon_Age_Error - test14cagefromfm$RadAge_error)
max(res14cagefromfm, na.rm = TRUE)
max(res14cagefromfmError, na.rm = TRUE)
#Largest residual is 5 years
#Largest error residual is 3 years
#Shouldn't be an issue, as these calculated 14c ages should actually be more accurate than the rounded ages.
#Potentially calculate 14c ages with decimals instead of rounding to the nearest year for more accuracy?

testfmfrom14cage <- radAge_to_fm(filtered_data$Radiocarbon_Age, filtered_data$Radiocarbon_Age_Error)
resfmfrom14cage <- abs(filtered_data$Fraction_Modern - testfmfrom14cage$Fm)
resfmfrom14cageError <- abs(filtered_data$Fraction_Modern_Error - testfmfrom14cage$Fm_error)
max(resfmfrom14cage, na.rm = TRUE)
max(resfmfrom14cageError, na.rm = TRUE)
#Largest residual is 0.00052 (quite large)
#Largest error residual is 0.00031 (quite large)
#Some studies, namely Black_23, round 14c age to the nearest 10 years, and 14c age error to the nearest 5 years.
#According to Claude, it is very likely that this rounding is the cause of the error (up to ~0.00054 fm residuals and ~0.00027 fm error residuals are expected).
#for conversion to fm, d14C should always be attempted first; leave note in protocol/manuscript about this issue and notify technicians.

