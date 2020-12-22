# Clean up R environment - This is not necesary, but I like to do it. 
rm(list = ls()) 

library(dplyr)
library(lubridate)

# LOAD FUNCTIONS FROM myFLIC.R
setwd("/Users/jpinzon/Documents/UTSW/Projects/Jmjc Project/03 Feeding preference/00 FLIC")
source("myFLIC.R")

# Set directory to data
setwd("/Users/jpinzon/Documents/UTSW/Projects/Jmjc Project/03 Feeding preference/00 FLIC/60811m1")

# READ THE FILE:
df_a=read_data_flic("DFM_1_2.csv")
# Create a column with the time from first record
df_a$time=difftime(df_a$ntime,df_a$ntime[1],  units=c("mins"))

PI_split(split_df_len(df_a,60,10), df_a)

