# initialize
rm(list=ls())
source("supportingFunctions.R")
library(ggplot2)

# get allData
#txt_to_csv("countryY/")
#compile_data(directory = "countryX", NA_rows = "warn")
#compile_data(directory = "countryY", NA_rows = "warn")

# summarize data
all_data <- summarize_data()

