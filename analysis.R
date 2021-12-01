# initialize
rm(list=ls())
source("supportingFunctions.R")
library(ggplot2)

# get allData
#txt_to_csv("countryY/")
#compile_data(directory = "countryX", NA_rows = "warn")
#compile_data(directory = "countryY", NA_rows = "warn")

# summarize data
summarize_data()

### SUMMARY ###
# The disease likely began in Country X. This is because the cases started much earlier in Country X than they
# did in Country Y (as seen in the two graphs "Country X Infected Cases" and "Country Y Infected Cases").
