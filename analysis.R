# initialize
rm(list=ls())
source("supportingFunctions.R")
library(ggplot2)

### assumptions ###
# assumes countryY/ and countryX/ are in current working directory
# assumes allData.csv exists and is an empty file

# get allData
txt_to_csv("countryY/")
compile_data(directory = "countryX", NA_rows = "warn")
compile_data(directory = "countryY", NA_rows = "warn")

# summarize data
summarize_data()


### SUMMARY ###
# The disease likely began in Country X. This is because the cases started much earlier in Country X than they
# did in Country Y (as seen in the two graphs "Country X Infected Cases" and "Country Y Infected Cases").

# A vaccine developed for Country Y is likely not to work for Country X. Differences in which markers are present
# in a screening indicate differences in the protein of the disease and possibly a difference in a patient's response to
# the bacteria within their immune system. This means that, if the people of Country Y and Country X have largely
# different markers appearing in their screenings, a vaccine meant to target a specific variant of the bacteria in one
# country would likely be ineffective in the other. This is the case for these countries, as Country X has a prevalence
# of markers 1-5 while Country Y has a prevalence of markers 6-10 with some 4 and 5 (as seen in the graphs "Marker Count of
# Country X" and "Marker Count of Country Y"). Therefore, the disease has evolved enough while traveling between
# countries that a vaccine from Country Y would probably see resistance from the disease in Country X and be ineffective.
