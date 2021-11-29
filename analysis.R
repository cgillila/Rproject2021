# initialize
rm(list=ls())
source("supportingFunctions.R")
library(ggplot2)

# takes in a directory and string, the string NA_rows must be
# "remove", "warn", or "include"
compile_data<-function(directory, NA_rows){
  # get all the files names
  file_names <- list.files(path=directory, pattern=".csv", full.names=T)
  
  # deal with the NAs accordingly
  rmNA <- F
  warn <- F
  if (NA_rows == "remove") {
    rmNA <- T
  }
  if (NA_rows == "warn") {
    warn <- T
  }
  
  for (file in file_names) {
    temp_data = read.csv(file, header=T)
  }
  return (file_names)
}

result <- compile_data(directory = "countryX", NA_rows = "remove")
print(result)