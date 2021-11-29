# initialize
rm(list=ls())
source("supportingFunctions.R")
library(ggplot2)

# takes in a directory and string
## the string NA_rows must be "remove", "warn", or "include"
compile_data<-function(directory, NA_rows){
  # get all the files names
  file_names <- list.files(path=directory, pattern=".csv", full.names=T)
  
  # deal with the NAs accordingly
  rmNA <- F
  warn <- F
  if (NA_rows == "remove") {
    # set bool to remove NA rows
    rmNA <- T
  }
  if (NA_rows == "warn") {
    # set bool to keep but warn NA rows
    warn <- T
  }
  
  # get country name
  country <- substring(directory,8,8)
  
  # loop through files in the directory
  for (file in file_names) {
    # load data into temp data frame
    temp_data = read.csv(file, header=T, stringsAsFactors=F)
    day <- substring(file,17,19)
    
    # refine data frame depending on NAs
    if (rmNA) {
      temp_data <- temp_data[complete.cases(temp_data),]
    }
    
    if (warn && !complete.cases(temp_data)) {
      cat("File " + file + " has NA rows")
    }

    # add country column to the data
    temp_data$country = country
    
    # add dayofYear to data
    temp_data$dayofYear = day
    
    # append to allData
    ## check if allData is empty
    if (file.info("allData.csv")$size == 0) {
      # write col names if empty
      col_name = T
    } else {
      # do not write col names
      col_name = F
    }
    write.table(temp_data, file="allData.csv", append=T, sep=",", row.names=F, col.names=col_name)
  }
  
  return (0)
}

result <- compile_data(directory = "countryX", NA_rows = "remove")
