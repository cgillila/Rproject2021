########################
# text to csv          #
########################

# converts the txt files in a directory to csv files
txt_to_csv<-function(dir) {
  # lists the files in the directory
  dir_files = list.files(path=dir)
  
  # for each file in the directory, converts file to csv
  for (file in dir_files) {
    # checks that the file is a txt file
    if (substr(file, nchar(file)-3, nchar(file)) == ".txt") {
      # creates file path
      file_path = paste(dir, file, sep="")
      
      # reads data from files in input directory
      data<-read.table(file_path, header=TRUE, sep="\t", stringsAsFactors=FALSE)
      
      # if delimiter is not a tab, uses a space
      if (ncol(data) == 1) {
        data<-read.table(file_path, header=TRUE, sep=" ", stringsAsFactors=FALSE)
      }
      
      # gets name of file without .txt
      file_name = substr(file, 1, nchar(file)-4)
      
      # creates csv file name and path
      csv_file = paste(file_name, ".csv", sep="")
      csv_file_path = paste(dir, csv_file, sep="")
      
      # writes to csv
      write.table(x=data,file=csv_file_path,row.names=FALSE,col.names=TRUE,sep=",")
    }
  }
}


########################
# compile data         #
########################

# takes in a directory and string
## the string NA_rows must be "remove", "warn", or "include"
## assumes allData.csv exists, either empty or has data
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
    ## if user wants to remove NAs, then remove the rows with NAs
    if (rmNA) {
      temp_data <- temp_data[complete.cases(temp_data),]
    }
    
    ## if user wants to include NAs but warn, then warn about NAs
    if (warn && !complete.cases(temp_data)) {
      cat("WARNING: File", file , "has NA rows")
    }
    
    # add country column to the data
    temp_data$country = country
    
    # add dayofYear to data
    temp_data$dayofYear = strtoi(day)
    
    # append to allData
    ## check if allData is empty
    if (file.info("allData.csv")$size == 0) {
      # write col names if file is empty
      write.table(temp_data, file="allData.csv", append=F, sep=",", row.names=F, col.names=T)
    } else {
      # do not write col names and append data
      write.table(temp_data, file="allData.csv", append=T, sep=",", row.names=F, col.names=F)
    }
  }
}


########################
# summarize data       #
########################

# summarizes the compiled data into:
## number of screens run
## percent of patients screened that were infected
## male vs. female patients
## age distribution
# assumes compiled data stored as allData.csv
summarize_data<-function(){
  # load all data
  all_data = read.csv("allData.csv", header=T, stringsAsFactors=F)
  
  
  # find total number of screens run
  ## assuming every row is a unique screen
  total_screens <- nrow(all_data)
  cat("The number of total screens run: ", total_screens, "\n")
  
  
  # percent of patients that were infected
  total_infected = 0
  # loop through and count how many screens contain the protein
  for (i in seq(1,total_screens)) {
    for (j in seq(3,12)) {
      if (all_data[i,j] == 1) {
        total_infected = total_infected + 1
        break
      }
    }
  }
  # calculate the percent infected
  percent_infected = total_infected/total_screens * 100
  
  # print results
  cat("The percent of patients screened that were infected: ", percent_infected ,"%\n")
  
  
  # male vs. female patients
  number_male_patients = nrow(all_data[all_data$gender == "male",])
  number_female_patients = nrow(all_data[all_data$gender == "female",])
  cat("The number of male patients: ", number_male_patients, "\n")
  cat("The number of female patients: ", number_female_patients, "\n")
  
  
  # age distribution
  copy_all_data <- all_data
  copy_all_data$group <- cut(copy_all_data$age, breaks = c(0,10,20,30,40,50,60,70,80,400)
                             ,labels = c("0-10","10-20","20-30","30-40","40-50","50-60","60-70","70-80","80+"),
                             right=T)
  age_distribution <- ggplot(copy_all_data, aes(x=group, fill = gender)) +
    geom_bar()
  
  print(age_distribution)
  
  
  return (all_data)
}


