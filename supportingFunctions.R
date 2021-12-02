# Coleen Gillilan
# Megan Cater
# bios30318
# R Project
# supportingFunctions.R

########################
# text to csv          #
########################

# converts the txt files in a directory to csv files
txt_to_csv<-function(dir) {
  # list the files in the directory
  dir_files = list.files(path=dir)
  
  # for each file in the directory, convert file to csv
  for (file in dir_files) {
    # check that the file is a txt file
    if (substr(file, nchar(file)-3, nchar(file)) == ".txt") {
      # create file path
      file_path = paste(dir, file, sep="")
      
      # read data from files in input directory
      data<-read.table(file_path, header=TRUE, stringsAsFactors=FALSE)
      
      # get name of file without .txt
      file_name = substr(file, 1, nchar(file)-4)
      
      # create csv file name and path
      csv_file = paste(file_name, ".csv", sep="")
      csv_file_path = paste(dir, csv_file, sep="")
      
      # write to csv
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
  
  
  ### find total number of screens run ###
  ## assuming every row is a unique screen
  total_screens <- nrow(all_data)
  cat("The number of total screens run: ", total_screens, "\n")
  
  
  ### percent of patients that were infected ###
  ## check if any of markers are a 1
  total_infected <- nrow(all_data[all_data$marker01==1 | all_data$marker02==1 | all_data$marker03==1 |
                         all_data$marker04==1 | all_data$marker05==1 | all_data$marker06==1 |
                         all_data$marker07==1 | all_data$marker08==1 | all_data$marker09==1 |
                         all_data$marker10==1, ])

  # calculate the percent infected
  percent_infected = total_infected/total_screens * 100
  
  # print results
  cat("The percent of patients screened that were infected: ", percent_infected ,"%\n")
  
  
  ### male vs. female patients ###
  number_male_patients = nrow(all_data[all_data$gender == "male",])
  number_female_patients = nrow(all_data[all_data$gender == "female",])
  cat("The number of male patients: ", number_male_patients, "\n")
  cat("The number of female patients: ", number_female_patients, "\n")
  
    # plot age distribution
  copy_all_data <- all_data
  copy_all_data$group <- cut(copy_all_data$age, breaks = c(0,10,20,30,40,50,60,70,80,400),
                             labels = c("0-10","10-20","20-30","30-40","40-50","50-60","60-70","70-80","80+"),
                             right=T)
  age_distribution <- ggplot(copy_all_data, aes(x=group, fill = gender)) +
    geom_bar() +
    ggtitle("Age Distribution of all Screenings") +
    xlab("Age Group") +
    ylab("Number of Individuals")
  
  print(age_distribution)
  
  
  ### which country it likely began in ###
  ## get data on country X
  x_data <- all_data[all_data$country == "X",]
  x_infected <- x_data[x_data$marker01==1 | x_data$marker02==1 | x_data$marker03==1 |
                       x_data$marker04==1 | x_data$marker05==1 | x_data$marker06==1 |
                       x_data$marker07==1 | x_data$marker08==1 | x_data$marker09==1 |
                       x_data$marker10==1, ]
  
  ## get data on country Y
  y_data <- all_data[all_data$country == "Y",]
  y_infected <- y_data[y_data$marker01==1 | y_data$marker02==1 | y_data$marker03==1 |
                         y_data$marker04==1 | y_data$marker05==1 | y_data$marker06==1 |
                         y_data$marker07==1 | y_data$marker08==1 | y_data$marker09==1 |
                         y_data$marker10==1, ]
  
  
  # plot the days and the count of cases
  x_infected_graph <- ggplot(x_infected, aes(x=dayofYear)) +
    geom_bar(fill="red") +
    xlab("day of Year") +
    ylab("number of positive cases") +
    ggtitle("Country X Infected Cases") +
    xlim(119,180) +
    ylim(0,500)
  y_infected_graph <- ggplot(y_infected, aes(x=dayofYear)) +
    geom_bar(fill="blue") +
    xlab("day of Year") +
    ylab("number of positive cases") +
    ggtitle("Country Y Infected Cases")+
    xlim(119,180) +
    ylim(0,500)

  # print results
  print(x_infected_graph)
  print(y_infected_graph)

  
  ### Determine marker count ###
  # Counts the number of appearances of each marker per country and stores the data
  markers <- data.frame(marker = seq(1,10), 
                        totalX = colSums(x_data[, 3:12] != 0),
                        totalY = colSums(y_data[, 3:12] != 0))
  markers$marker <- as.factor(markers$marker)
  
  # Plots Country X's marker data
  x_marker_graph <- ggplot(markers, aes(x = marker, y = totalX, fill = marker)) + 
    geom_bar(stat = "identity", show.legend = FALSE) +
    ggtitle("Marker Count of Country X") +
    xlab("Marker") + ylab("Total") +
    theme_minimal()
  
  # Plots Country Y's marker data
  y_marker_graph <- ggplot(markers, aes(x = marker, y = totalY, fill = marker)) + 
    geom_bar(stat = "identity", show.legend = FALSE) +
    ggtitle("Marker Count of Country Y") +
    xlab("Marker") + ylab("Total") +
    theme_minimal()
  
  # print results
  print(x_marker_graph)
  print(y_marker_graph)
}
