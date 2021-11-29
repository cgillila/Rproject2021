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

txt_to_csv("countryY/")
