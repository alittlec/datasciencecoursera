pollutantmean <- function(directory, pollutant, id=1:332){
  
  curr_dir <- getwd()
  setwd(directory)
  filenames <- paste(formatC(id, width=3, flag="0"),".csv",sep = "")
  poll_list <- vector(mode="numeric")
  for (i in seq_along(filenames)) {
    file <-read.csv(filenames[i], header = TRUE)
    poll_list<-append(poll_list,unlist(file[pollutant]))
  }
  setwd(curr_dir)
  mean(poll_list, na.rm = TRUE)

}