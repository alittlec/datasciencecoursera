readfiles <- function(directory, id){
  filenames <- paste(directory, "/", formatC(id, width=3, flag="0"), ".csv", sep="")
  specdata <- list(length = length(filenames))
  for (i in seq_along(filenames)){
    specdata[[i]] <- read.csv(filenames[i])
  }
  specdata
}

complete <- function(directory,id=1:332){
  
  specdata <- readfiles(directory, id)
  num_comp_case <- ret_id <- numeric()
  
  for (i in seq_along(specdata)){
    
    filedata <- specdata[[i]]
    num_comp_case[i] <- sum(complete.cases(filedata))
    ret_id[i]<-filedata$ID[i]

  }
  ret_data<-data.frame(id=ret_id,nobs=num_comp_case)
  ret_data

}