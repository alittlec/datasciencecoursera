corr <- function(directory, threshold=0){
  
  specdata <- readfiles(directory,1:332) #list of dfs with file contents
  ret_data <- numeric()
  
  for(i in seq_along(specdata)){
    monitor <- specdata[[i]] 
    monitor <- na.omit(monitor) #strip out NAs
    if (nrow(monitor) > threshold){
      ret_data <- c(ret_data, cor(monitor$sulfate, monitor$nitrate) ) #gather result
    }
  }

  ret_data
}