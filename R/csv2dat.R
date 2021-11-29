#' csv2dat
#' 
#' Combine and convert .CSV file(s) in the defined path to a \code{dataframe} 
#'
#' @param path location for the target .CSV file(s) 
#'

#' @return \code{dataframe} 
#' @export


csv2dat <- function(path)
{
  setwd(path)
  # Need following library plyr, data.table, readr
  memory.limit(size = 750000)
  # Select the .CSV files in the path
  a1 <- list.files(path= path, pattern = ".csv")
  
  # Convert .CSV files to dataframe inside a list and then convert it into a single dataframe
  a2 <- plyr::ldply(lapply(a1, data.table::fread),data.frame)
  
  a2
  
}



##csv2rds("C:/Datasets/Prescription Datasets/2018/PDPI","Full_2018_12.rds")
