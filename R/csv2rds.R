
#' csv2rds
#' 
#' Convert .CSV file(s) in the target path to a .rds file in the defined folder
#'
#' @param path location for the target .CSV file(s) 
#' @param output location for the output .rds file
#'
#' @return .rds file in the output location
#' @export


csv2rds <- function(path,output)
{
  setwd(path)
  # Need following library plyr, data.table, readr
  memory.limit(size = 750000)
  # Select the .CSV files in the path
  a1 <- list.files(path= path, pattern = ".CSV")
  
  # Convert .CSV files to dataframe inside a list and then convert it into a single dataframe
  a2 <- plyr::ldply(lapply(a1, data.table::fread),data.frame)
  
  # Write rds file from the dataframe
  readr::write_rds(a2,output,compress = "xz")
  
}



##csv2rds("C:/Datasets/Prescription Datasets/2018/PDPI","Full_2018_12.rds")
