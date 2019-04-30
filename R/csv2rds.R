
#' csv2rds - Convert .CSV file(s) in the path defined to .rds file in a desired location
#'
#' @param path .CSV file(s) location
#' @param output desired output .rds file with location
#'
#' @return .rds file in a desired location
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



##csv2rds("C:/Datasets/Prescription Datasets/2018/PDPI","Full_2018_07")
