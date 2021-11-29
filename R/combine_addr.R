#' combine_addr
#'
#' Combine and convert \code{ADDR} files in the defined path to a \code{dataframe}
#'
#' @param  path location for the \code{ADDR} files
#'
#' @return \code{dataframe} 
#' @export
#'

combine_addr <- function(path)
{
  setwd(path)
  # Need following library plyr, data.table, readr
  memory.limit(size = 750000)
  # Select the .CSV files in the path
  a1 <- list.files(path= path, pattern = c(".csv"))
  
  # Convert .CSV files to dataframe inside a list and then convert it into a single dataframe
  a2 <- plyr::ldply(lapply(a1, data.table::fread),data.frame)
  a2[9] = NULL
  
  colnames(a2) <- c("PERIOD",
                    "PRACTICE",
                    "SURGERY_NAME",
                    "ADDRESS1",
                    "ADDRESS2",
                    "TOWN",
                    "COUNTY",
                    "POSTCODE"
  )
  
  a2
}
