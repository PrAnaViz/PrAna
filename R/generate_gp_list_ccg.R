#' generate_gp_list_ccg
#'
#' @param shape_file Shape file location
#' @param full_postcode Full postcode with geo information
#' @param gp_data GP extracted data location
#' @param region CCG region name
#'
#' @return a character vector file with GP Practice codes
#' @importFrom readr read_csv
#' @importFrom dplyr select
#' @export
#'

generate_gp_list_ccg <- function(shape_file, region,full_postcode, gp_data) {
  
  ADDR_names <- c( "PERIOD",
                   "PRACTICE_CODE",
                   "PRACTICE_NAME",
                   "ADDRESS_1",
                   "ADDRESS_2",
                   "ADDRESS_3",
                   "ADDRESS_4",
                   "POSTCODE")
  
  gp_data_extract <- readr::read_csv(gp_data, 
                              col_names = F) %>%
    data.table::setnames (ADDR_names)
  
  polygon_data <- shpsubset2polygon(shape_file, region)
  mapdata <- data4map(gp_data_extract,full_postcode)
  
  # generate second set of unique location IDs for second layer of selected locations
  mapdata$secondLocationID <- paste(as.character(mapdata$locationID), "_selectedLayer", sep="")
  colnames(mapdata) <-c ("locationID","Latitude","Longitude","secondLocationID") 
  coordinates <- sp::SpatialPointsDataFrame(mapdata[,c('Longitude', 'Latitude')] , mapdata)
  
  ## For Final GP lists based on area
  full_gp_list <- finalgplist(coordinates, polygon_data, mapdata, gp_data_extract) %>%
    dplyr::mutate(region = region)
  
  gp_codes <- full_gp_list %>%
    dplyr::select(PRACTICE_CODE) %>%
    unique() %>%
    data.table::setnames("PRACTICE")
  
  gp_codes_period <- full_gp_list %>%
    dplyr::select(PERIOD, PRACTICE_CODE) %>%
    unique()
  
  
  newlist <- list(gp_codes = gp_codes, gp_codes_period = gp_codes_period, full_gp_list = full_gp_list)
  return(newlist)
}
