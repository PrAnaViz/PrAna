#' finalgplist
#'
#' Generate GP lists based on the shape file and GP address
#' 
#' @param coordinate coordinates generated from shape file
#' @param x polygon data generated from shape file
#' @param y map data generated from \code{data4map()} function 
#' @param z GP extract data generated from GP data
#'
#' @return a dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' finalgplist(coordinates, polygon_data, mapdata, gp_data_extract) 
#' }

finalgplist <- function(coordinate,x,y,z)
{
  a1 <- sp::over(coordinate,x[[2]])
  a2 <- y[which(!is.na(a1)),]
  a3 <- cbind(z, 
              Latitude = a2$'Latitude' [match(gsub(" ", "",z$'POSTCODE'), gsub(" ", "", a2$'locationID' ))],
              Longitude= a2$'Longitude' [match(gsub(" ", "",z$'POSTCODE'), gsub(" ", "", a2$'locationID' ))]
  )
  a4 <- a3[which(!is.na(a3$Latitude)),]
  a4
}

