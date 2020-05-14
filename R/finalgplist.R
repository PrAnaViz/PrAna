#' finalgplist
#'
#' @param coordinate
#' @param x
#' @param y
#' @param z
#'
#' @return
#' @export
#'

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

##Saltford_GP <- finalgplist(coordinates, Saltford,mapdata,full_2017)

