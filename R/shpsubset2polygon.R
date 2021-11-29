#' shpsubset2polygon
#'
#' Convert shpae files to the polygons 
#' 
#' @param shapefile shape file location
#' @param region region of interest
#'
#' @return a dataframe, polygon file
#' @export
#'
#' @examples
#' \dontrun{
#' shpsubset2polygon("C:/dataset/shape_files","NHS Bath and North East Somerset CCG")
#' }
#' 
shpsubset2polygon <- function(shapefile, region)
{
  # Read shape file
  a <- sf::st_read(shapefile) %>%
    dplyr::rename(region_name = 3) %>%
    dplyr::filter(region_name %in% region)
  
  nc_sp <- sf:::as_Spatial(a$geom)
  
  s <- sp::spTransform(nc_sp, CRS("+init=epsg:4326"))
  s_data <- ggplot2::fortify(s)
  s_polygon <- sp::SpatialPolygons(s@polygons)
  return( list(s_data,s_polygon ))
}
