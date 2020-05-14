#' subset_shapefile
#'
#' @param shapefile shape file
#' @param reg_name region name
#'
#' @return
#' @export
#'
#'
subset_shapefile <- function(shapefile, reg_name) {
  a <- shapefile[shapefile$ccg18nm == reg_name,]
  s <- sp::spTransform(a, CRS("+init=epsg:4326"))
  s_data <- ggplot2::fortify(s)
  s_polygon <- sp::SpatialPolygons(s@polygons)
  return( list(s_data,s_polygon ))
}
