#' subset_shape2
#'
#' @param x
#' @param y
#'
#' @return
#' @export
#'

subset_shape2 <- function(x,y) {
  tab0 <- NULL
  tab1 <- NULL
  for (i in (1:length(x)))
  {
    tab1[[paste (x[i])]] <- subset_shapefile(y,x[i])
    tab1
  }
  tab1
}
