#' data4map
#' Add the geo location IDs to the GP address files
#' @param x GP ADDR files
#' @param y postcode dataset
#'
#' @return a dataframe
#' @export
#'

data4map <- function(x,y)
{
  a1 <- as.matrix(gsub(" ", "",x$POSTCODE))
  a2 <- subset(y,gsub(" ", "",y$postcode)  %in% c(a1))
  colnames(a2)[1] <- "locationID"
  a2
}