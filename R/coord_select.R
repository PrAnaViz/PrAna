#' coord_select
#'
#' @param x
#' @param y
#' @param z
#' @param w
#'
#' @return
#' @export
#'
#'
coord_select <- function(x,y,z,w) {
  tab1 <- NULL
  tab2 <- NULL
  for (i in (1:length(x)))
  {

    tab1[[paste (names(x)[i])]] <- finalgplist(y,(x)[[i]],z,w)
    tab2[[paste (names(x)[i])]] <- as.data.frame( unique(tab1[[paste (names(x)[i])]]$PRACTICE))
    tab2
  }
  tab3 <- ldply(tab2,data.frame)
  tab3
}
