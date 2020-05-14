## Function to aggregate

#' aggrapi
#'
#' @param y dataframe
#'
#' @return dataframe
#' @export
#'
aggrapi <- function(y) {
  tab0 <- NULL
  tab1 <- NULL
  tab2 <- NULL
  tab3 <- NULL

  tab3 <-as.data.frame(unique(stringr::str_split_fixed(y$NM, " ",n=2)))[1]%>%
    mutate(V1= as.character(V1))

  tab0 <- as.data.frame(tab3[!duplicated(tab3$`V1` ), ])
  colnames(tab0)[1] <- c("V1")

  for (i in (1:nrow(tab0)))
  {

    tab1[[paste (tab0[i,])]] <- y[which(grepl(tab0[i,] ,y$NM)) ,]
    tab1
  }
  tab2 <- plyr::ldply(tab1,data.frame)
  tab2

}
