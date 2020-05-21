#' practice_wise
#'
#' Import NHS dataset files and generate processed individual GP practice prescription dataset
#'
#' @param x processed prescription dataset
#' @param y a character vector consisting of GP practices
#' @param z processed BNF  mapped file
#' 
#' @return multiple individual GP practice wise processed prescription dataset
#' @export
#' 
#' @importFrom utils write.csv
#' @importFrom dplyr %>%
#' 
#' @import tidyverse
#' @examples
#' \dontrun{
#' practice_wise(data201712_03,UK_GP2017)
#' }
#' 
practice_wise <- function(x,y,z) {
  
  tab01 <- NULL
  tab02 <- NULL
  tab03 <- NULL
  tab04 <- NULL
  tab05 <- NULL
  tab06 <- NULL
  tab07 <- NULL
  s <- NULL
  
  for (i in (1:length( unique(y)  )))
  {
    
    j = unique(y)[i]
    
    tab01 <- x %>%
      dplyr::filter(PRACTICE %in% j) %>%
      data.table::setnames(4,"BNFCODE") %>%
      merge(z,by = "BNFCODE", all = F) %>%
      dplyr::select("PERIOD","PRACTICE","BNFCODE","MDR","VPPID","VPID.x","ISID","value","UOM","ITEMS","QUANTITY")
    
    s <- strsplit(tab01$ISID, split = "#")
    
    tab02<- data.frame(
      PERIOD = rep(tab01$'PERIOD', sapply(s, length)),
      PRACTICE = rep(tab01$'PRACTICE', sapply(s, length)),
      BNF_CODE = rep(tab01$'BNFCODE', sapply(s, length)),
      VPPID = rep(tab01$'VPPID', sapply(s, length)),
      VPID = rep(tab01$'VPID.x', sapply(s, length)),
      MDR = rep(tab01$'MDR', sapply(s, length)),
      ITEMS = rep(tab01$'ITEMS', sapply(s, length)),
      QUANTITY = rep(tab01$'QUANTITY', sapply(s, length)),
      API = unlist(strsplit(tab01$'ISID', split ="#")),
      Vol_Mass =  sapply(unlist(strsplit(tab01$'value', split ="#")), as.numeric),
      UOM = unlist(strsplit(tab01$'UOM', split = "#"))
    )
    tab03 <- cbind(tab02,Vol_Mass_Tot = (tab02$'Vol_Mass'*tab02$'QUANTITY')* (uom_2$'multi_fac'[match(gsub(" ", "",tab02$'UOM'), gsub(" ", "",  uom_2$'CD'))]))
    tab04 <- cbind(tab03, DForm = dform$'FORMCD'[match(gsub(" ", "",tab03$'VPID'), gsub(" ", "",  dform$'VPID'))])
    
    tab05  <- tab04 %>%
      dplyr::select("PERIOD", "PRACTICE","API","Vol_Mass_Tot","DForm")%>%
      dplyr::mutate(API= as.character(API))%>%
      dplyr::mutate(API_CODE = API) %>%
      dplyr::group_by(API_CODE,PRACTICE,PERIOD,DForm)%>%
      dplyr::summarize(gram = sum(Vol_Mass_Tot, na.rm = T)) %>%
      dplyr::left_join(ing2[,c(1,5)], by="API_CODE") %>%
      dplyr::mutate(gram = gram/1000)%>%
      dplyr::select("NM", "API_CODE","PRACTICE","PERIOD","DForm","gram")
    
    tab06 <- aggrapi(tab05[, c("NM", "gram")])
    
    tab07 <- cbind(CPD = tab06$'.id' [match(gsub(" ", "",tab05$"NM"), gsub(" ", "", tab06$'NM'))],tab05)
    
    write.csv(tab07, paste0( j,".csv"))
    
  }
}
