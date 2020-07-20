#' practice_wise
#'
#' Import NHS dataset files and generate processed individual GP practice prescription dataset
#'
#' @param presdata combined prescription data \code{data.frame} generated using \code{csv2dat()} function
#' @param gpvector \code{character} vector consisting GP practices
#' @param apimap BNF code mapped \code{data.frame} generated using \code{importdmd()} function
#' @param uom a \code{data.frame} generated using \code{importdmd()} function containing different unit of measurements and its corresponding codes
#' @param dform a \code{tibble} generated using \code{importdmd()} function containing different medicinal form and its corresponding codes
#' @param ing a \code{tibble} generated using \code{importdmd()} function containing different APIs and its corresponding codes
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
#'  
#' ## an example character vector containing different GP Practices
#' GP_practices <- c("L81069","L81070","L81071","L81072")
#' 
#' ## Define the files output location
#' setwd("C:/Datasets/practice_wise/2018")
#' 
#' ## data201812 - generated using the csv2dat() function
#' 
#' ## modify objects generated using the impordmd() function 
#' 
#' ## api mapping 
#' api_map <- dmdfile$api_map
#'
#' ## unit of measurement  
#' uom <- dmdfile$uomwdesc %>%
#'         dplyr::rename(UOM = CD)
#'
#' ## medicinal form 
#' dform <- dmdfile$dform
#'
#' ## ingredients
#' ing <- dmdfile$ing %>%
#'         dplyr::rename(API_CODE = 1)

#' 
#' ## execute practice_wise function to generate multiple files
#' practice_wise(data201812, GP_practices, api_map, uom, dform, ing)
#' 
#' }
#' 
practice_wise <- function(presdata,gpvector,apimap,uom,dform, ing) {
  
  tab01 <- NULL
  tab02 <- NULL
  tab03 <- NULL
  tab04 <- NULL
  tab05 <- NULL
  tab06 <- NULL
  s <- NULL
  
  for (i in (1:length( unique(gpvector)  )))
  {
    
    j <- unique(gpvector)[i]
    
    tab01 <- presdata %>%
      dplyr::filter(PRACTICE %in% j) %>%
      data.table::setnames(4,"BNFCODE") %>%
      merge(apimap,by = "BNFCODE", all = FALSE) %>%
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
    
    tab03 <- tab02 %>%
      dplyr::mutate_if(is.factor, as.character) %>%
      dplyr::left_join( dplyr::select(uom,c(UOM, multi_fac)) , by = "UOM") %>%
      dplyr::mutate(Vol_Mass_Tot =Vol_Mass *QUANTITY * multi_fac  ) %>%
      dplyr::left_join( dform , by = "VPID") %>%
      dplyr::rename(DForm = FORMCD)
    
    tab04  <- tab03 %>%
      dplyr::select("PERIOD", "PRACTICE","API","Vol_Mass_Tot","DForm")%>%
      dplyr::mutate(API= as.character(API))%>%
      dplyr::mutate(API_CODE = API) %>%
      dplyr::group_by(API_CODE,PRACTICE,PERIOD,DForm)%>%
      dplyr::summarize(gram = sum(Vol_Mass_Tot, na.rm = TRUE)) %>%
      dplyr::left_join(ing[,c(1,5)], by="API_CODE") %>%
      dplyr::mutate(gram = gram/1000)%>%
      dplyr::select("NM", "API_CODE","PRACTICE","PERIOD","DForm","gram")
    
    tab05 <- aggrapi(tab04[, c("NM", "gram")])
    
    tab06 <- cbind(CPD = tab05$'.id' [match(gsub(" ", "",tab04$"NM"), gsub(" ", "", tab05$'NM'))],tab04)
    
    write.csv(tab06, paste0( j,".csv"))
    
  }
}
