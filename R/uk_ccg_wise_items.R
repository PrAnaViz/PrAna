uk_ccg_wise_items <- function(x,y) {

  tab01 <- NULL
  tab02 <- NULL
  tab03 <- NULL
  tab04 <- NULL
  s <- NULL
  tab1 <- NULL
  tab2 <- NULL
  tab3 <- NULL

  for (i in (1:length( unique(y[[3]])  )))
  {

    j = unique(y[[3]])[i]

    tab01 <- x %>%
      filter (region %in% j) %>%
      data.table::setnames(4,"BNFCODE") %>%
      merge(bnf_full_final_02,by = "BNFCODE", all = F) %>%
      select("region","PERIOD","PRACTICE","BNFCODE","MDR","VPPID","VPID.x","ISID","value","UOM","ITEMS","QUANTITY")

    s <- strsplit(tab01$ISID, split = "#")

    tab02<- data.frame(
      region = rep(tab01$'region', sapply(s, length)),
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

    tab1  <- tab04 %>%
      dplyr::select("region","PERIOD", "PRACTICE","API","ITEMS","DForm")%>%
      mutate(API= as.character(API))%>%
      mutate(API_CODE = API) %>%
      group_by(region, API_CODE,PRACTICE,PERIOD,DForm)%>%
      dplyr::summarize(tot_items = sum(ITEMS, na.rm = T)) %>%
      left_join(ing2[,c(1,5)], by="API_CODE") %>%
      select("region","NM", "API_CODE","PRACTICE","PERIOD","DForm","tot_items")

    tab2 <- aggrapi(tab1[, c("NM", "tot_items")])

    tab3 <- cbind(CPD = tab2$'.id' [match(gsub(" ", "",tab1$"NM"), gsub(" ", "", tab2$'NM'))],tab1)

    write.csv(tab3, paste0( j,".csv"))

  }
}
