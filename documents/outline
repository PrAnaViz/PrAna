
bnf_full <- data.table::fread ("C:/Users/kjj28/OneDrive/R Directory/Presciption data/08102018/datasets/June 2018 Snomed mapping.csv", header = T, stringsAsFactors = F)

bnf_full$'BNF Code' <- gsub("'", "",bnf_full$'BNF Code' )
bnf_full$'VMPP / AMPP SNOMED Code'<-gsub("'", "",bnf_full$'VMPP / AMPP SNOMED Code' )

ampp <-readxl::read_excel("C:/dmdDataLoader/excel/f_ampp.xlsx", sheet = "AmppType")  
vmpp <-readxl::read_excel("C:/dmdDataLoader/excel/f_vmpp.xlsx", sheet = "VMPP") 
vmp <-readxl::read_excel("C:/dmdDataLoader/excel/f_vmp.xlsx", sheet = "VPI") 
dform <- readxl::read_excel("C:/dmdDataLoader/excel/f_vmp.xlsx", sheet = "DrugForm") 
ing <- readxl::read_excel("C:/dmdDataLoader/excel/f_ingredient.xlsx", sheet = "Ingredient") 
UoM_01 <- readxl::read_excel("C:/dmdDataLoader/excel/f_lookup.xlsx", sheet = "UoM") 


vmp_summ <- plyr::ddply(vmp, .(VPID), summarise, length(unique(ISID)), 
                        ISID= paste(unique(ISID), collapse = "#"),
                        value = paste(STRNT_NMRTR_VAL, collapse = "#"), 
                        UoM =  paste(STRNT_NMRTR_UOMCD, collapse = "#"))

# Match column: vmpp/ampp SNOMED code (SNOMED MAPPING file) with column : APPID (dm+d generated f_ampp file) to generate VPPID column
bnf_full01 <- cbind(bnf_full,VPPID = ampp$'VPPID' [match(gsub(" ", "",bnf_full$'VMPP / AMPP SNOMED Code' ), gsub(" ", "", ampp$'APPID'))] )

# Match vmpp/ampp SNOMED code* (SNOMED MAPPING file) with VPPID (dm+d generated f_ampp file) to update VPPID column
bnf_full01$'VPPID'[is.na(bnf_full01$'VPPID')] <- ampp$'VPPID'[match(gsub(" ", "",bnf_full01$'VMPP / AMPP SNOMED Code' [is.na(bnf_full01$'VPPID')]), gsub(" ", "",  ampp$'VPPID'))]

# Match vmpp/ampp SNOMED code* (SNOMED MAPPING file) with VPPID* (dm+d generated f_ampp file) to update VPPID column only first 6 strings
bnf_full01$'VPPID'[is.na(bnf_full01$'VPPID')] <-ampp$'VPPID' [match ( substr((bnf_full01$'VMPP / AMPP SNOMED Code' [is.na(bnf_full01$'VPPID')]),start = 1,stop = 6 ) , gsub(" ", "", substr( ampp$'VPPID', 1, 6)))]

# Fill empty VPPID column values with SNOMED values
bnf_full01$'VPPID'[is.na(bnf_full01$'VPPID')] <- bnf_full01$`VMPP / AMPP SNOMED Code`[is.na(bnf_full01$'VPPID')]

# Match VPPID (SNOMED MAPPING file) with VPPID (dm+d generated f_vmpp file) to add VPID column
bnf_full01 <- cbind( bnf_full01, VPID = vmpp$'VPID' [match(gsub(" ", "",bnf_full01$'VPPID'), gsub(" ", "", vmpp$'VPPID'))])

# Match VPPID (SNOMED MAPPING file) with VPPID (dm+d generated f_vmpp file) to update VPID column (only empty VPID values)
bnf_full01$'VPID'[is.na(bnf_full01$'VPID')] <- vmpp$'VPID'[match(gsub(" ", "",substr(bnf_full01$'VPPID' [is.na(bnf_full01$'VPID')], 1, 6)), gsub(" ", "",  substr(vmpp$'VPPID', 1, 6)))]

# Match VPPID* (SNOMED MAPPING file) with VPPID* (dm+d generated f_vmpp file) to update VPID column only first 6 strings
bnf_full01$'VPID'[is.na(bnf_full01$'VPID')] <- vmpp$'VPID'[match(gsub(" ", "",substr(bnf_full01$'VPPID' [is.na(bnf_full01$'VPID')], 1, 6)), gsub(" ", "",  substr(vmpp$'VPPID', 1, 6)))]

# Match VPID (SNOMED MAPPING file) with VPID (dm+d generated f_vmp file) to add VPID2 column
bnf_full01 <- cbind( bnf_full01, VPID2 =vmp_summ$'VPID'[match(gsub(" ", "",bnf_full01$'VPID'), gsub(" ", "",vmp_summ$'VPID'))])

# Match VPPID* (SNOMED MAPPING file) with VPPID* (dm+d generated f_vmpp file) to add VPID column - only first 6 strings
bnf_full01$'VPID2'[is.na(bnf_full01$'VPID2')] <-vmp_summ$'VPID'[ match( substr(bnf_full01$'VPID' [is.na(bnf_full01$'VPID2')], 1, 6), substr(vmp_summ$'VPID', 1, 6))]

# duplicate VPID to VPID2 column for merging purpose
vmp_summ<- cbind(vmp_summ, VPID2 = vmp_summ$'VPID')

# Merge both tab based on VPID2
bnf_full_final_02 <- merge(bnf_full01,vmp_summ,by = "VPID2")

# write it as data file
write.csv(bnf_full_final,"C:/R Directory/Shiny/PRANA/inst/shiny-Apps/pda_2/data/bnf_snomed_mapping_02.csv")

# Remove duplicates based on column BNF code
bnf_full_final_02 <- bnf_full_final[!duplicated(bnf_full_final$`BNF Code` ), ]

# Remove duplicates based on column VPID2
bnf_full_final_03 <- bnf_full_final_02[!duplicated(bnf_full_final_02$VPID2 ), ]



setwd("C:/Datasets/Prescription Datasets/datasets")

## Unit of measurement file
uom <- readr::read_rds("uom.rds")


## drug form file
# dform <- { 
#   a1 <-readr::read_rds("dform.rds")
#   a1$VPID <- format(as.numeric (as.character(a1$VPID)), scientific = FALSE)
#   a1$FORMCD <- format(as.numeric (as.character(a1$FORMCD)), scientific = FALSE)
#   a1
# }


# ## ingredients
# ing <-{ 
#   a1 <- readr::read_rds("ing.rds")
#   a1$'ISID' <- format(as.numeric (as.character(a1$'ISID')), scientific = FALSE)
#   a1$'ISID' <-gsub(" ", "", a1$'ISID')
#   a1
# }

memory.limit(size = 750000)

setwd("C:/Datasets/Prescription Datasets/2018/GPs")
Keynsham_finalgplist <- data.table::fread ("C:/Datasets/Prescription Datasets/2018/GPs/Keynsham_GP_List_2018.csv", header = T, stringsAsFactors = F)
keynsham_gp2018 <-c ( Keynsham_finalgplist$PRACTICE)


setwd("C:/Datasets/Prescription Datasets/2018/PDPI")
data201812 <- readr::read_rds("Full_2018_12.rds")

str(data201812)

data201812_02 <- subset(data201812, PRACTICE %in% keynsham_gp2018)


unique(data201812_02$PRACTICE)

library(readr)
colnames(bnf_full_final)[2]<- "BNFCODE"
colnames(data201812_02)[4] <- c("BNFCODE")

str(data201812)

## Merge prescription and SNOMED files
#

colnames(bnf_full_final)[2] <- c("BNFCODE")

merge_01 <- {
  a1 <- Keynsham_prescription_201812
  colnames(a1)[4] <- c("BNFCODE")
  a2 <- bnf_full_final
  a2$ISID <- sapply(a2$ISID, as.character)
  a3 <- merge(a2[!duplicated(a2$BNFCODE),],a1,by = "BNFCODE")
  a4 <- a3[, c("PERIOD","PRACTICE","BNFCODE","MDR: Product Description","VPPID","VPID.x","VPID.y","ISID","value","UoM","ITEMS","QUANTITY")]
  a4

}


#merge_01 <- merge(bnf_full_final_04,data201812_02,by = c("BNFCODE"), allow.cartesian = T)



## Merge everything and calculate the ingredients for each prescription
str(merge_03)
merge_03 <- {
  a1 <- merge_01
  a1$ISID <- sapply(a1$ISID, as.character)
  a1$value <- sapply(a1$value, as.character) 
  a1$UoM <- sapply(a1$UoM, as.character)
  s <- strsplit(a1$ISID, split = "#")
  v <- strsplit(a1$value, split = "#")
  u <- strsplit(a1$UoM, split = "#")
  a2 <- data.frame(PERIOD = rep(a1$'PERIOD', sapply(s, length)),
                   PRACTICE = rep(a1$'PRACTICE', sapply(s, length)),
                   BNF_CODE = rep(a1$'BNFCODE', sapply(s, length)),
                   #VPPID = rep(a1$'VPPID', sapply(s, length)),
                   VPID = rep(a1$'VPID.x', sapply(s, length)),
                   MDR = rep(a1$'MDR: Product Description', sapply(s, length)),
                   ITEMS = rep(a1$'ITEMS', sapply(s, length)),
                   QUANTITY = rep(a1$'QUANTITY', sapply(s, length)),
                   API = unlist(s),
                   Vol_Mass =  sapply(unlist(v), as.numeric),
                   UoM = unlist(u))
  a2$API <- sapply(a2$API,as.character)
  a2$UoM = sapply(a2$UoM, as.character)
  a3 <- cbind(a2,Vol_Mass_Tot = (a2$'Vol_Mass' *a2$'QUANTITY' )* uom$multi_fac[match(gsub(" ", "",a2$'UoM'), gsub(" ", "",  uom$'CD'))])
  a4 <- cbind(a3, DForm = dform$'FORMCD'[match(gsub(" ", "",a3$'VPID'), gsub(" ", "",  dform$'VPID'))]
              # qt_val = vmpp$'QTVAL' [match(gsub(" ", "",a3$'VPPID'), gsub(" ", "",  vmpp$'VPPID' ))],
              # qt_uom = vmpp$'QTY_UOMCD' [match(gsub(" ", "",a3$'VPPID'), gsub(" ", "",  vmpp$'VPPID'))]
              )
  a4
}

str(bnf_full05_02)
## Calculate gram of each API
merge_04 <- {
  a10 <- merge_03
  #a10$API <- sapply(a10$API,as.character)
  a1 <- merge_03[, c("PERIOD", "PRACTICE","API","Vol_Mass_Tot","DForm")]
  a2 <- aggregate(a1$Vol_Mass_Tot, by=list(API_CODE=gsub(" ", "",a1$'API'),PRACTICE = a1$'PRACTICE',PERIOD = a1$'PERIOD',DFORM = a1$"DForm"), FUN=sum)
  a3 <- cbind(API = ing$'NM' [match(gsub(" ", "",a2$'API_CODE'), gsub(" ", "", ing$'ISID'))],a2,gram = ((a2$x)/1000))
  a3
}



library(stringr)
library(plyr)

merge_05 <-{
  a1 <- fullapi(merge_04[, c("API", "gram")])
  a2 <- cbind(CPD = a1$'.id' [match(gsub(" ", "",merge_04$"API"), gsub(" ", "", a1$'API'))],merge_04)
  a2
}

## Compare with old bnf_full_final

bnf_full_final_old <- data.table::fread ("C:/Users/kjj28/OneDrive/R Directory/Presciption data/07012019/datasets/bnf_full_final.csv", header = T, stringsAsFactors = F)

# Remove duplicates based on column BNF code
bnf_full_final_old_02 <- bnf_full_final_old[!duplicated(bnf_full_final_old$BNFCODE ), ]

# Remove duplicates based on column VPID2
bnf_full_final_old_03 <- bnf_full_final_old_02[!duplicated(bnf_full_final_old_02$VPID2 ), ]


str (bnf_full_final)
str(bnf_full_final_old)
bnf_full_final_old$VPID2[1]


bnf_full_final_old_04 <- data.table::data.table(bnf_full_final_old, key = c("BNFCODE"))
merge_old_01 <- merge(bnf_full_final_old_04,data201812_02,by = "BNFCODE", allow.cartesian = T)
str(merge_old_02)


merge_old_02 <- merge_old_01[, c("PERIOD","PRACTICE","BNFCODE","MDR","VPPID","VPID","ISID","value","UOM","ITEMS","QUANTITY")]

## Merge everything and calculate the ingredients for each prescription
str(merge_old_04)
merge_old_04 <- {
  a1 <- merge_old_02
  a1$ISID <- sapply(a1$ISID, as.character)
  a1$value <- sapply(a1$value, as.character) 
  a1$UoM <- sapply(a1$UoM, as.character)
  s <- strsplit(a1$ISID, split = "#")
  v <- strsplit(a1$value, split = "#")
  u <- strsplit(a1$UOM, split = "#")
  a2 <- data.frame(PERIOD = rep(a1$'PERIOD', sapply(s, length)),
                   PRACTICE = rep(a1$'PRACTICE', sapply(s, length)),
                   BNF_CODE = rep(a1$'BNFCODE', sapply(s, length)),
                   VPID = rep(a1$'VPID', sapply(s, length)),
                   MDR = rep(a1$'MDR', sapply(s, length)),
                   ITEMS = rep(a1$'ITEMS', sapply(s, length)),
                   QUANTITY = rep(a1$'QUANTITY', sapply(s, length)),
                   API = unlist(s),
                   Vol_Mass =  sapply(unlist(v), as.numeric),
                   UoM = unlist(u))
  a2$API <- sapply(a2$API,as.character)
  a2$UoM = sapply(a2$UoM, as.character)
  
  #a3 <- cbind(a2, ITEMS_TOT = (a2$'ITEMS' *a2$'QUANTITY' )) 
  a4 <- cbind(a2,Vol_Mass_Tot = (a2$'Vol_Mass' *a2$'QUANTITY' )* uom$multi_fac[match(gsub(" ", "",a2$'UoM'), gsub(" ", "",  uom$'CD'))])
  a5 <- cbind(a4, DForm = dform$'FORMCD'[match(gsub(" ", "",a4$'VPID'), gsub(" ", "",  dform$'VPID'))],
              qtval = vmpp$QTVAL [match(gsub(" ", "",a4$'VPID'), gsub(" ", "",  vmpp$VPID ))],
              qtuom = dform$'FORMCD'[match(gsub(" ", "",a4$'VPID'), gsub(" ", "",  dform$'VPID'))])
  
  a5
}


## Calculate gram of each API
merge_old_05 <- {
  a10 <- merge_old_04
  a10$API <- sapply(a10$API,as.character)
  a1 <- a10[, c("PERIOD", "PRACTICE","API","Vol_Mass_Tot","DForm")]
  a2 <- aggregate(a1$Vol_Mass_Tot, by=list(API_CODE=gsub(" ", "",a1$'API'),PRACTICE = a1$'PRACTICE',PERIOD = a1$'PERIOD',DFORM = a1$"DForm"), FUN=sum)
  a3 <- cbind(API = ing$'NM' [match(gsub(" ", "",a2$'API_CODE'), gsub(" ", "", ing$'ISID'))],a2,gram = ((a2$x)/1000))
  a3
}

library(stringr)
library(plyr)

merge_old_06 <-{
  a1 <- fullapi(merge_old_05[, c("API", "gram")])
  a2 <- cbind(CPD = a1$'.id' [match(gsub(" ", "",merge_old_05$"API"), gsub(" ", "", a1$'API'))],merge_old_05)
  a2
}
