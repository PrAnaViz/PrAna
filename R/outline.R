
bnf_full <- data.table::fread ("C:/Users/kjj28/OneDrive/R Directory/Presciption data/08102018/datasets/June 2018 Snomed mapping.csv", header = T, stringsAsFactors = F)

bnf_full$'BNF Code' <- gsub("'", "",bnf_full$'BNF Code' )
bnf_full$'VMPP / AMPP SNOMED Code'<-gsub("'", "",bnf_full$'VMPP / AMPP SNOMED Code' )

ampp <-readxl::read_excel("C:/dmdDataLoader/excel/f_ampp.xlsx", sheet = "AmppType")  
vmpp <-readxl::read_excel("C:/dmdDataLoader/excel/f_vmpp.xlsx", sheet = "VMPP") 
vmp <-readxl::read_excel("C:/dmdDataLoader/excel/f_vmp.xlsx", sheet = "VPI") 


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
bnf_full_final <- merge(bnf_full01,vmp_summ,by = "VPID2")

# write it as data file
write.csv(bnf_full_final,"C:/R Directory/Shiny/PRANA/inst/shiny-Apps/pda_2/data/bnf_snomed_mapping.csv")
