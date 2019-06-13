#' importdmd - import dm+d files, merge with June 2018 snomed mapping file and convert to dataframe
#'
#' @param path dm+d excel file(s) location. Example: importdmd("C:/dmdDataLoader/excel/")
#'
#' @return dataframe 
#' @export
#'

importdmd <- function(path)
{
  # Need following library plyr, data.table, readr
  memory.limit(size = 750000)
  
  # import June 2018 SNOMED Mapping file
   bnf_full <- data.table::fread (file.path("data/", "June 2018 Snomed mapping.csv"), header = T, stringsAsFactors = F)
   bnf_full$'BNF Code' <- gsub("'", "",bnf_full$'BNF Code' )
   bnf_full$'VMPP / AMPP SNOMED Code'<-gsub("'", "",bnf_full$'VMPP / AMPP SNOMED Code' )
  
  setwd(path) # C:/dmdDataLoader/excel/
  
  # # Create all files from the dm+d folder
  ampp <-readxl::read_excel("f_ampp.xlsx", sheet = "AmppType")
  vmpp <-readxl::read_excel("f_vmpp.xlsx", sheet = "VMPP")
  vmp <-readxl::read_excel("f_vmp.xlsx", sheet = "VPI")
  dform <- readxl::read_excel("f_vmp.xlsx", sheet = "DrugForm")
  ing <- readxl::read_excel("f_ingredient.xlsx", sheet = "Ingredient")
  UoM_01 <- readxl::read_excel("f_lookup.xlsx", sheet = "UoM")

  vmp_summ <- plyr::ddply(vmp, .(VPID), summarise, length(unique(ISID)),
                          ISID= paste(unique(ISID), collapse = "#"),
                          value = paste(STRNT_NMRTR_VAL, collapse = "#"),
                          UoM =  paste(STRNT_NMRTR_UOMCD, collapse = "#"))
  
  # Match column: vmpp/ampp SNOMED code (SNOMED MAPPING file) with column : APPID (dm+d generated f_ampp file) to generate VPPID column
  bnf_full01 <- cbind(bnf_full,VPPID = ampp$'VPPID' [match(gsub(" ", "",bnf_full$'VMPP / AMPP SNOMED Code' ), gsub(" ", "", ampp$'APPID'))] )
  
  
  bnf_full01
  
  
}
