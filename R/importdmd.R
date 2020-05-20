#' importdmd 
#' 
#' Import dm+d files and link it to the BNF code using SNOMED mapping file
#'
#' @param path dm+d excel file(s) location.
#'
#' @return A list with the following elements:
#' \itemize{
#'   \item api_map - a \code{data.frame} containing each BNF code mapped to individual API, its medicinal strength and medicinal form
#'   \item ing - a \code{tibble} containing all the API and its code
#'   \item uom - a \code{tibble} containing different unit of measurement and its code
#'   \item uomwdesc - a \code{data.frame} containing different unit of measurement and its code with description
#'   \item dform - a \code{tibble} containing different medicinal form and its code
#' }
#'  
#' @export
#'
#'
#' @importFrom utils memory.limit
#' @importFrom plyr ddply
#' @importFrom dplyr %>%
#' 
#' @import reshape2
#'
#' @examples 
#' \dontrun{
#' importdmd("C:/dmdDataLoader/excel/")
#' }
#' 

importdmd <- function(path)
{
  memory.limit(size = 750000)
  
  # import SNOMED Mapping (June 2018) file
  bnf_full <- data.table::fread (system.file("extdata","snomed_mapping.csv", package = "PRANA"), header = T, stringsAsFactors = F)
  bnf_full$'BNF Code' <- gsub("'", "",bnf_full$'BNF Code' )
  bnf_full$'VMPP / AMPP SNOMED Code'<-gsub("'", "",bnf_full$'VMPP / AMPP SNOMED Code' )
  
  setwd(path)
  
  # # Create all files from the dm+d folder
  ampp <-readxl::read_excel("f_ampp.xlsx", sheet = "AmppType")
  vmpp <-readxl::read_excel("f_vmpp.xlsx", sheet = "VMPP")
  vmp <-readxl::read_excel("f_vmp.xlsx", sheet = "VPI")
  dform <- readxl::read_excel("f_vmp.xlsx", sheet = "DrugForm")
  ing <- readxl::read_excel("f_ingredient.xlsx", sheet = "Ingredient")
  UoM_01 <- readxl::read_excel("f_lookup.xlsx", sheet = "UoM")
  uom_desc <- data.table::fread (system.file("extdata","uom_desc.csv", package = "PRANA"), header = T, stringsAsFactors = F)
  UoM_02 <- cbind(UoM_01, multi_fac=uom_desc$multi_fac  [match(gsub(" ", "",UoM_01$CD), gsub(" ", "",  uom_desc$'CD'))])
  
  
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
  
  # modify colnames
  colnames(bnf_full_final_02) <- c("VPID2", "BNFCODE", "MDR", "SNOMED_Code",
                                "DMD: Product Description at presentation level",
                                "DMD: Product Description at presentation and pack level",                              "VPPID",
                                "VPID.x", "VPID.y", "length", "ISID", "value", "UOM")
  
  bnf_full_final_02 <- bnf_full_final_02 %>%
                       dplyr::mutate(ISID= as.character(ISID))%>%
                       dplyr::distinct(BNFCODE,.keep_all= TRUE)
  
  newlist <- list(api_map = bnf_full_final_02, dform = dform, uom = UoM_01, uomwdesc = UoM_02, ing= ing )
  return(newlist)
}

