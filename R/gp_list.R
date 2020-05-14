## Find all the practice codes with respect to the CCG


#path1 = "C:/Datasets/Prescription Datasets/CCG/2015"
#path2 = "C:/Datasets/Prescription Datasets/NHS/2015"


#library(rowr)

#' gp_list
#'
#' @param path Path to the directory with all .csv files
#'
#' @return
#' @export
#'
#'
gp_list <- function(path) {
    files = list.files(path = path,pattern = ".csv" )
    tab1 = NULL
    tab2 = NULL

    for (i in 1:length(files) ){
          tab1[[ paste0(stringr::str_remove(files[i], ".csv")) ]] <- unique(as.data.frame( data.table::fread(file.path(path,files[i]),header = T))$PRACTICE)
          tab1
    }
    tab2 <- plyr::ldply(tab1,data.frame)
    #tab2 <- do.call("cbind.fill",tab1)
    tab2
}


# CCG_wise <- gp_list(path1)
# shape_wise <- gp_list(path2)
#
# colnames(CCG_wise)[1] <- "CCG"
# colnames(CCG_wise)[2] <- "PRACTICE"
#
# colnames(shape_wise)[1] <- "CCG"
# colnames(shape_wise)[2] <- "PRACTICE"
#
#
#
# ccg_tab <- reshape2::dcast (CCG_wise, .~CCG, value.var  = "PRACTICE")
#
# ccg_tab3 <- reshape2::dcast (CCG_wise, CCG~., value.var  = "PRACTICE")
#
# shapewise_tab <- reshape2::dcast (shape_wise, .~CCG, value.var  = "PRACTICE")
#
# shapewise_tab3 <- reshape2::dcast (shape_wise, CCG~., value.var  = "PRACTICE")
#
# shapewise_tab2 <- tidyr::spread (shape_wise,PRACTICE )
#
# gp_list_02 <- function(path) {
#   files = list.files(path = path,pattern = ".csv" )
#   tab1 = NULL
#   tab2 = NULL
#
#   for (i in 1:length(files) ){
#     tab1[[ paste0(stringr::str_remove(files[i], ".csv")) ]] <- unique(as.data.frame( data.table::fread(file.path(path,files[i]),header = T))$PRACTICE)
#     tab1
#   }
#   #tab2 <- plyr::ldply(tab1,data.frame)
#   tab2 <- do.call("cbind.fill",c(tab1, fill= NA))
#   tab2
# }
#
# shape_wise02 <- gp_list_02(path2)
# ccg_wise02 <- gp_list_02(path1)
#
# compare <- merge(shapewise_tab3, ccg_tab3, by = "CCG")
#
#
# nhs_ccg_names <- as.data.frame(stringr::str_remove(list.files(path = path1,pattern = ".csv" ), ".csv"))
# shape_ccg_names <- as.data.frame(stringr::str_remove(list.files(path = path2,pattern = ".csv" ), ".csv"))
#
#
#
# colnames(ccg_wise02)
#
# for (i in 1:nrow (shape_ccg_names))
# {
#   colnames(shape_wise02)[i] <- paste(shape_ccg_names[i,1])
# }
# for (i in 1:nrow (nhs_ccg_names))
# {
#   colnames(ccg_wise02)[i] <- paste(nhs_ccg_names[i,1])
# }
#
# getwd()
# setwd("C:/Datasets/Prescription Datasets/GPs")
#
# write.csv(shape_wise02, "shapewise_gp_2015.csv")
# write.csv(ccg_wise02, "CCGwise_gp_2015.csv")
# write.csv(compare, "compare_gp_2015.csv")
#
#
#
#
# ## For 2016
#
# path_2016_01 = "C:/Datasets/Prescription Datasets/CCG/2016"
# path_2016_02 = "C:/Datasets/Prescription Datasets/NHS/2016"
#
# shape_wise_2016 <- gp_list_02(path_2016_02)
# ccg_wise_2016 <- gp_list_02(path_2016_01)
#
#
# write.csv(shape_wise_2016, "shapewise_gp_2016.csv")
# write.csv(ccg_wise_2016, "CCGwise_gp_2016.csv")

## For 2017



## For 2018
