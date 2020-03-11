#' saveData
#'
#' Upload csv to MySQL DB
#'  
#' @param data csv data
#' @param tab_name Table name in the tbe DB
#'
#' @return
#' @export
#'
saveData <-function (data,tab_name,selectdb){
  con <- dbConnect (
    drv = RMariaDB::MariaDB(),
    username ='kjsql', 
    password = 'nEd01@fd7TZl',
    dbname =selectdb ,
    host='51.132.130.246')
  on.exit(dbDisconnect(con), add = TRUE)
  dbWriteTable(con,tab_name,data,row.names= FALSE,  append=T)
  dbDisconnect(con)
}  
