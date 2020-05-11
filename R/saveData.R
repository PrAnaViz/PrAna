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
  con <-  DBI::dbConnect (
    drv = RMariaDB::MariaDB(),
    username ='kjsql', 
    password = 'nEd01@fd7TZl',
    dbname =  selectdb ,
    host='localhost'
    )
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con,tab_name,data,row.names= FALSE,  append=T)
  DBI::dbDisconnect(con)
}  
