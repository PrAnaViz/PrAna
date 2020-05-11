#' saveData
#'
#' Upload csv to MySQL DB
#'  
#' @param data csv data
#' @param tab_name Table name in the tbe DB
#' @param selectdb enter the interested db
#' @param usernm input your MySQL username
#' @param pwd input your MySQL password
#'
#' @return a table in MySQL
#' @export
#'
saveData <-function (data,tab_name,selectdb, usernm,pwd){
  
    con <-  DBI::dbConnect (
          drv = RMariaDB::MariaDB(),
          username = usernm, 
          password = pwd,
          host='localhost',
          dbname =  selectdb 
    )
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con,tab_name,data,row.names= FALSE,  append=T)
  DBI::dbDisconnect(con)
}  
