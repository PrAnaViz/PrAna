#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    aggr_alldb <- reactive({ DBI:: dbConnect(
        drv = RMariaDB::MariaDB(),
        username ='kjsql', 
        password = 'nEd01@fd7TZl', 
        dbname = input$selectdb3,
        host='51.132.130.246')
    })
    
    sqldata <-  eventReactive(input$submit, {
        saveData (mydata(),input$text,input$selectdb)
    })
    
    dir_path <- reactive({
        unique(dirname(input$file1$datapath))
    })
    
    list_files <- reactive({
        list.files(dir_path(), pattern="*.csv", full.names=TRUE)
    })
    
    mydata <- reactive({
        rbindlist(lapply(list_files(),fread))
    })
    
    datafromsql <- reactive({
        dbReadTable(aggr_alldb(), input$select_tables) %>%
        distinct()
    })
    
    output$tab_datafromsql <- renderDataTable({ 
        withProgress(message = 'Data is loading, please wait ...', value = 1:10, {
            as.data.frame(datafromsql())
        })
    })
    
    output$tab_sqldata<- renderDataTable({ 
        withProgress(message = 'Data is loading, please wait ...', value = 1:10, {
            sqldata()
        })
    })
    
    output$uifile1 <- renderUI({
        fileInput('file1', 'Upload your file(s):',
                  accept = c(
                      'text/csv',
                      'text/comma-separated-values',
                      'text/tab-separated-values',
                      'text/plain',
                      '.csv',
                      '.tsv'
                  ),
                  multiple = TRUE)
    })
    
    
    ## To see the tables in DB
    output$tables_output <- renderUI({
        withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
            selectInput(inputId="select_tables",
                        label="Select CCG:",
                        choices=unique(as.data.frame(dbListTables(aggr_alldb() ))),selected =head(unique(as.data.frame(dbListTables ( aggr_alldb() ) ))),  multiple = FALSE)
        })
    })
    
    

})
