#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/

options(shiny.maxRequestSize = 50000000*1024^6)

ui <- fluidPage(


fluidRow(
        column(width = 3,
            box( 
              textInput("usernm", "user name:", "type here"),
              passwordInput("pwd", "Password:"),
              br(),
              actionButton("mysql_submit", "Submit"),
              selectInput('selectupload', 'Upload type:',
                              c(
                                'full' = 'full',
                                'part' = 'part',
                                'selected' = 'selected'
                              ), selected = 'selected'
                  ),
                  selectInput('selectsql', 'SQL:',
                              c(
                                'old' = 'old',
                                'new' = 'new'
                              ), selected = 'new'
                  ),
                 conditionalPanel("input.selectupload == 'part'",
                                    selectInput('selectdb', 'Select DB:',
                                                  c(
                                                    'CCG' = 'CCG',
                                                    'support' = 'support',
                                                    'demo_one' = 'demo_one'
                                                    ), selected = 'support'
                                                  ),
                                      uiOutput("uifile1"),
                                      textInput("text", "Table name:", "type here"),
                                      br(),
                                      actionButton("submit", "Upload"),
                                      tabBox(width = NULL
                                              
                                             )
                                    )
            ) # End of Box
        ), # End of Column

        column(9,
                tabBox(width = NULL,
                        
                       tabPanel("tab sql",
                                dataTableOutput("tab_sqldata")      
                       ),
                       tabPanel("tab from sql",
                                
                                
                                   selectInput('selectdb3', 'Select DB:',
                                          c(
                                            'support' = 'support',
                                            'demo_one' = 'demo_one'
                                            ), selected = 'support'
                                      ),
                                   uiOutput("tables_output"), 
                                   dataTableOutput("tab_datafromsql")  
                                 )
                      )
        )
                          
      ) # End of Fluid Row


)


server <- function(input,output,session){
  
  aggr_alldb <- reactive({ 
    DBI:: dbConnect(
    drv = RMariaDB::MariaDB(),
    username = input$usernm , 
    password = isolate(input$pwd), 
    dbname = input$selectdb3,
    host='localhost')
  })
  
  sqldata <-  eventReactive(input$submit, {
    saveData (mydata(),input$text,input$selectdb, input$usernm,input$pwd)
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
  
  observeEvent(input$mysql_submit , {
        output$tables_output <- renderUI({
            withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
              selectInput(inputId="select_tables",
                          label="Select CCG:",
                          choices=unique(as.data.frame(dbListTables(aggr_alldb() ))),selected =head(unique(as.data.frame(dbListTables ( aggr_alldb() ) ))),  multiple = FALSE)
            })
        })
})

  
}
shinyApp(ui = ui, server = server)