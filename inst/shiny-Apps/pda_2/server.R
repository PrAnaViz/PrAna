#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

options(shiny.maxRequestSize = 5000000*1024^6)

## Login details
login_details <- data.frame(user = c("admin","fcte20","kp300","dgk30"),
                            pswd = c("AdmInPswD","chloramphenicol1","Ckhepoler.20","r3sist-r3sistanc3"))
login <- box(
  title = "Login",
  textInput("userName", "Username"),
  passwordInput("passwd", "Password"),
  br(),
  actionButton("Login", "Log in")
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  #To logout back to login page
  login.page = paste(
    isolate(session$clientData$url_protocol),
    "//",
    isolate(session$clientData$url_hostname),
    ":",
    isolate(session$clientData$url_port),
    sep = ""
  )
  
  # For login values
  USER <- reactiveValues(Logged = F)
  observe({
    if (USER$Logged == FALSE) {
      if (!is.null(input$Login)) {
        if (input$Login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          Id.username <- which(login_details$user %in% Username)
          Id.password <- which(login_details$pswd %in% Password)
          if (length(Id.username) > 0 & length(Id.password) > 0){
            if (Id.username == Id.password) {
              USER$Logged <- TRUE
            }
          }
        }
      }
    }
  })
  
  
  # output file
  #BNF SNOMED Mapping file
  snomedmap <- reactive({ 
    importdmd("C:/dmdDataLoader/excel/")
  })
  
  # Connect the SQL db
  aggr01_2014_2018 <- DBI::dbConnect(RSQLite::SQLite(), "data/aggr01_2014_2018.sqlite")
  
  # create table from the db
    Five_catchment_2014_2018 <- tbl(aggr01_2014_2018, "Five_catchment_2014_2018")
  
  # Subset data
  data_subset_01 <- Five_catchment_2014_2018 %>%
    filter(site %in% c('A','B'))
  
  # Subset data
  data_subset_02 <- reactive ({ 
    tbl(aggr01_2014_2018, "Five_catchment_2014_2018") %>%
    filter(site %in% input$searchoption2) %>% 
    group_by(site, CPD, API, PRACTICE, PERIOD) %>%
    summarize(sum = sum(gram, na.rm = T))%>%
    filter(tolower(CPD) %in% targetdata()$V1)
    })
  
  
  
  # Subset monthwise data
  data_monthwise <- reactive ({ 
    tbl(aggr01_2014_2018, "Five_catchment_2014_2018") %>%
      filter(site %in% input$searchoption2) %>% 
      group_by(site, CPD, PRACTICE, PERIOD) %>%
      summarize(sum = sum(gram, na.rm = T)) %>%
      filter(tolower(CPD) %in% targetdata()$V1) %>%
      mutate(Year=(substr(PERIOD, 1, 4))) %>%
      mutate(Month=(substr(PERIOD, 5, 6))) %>%
      group_by(site, CPD, Year, Month) %>%
      summarize(gram = sum(sum, na.rm = T)) %>%
      mutate(kg = gram/1000)
  })
  
  # Subset yearwise data with GP code
  data_yearwise_with_gp <- reactive ({ 
    tbl(aggr01_2014_2018, "Five_catchment_2014_2018") %>%
      filter(site %in% input$searchoption2) %>% 
      group_by(site, CPD, PRACTICE, PERIOD) %>%
      summarize(sum = sum(gram, na.rm = T)) %>%
      filter(tolower(CPD) %in% targetdata()$V1) %>%
      mutate(Year=(substr(PERIOD, 1, 4))) %>%
      group_by(site, CPD, Year, PRACTICE) %>%
      summarize(gram = sum(sum, na.rm = T)) %>%
      mutate(kg = gram/1000)
  })
  
  
  # Subset monthwise data with GP code
  data_monthwise_with_gp  <- reactive ({ 
    tbl(aggr01_2014_2018, "Five_catchment_2014_2018") %>%
      filter(site %in% input$searchoption2) %>% 
      group_by(site, CPD, PRACTICE, PERIOD) %>%
      summarize(sum = sum(gram, na.rm = T)) %>%
      filter(tolower(CPD) %in% targetdata()$V1) %>%
      mutate(Year=(substr(PERIOD, 1, 4))) %>%
      mutate(Month=(substr(PERIOD, 5, 6))) %>%
      group_by(site, CPD, Year, Month, PRACTICE) %>%
      summarize(gram = sum(sum, na.rm = T)) %>%
      mutate(kg = gram/1000)
  })
  
  ## For drug targets
  targetdata <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    fread(inFile$datapath, header = FALSE,
          sep = ",", quote = '"')
  })
  
  ### Outputs
  output$txt_timeseries01 <- renderText({
    str(tbl(aggr01_2014_2018, "Five_catchment_2014_2018"))
  })
  
  output$sidebarpanel <- renderUI({
    if (USER$Logged == TRUE) {
      div(
        sidebarUserPanel(
          isolate(input$userName),
          subtitle = a(icon("usr"), "Logout", href = login.page)
        ),
        sidebarMenu(
          tags$hr(),
          uiOutput("uifile1"),
          uiOutput("uifile2"),
          bsTooltip ('uifile2',"Click here to upload your data","bottom", options = NULL),
          tags$hr(),
          menuItem( "Raw_data", tabName = "rawdatasets", icon = icon("table")),
          menuItem( "SNOMED", tabName = "snomed", icon = icon("table")),
          menuItem( "Time Series", tabName = "timeseries", icon = icon("table"))
          
          
        )
      )
    }
  })
  
  ## body
  
  output$body <- renderUI({
    if (USER$Logged == TRUE) {
      tabItems(
        # First tab content
        tabItem(
        tabName = "snomed",
        fluidRow(
          box(width = 2,height = 1500
              
              
          ),# End of Box
          
          
          tabBox(width = 10,height = 1500,
                 tabPanel("DataTable01",
                          #downloadButton ('downdat.data_timeseries04'),
                          dataTableOutput("tab_snomedmap" ,height="450px"))
                 
          ) # End of tabBox     
        ) # End of fluidRow
      ),
      # Third tab content
      tabItem(
        tabName = "timeseries",
        fluidRow(
          box(width = 2,height = 1500,
              selectInput('searchoption2', 'Select site:',
                          c('A' = 'A',
                            'B' = 'B',
                            'C' = 'C',
                            'D' = 'D',
                            'E' = 'E'
                          ), selected = 'A'
              ),
              
              tags$hr()
              
          ),# End of Box
          
          
          tabBox(width = 10,height = 1500,
                 # include the UI for each tab
                 source(file.path("ui", "tab-timeseries.R"),  local = TRUE)$value
                 
                 
          ) # End of tabBox     
        ) # End of fluidRow
      ),
      
      
      # Second tab content
      tabItem(
        tabName = "rawdatasets",
        fluidRow(
          box(width = 2,height = 1500,
              selectInput('searchoption3', 'Select site:',
                          c('A' = 'A',
                            'B' = 'B',
                            'C' = 'C',
                            'D' = 'D',
                            'E' = 'E'
                          ), selected = 'A'
              ),
              
              tags$hr()
              
          ),# End of Box
          
          
          tabBox(width = 10,height = 1500,
                 
                 tabPanel("Data SQL",
                          downloadButton ('downdat.data_sql'),
                          dataTableOutput("tab_data_sql" ,height="450px")),
                 tabPanel("Subset_01",
                          #downloadButton ('downdat.data_timeseries04'),
                          dataTableOutput("tab_data_subset_01" ,height="450px")),
                 tabPanel("Subset_02",
                          #downloadButton ('downdat.data_timeseries04'),
                          dataTableOutput("tab_data_subset_02" ,height="450px")),
                 tabPanel("Data Yearwise",
                          #downloadButton ('downdat.data_timeseries04'),
                          verbatimTextOutput("txt_timeseries01"),
                          dataTableOutput("tab_data_yearwise" ,height="450px")),
                 tabPanel("Data Monthwise",
                          dataTableOutput("tab_data_monthwise" ,height="450px")),
                 tabPanel("Data Monthwise - with GP",
                          dataTableOutput("tab_data_monthwise_with_gp" ,height="450px")),
                 tabPanel("Data Yearwise - with GP",
                          dataTableOutput("tab_data_yearwise_with_gp" ,height="450px")),
                 tabPanel("Targets",
                          #downloadButton ('downdat.data_timeseries04'),
                          dataTableOutput("tab_targetdata" ,height="450px"))
                 
                 
          ) # End of tabBox     
        ) # End of fluidRow
      )
         
        
      )}
    else {
      login
    }
  })
  
  ## Plots
  
  source(file.path("server", "tab-timeseries.R"),  local = TRUE)$value
  
  ## Data tables
  
  output$tab_snomedmap <- renderDataTable(
    withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
    snomedmap()
    }), options = list(scrollX = TRUE) )
  
  output$tab_data_sql <- renderDataTable(
    withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
      as.data.frame( Five_catchment_2014_2018)
    }), options = list(scrollX = TRUE) )
  
  output$tab_data_subset_01 <- renderDataTable(
    withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
      as.data.frame( data_subset_01)
    }), options = list(scrollX = TRUE) )
  
  output$tab_data_subset_02 <- renderDataTable(
    withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
      as.data.frame( data_subset_02())
    }), options = list(scrollX = TRUE) )
  
  output$tab_data_yearwise <- renderDataTable(
    withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
      as.data.frame( data_yearwise())
    }), options = list(scrollX = TRUE) )
  
  output$tab_data_monthwise <- renderDataTable(
    withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
      as.data.frame( data_monthwise())
    }), options = list(scrollX = TRUE) )
  
  output$tab_data_monthwise_with_gp <- renderDataTable(
    withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
      as.data.frame( data_monthwise_with_gp())
    }), options = list(scrollX = TRUE) )
  
  output$tab_data_yearwise_with_gp <- renderDataTable(
    withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
      as.data.frame( data_yearwise_with_gp())
    }), options = list(scrollX = TRUE) )
  
  
  output$tab_targetdata <- renderDataTable(
    withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
      as.data.frame( targetdata())
    }), options = list(scrollX = TRUE) )
  
  ## UI File inputs
  
  output$uifile1 <- renderUI({
    tipify(fileInput('file1', 'Upload your Targets:',
                     accept = c(
                       'text/csv',
                       'text/comma-separated-values',
                       'text/tab-separated-values',
                       'text/plain',
                       '.csv',
                       '.tsv'
                     )),
           "Click here to upload your data")
  })
  
  output$uifile2 <- renderUI({
    tipify(fileInput('file2', 'Upload your Group:',
                     accept = c(
                       'text/csv',
                       'text/comma-separated-values',
                       'text/tab-separated-values',
                       'text/plain',
                       '.csv',
                       '.tsv'
                     )),
           "Click here to upload your data")
  })
  
  
  ## Download buttons
  output$downdat.data_sql = downloadHandler(
    filename = function (){ paste('data','.csv',sep = '')},
    content = function(file) {
      write.csv(as.data.frame( Five_catchment_2014_2018), file, row.names = TRUE)
    }
  )
 
}
