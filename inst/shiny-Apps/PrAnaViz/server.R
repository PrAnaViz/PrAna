source("global.R")
options(shiny.maxRequestSize = 5000000*1024^6)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  sql.demo_one <- "SELECT * FROM gp_practice." 
  sql.support <- "SELECT * FROM support." 
  
   # Connect to local host 
  aggr_wodb <- reactive({
    DBI:: dbConnect(
      drv = RMariaDB::MariaDB(),
      username = input$usernm ,
      password = isolate(input$pwd), 
      host=input$hostnm)
  }) 
  
  
  dform_desc <- reactive({
    tbl(aggr_wodb(), sql("SELECT * FROM support.dform_desc" ))
  })  
  
  gp_list_shape <- reactive({
    tbl(aggr_wodb(), sql("SELECT * FROM support.full_gp_shapewise" )) %>%
     mutate (region = toupper(region))
  })
  
  # select table bnf_group
  tbls_bnf_group <- "bnf_group"
  
  # df from support bnf_group
  df_bnf_group <- reactive({
    lapply(paste(sql.support, tbls_bnf_group), function(s) {
      tryCatch({ return(dbGetQuery(aggr_wodb(), s)) 
      }, error = function(e) return(as.character(e)))
    })
  })  
  
  # bnf group table
  bnf_group <- reactive({
    base::do.call(rbind, df_bnf_group()) %>%
      mutate(BNF_CHEMICAL_SUBSTANCE =(tolower(BNF_CHEMICAL_SUBSTANCE)))
  })  
  
  # select table dfrom_desc
  tbls_dform_desc<- "dform_desc"
  
  # df from support dfrom_desc
  df_dform_desc <-  reactive({
    lapply(paste(sql.support, tbls_dform_desc), function(s) {
      tryCatch({ return(dbGetQuery(aggr_wodb(), s)) 
      }, error = function(e) return(as.character(e)))
    })
  }) 
  
  # dfrom_desc table
  dform_desc <- reactive({
    base::do.call(rbind, df_dform_desc()) 
  }) 
  
  # select table gp data latnlong
  tbls_gp_data_full_latnlong <- "gp_data_full_latnlong"
  
  # df from support gp data latnlong
  df_gp_data_full_latnlong <- reactive({
    lapply(paste(sql.support, tbls_gp_data_full_latnlong), function(s) {
      tryCatch({ return(dbGetQuery(aggr_wodb(), s)) 
      }, error = function(e) return(as.character(e)))
    })
  }) 
  
  # postcode_full table
  gp_data_full_latnlong <- reactive({
    base::do.call(rbind, df_gp_data_full_latnlong()) %>%
      mutate(latitude = as.numeric(latitude),
             longitude = as.numeric(longitude))
  })  
  
  # For the UI to show all the regions based on year
  gp_list_year_tab  <- reactive ({
    gp_list_shape () %>%
    filter (Year %in% !!input$selectyear01) %>%
    select(region)
  })
  
  
  output$regioninput1 <- renderUI({
    withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
      selectInput(inputId="region_select_01",
                  label="Select region:",
                  choices=map()$region_name
      )
    })
  })
  
 
  
  # select table postcode_full
  tbls_ccg_gp_full <- "ccg_gp_full_latnlong"
  
  # df from support postcode_full
  df_ccg_gp_full <- reactive({
    lapply(paste(sql.support, tbls_ccg_gp_full), function(s) {
      tryCatch({ return(dbGetQuery(aggr_wodb(), s)) 
      }, error = function(e) return(as.character(e)))
    })
  }) 
  
  # postcode_full table
  ccg_gp_full <- reactive({
    base::do.call(rbind, df_ccg_gp_full())
  }) 
  
  ## To see the gp setting type
  output$settinginput1 <- renderUI({
    withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
      selectInput(inputId="setting_select_01",
                  label="Setting type:",
                  choices=c("all" , 
                            unique( ccg_gp_full()$setting_type )),
                  selected ="GP Practice",  multiple = FALSE
      )
    })
  })
  
  title_font <- list(
    size = 11,
    xref = "paper",
    xref = "paper",
    position = "top center"
  )
  
  ## Plots
  source(file.path("server", "tab-targeted.R"),  local = TRUE)$value
  source(file.path("server", "tab-non-targeted.R"),  local = TRUE)$value
  
  ## Outputs
  output$sidebarpanel <- renderUI({
      div(
        sidebarMenu(
          tags$hr(),
          menuItem("Database connect", 
                   
                   textInput("usernm", "user name:", "type here"),
                   passwordInput("pwd", "Password:"),
                   textInput("hostnm", "host name:", "type here"),
                   br(),
                   actionButton("mysql_submit", "Submit")
                   ),
          tags$hr(),
          menuItem("Targeted", tabName = "targeted"),
          menuItem("Non-Targeted", tabName = "non-targeted"),
          tags$hr(),
          uiOutput("uifile_shape"),
          selectInput('selectyear01', 'Select Prescirption Year:',
                      c(
                        '2015' = '2015',
                        '2016' = '2016',
                        '2017' = '2017',
                        '2018' = '2018'), 
                      selected = '2018'
          ),
          uiOutput("regioninput1"),
          uiOutput("settinginput1"),
          tags$hr()
        )
      )
    
  })
  
  ## body
  
  output$body <- renderUI({
          tabItems(
                    # targeted tab content
                    tabItem(
                        tabName = "targeted",
                        source (file.path("ui","tab-targeted.R"),local = TRUE)$value
                            ), #End of Tab item
                      
                    # non-targeted region tab content
                      tabItem(
                        tabName = "non-targeted",
                        source (file.path("ui","tab-non-targeted.R"),local = TRUE)$value
                        )# End of Tab item
                      
                    )
   
  })
  
  
  
  
}

