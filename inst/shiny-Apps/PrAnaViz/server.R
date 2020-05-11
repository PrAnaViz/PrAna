#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
# #

source("global.R")
options(shiny.maxRequestSize = 5000000*1024^6)

## Login details
login_details <- data.frame(user = c("admin","fcte20","kp300","dgk30","bkh20","nns31", "rjg20"),
                            pswd = c("AdmInPswD","chloramphenicol1","Ckhepoler.20","r3sist-r3sistanc3","EcPhRg","amrQpcr","PswDaDmin"))
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
  
  sql.demo_one <- "SELECT * FROM demo_one." 
  sql.support <- "SELECT * FROM support." 
  
  # output file
  
  # Connect to local host 
  aggr_wodb <- DBI:: dbConnect(
    drv = RMariaDB::MariaDB(),
    username ='kjsql', 
    password = 'nEd01@fd7TZl', 
    host='localhost')
  
  dform_desc <-  tbl(aggr_wodb, sql("SELECT * FROM support.dform_desc" ))
  
  gp_list_ccg_c <- tbl(aggr_wodb, sql("SELECT * FROM support.full_gp_ccg_commissioner" ))
  
  gp_list_shape <- tbl(aggr_wodb, sql("SELECT * FROM support.full_gp_shapewise" )) %>%
    mutate (region = toupper(region))
  
  # select table bnf_group
  tbls_bnf_group <- "bnf_group"
  
  # df from support bnf_group
  df_bnf_group <-  lapply(paste(sql.support, tbls_bnf_group), function(s) {
    tryCatch({ return(dbGetQuery(aggr_wodb, s)) 
    }, error = function(e) return(as.character(e)))
  })
  
  # bnf group table
  bnf_group <-   base::do.call(rbind, df_bnf_group) %>%
    mutate(BNF_CHEMICAL_SUBSTANCE =(tolower(BNF_CHEMICAL_SUBSTANCE)))
  
  # select table dfrom_desc
  tbls_dform_desc<- "dform_desc"
  
  # df from support dfrom_desc
  df_dform_desc <-  lapply(paste(sql.support, tbls_dform_desc), function(s) {
    tryCatch({ return(dbGetQuery(aggr_wodb, s)) 
    }, error = function(e) return(as.character(e)))
  })
  
  # dfrom_desc table
  dform_desc <-   base::do.call(rbind, df_dform_desc) 
  
  # select table gp data latnlong
  tbls_gp_data_full_latnlong <- "gp_data_full_latnlong"
  
  # df from support gp data latnlong
  df_gp_data_full_latnlong <-  lapply(paste(sql.support, tbls_gp_data_full_latnlong), function(s) {
    tryCatch({ return(dbGetQuery(aggr_wodb, s)) 
    }, error = function(e) return(as.character(e)))
  })
  
  # postcode_full table
  gp_data_full_latnlong <-   base::do.call(rbind, df_gp_data_full_latnlong)
  
  # For the UI to show all the regions based on year
  gp_list_year_tab  <- reactive ({ 
    if (input$selectdb_01 == "ccg_wise_commissioner") {
      gp_list_ccg_c %>%
        filter (Year %in% !!input$selectyear01) %>%
        select(region)
    }
    else if (input$selectdb_01 == "shape_wise") {
      gp_list_shape %>%
        filter (Year %in% !!input$selectyear01) %>%
        select(region)
    }
  })
  
  ## To see the regions
  output$regioninput1 <- renderUI({
    withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
      selectInput(inputId="region_select_01",
                  label="Select region:",
                  choices=unique(as.data.frame(gp_list_year_tab() )),selected ="NHS BATH AND NORTH EAST SOMERSET CCG",  multiple = FALSE
      )
    })
  })
  
  # select table postcode_full
  tbls_ccg_gp_full <- "ccg_gp_full_latnlong"
  
  # df from support postcode_full
  df_ccg_gp_full <-  lapply(paste(sql.support, tbls_ccg_gp_full), function(s) {
    tryCatch({ return(dbGetQuery(aggr_wodb, s)) 
    }, error = function(e) return(as.character(e)))
  })
  
  # postcode_full table
  ccg_gp_full <-   base::do.call(rbind, df_ccg_gp_full)
  
  ## To see the gp setting type
  output$settinginput1 <- renderUI({
    withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
      selectInput(inputId="setting_select_01",
                  label="Setting type:",
                  choices=c("all" , 
                            unique( ccg_gp_full$setting_type )),
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
    if (USER$Logged == TRUE) {
      div(
        sidebarUserPanel(
          isolate(input$userName),
          subtitle = a(icon("usr"), "Logout", href = login.page)
        ),
        sidebarMenu(
          tags$hr(),
          menuItem("Targeted", tabName = "year_wise"),
          menuItem("Non-Targeted", tabName = "heatmap_region"),
          tags$hr(),
          selectInput('selectdb_01', 'Select DB:',
                      c(
                        'CCG (c)' = 'ccg_wise_commissioner',
                        'Shape' = 'shape_wise'), 
                      selected = 'shape_wise'
          ),
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
    }
  })
  
  ## body
  
  output$body <- renderUI({
    if (USER$Logged == TRUE) {
      tabItems(
        
        # yearwise tab content
        tabItem(
          tabName = "year_wise",
          source (file.path("ui","tab-targeted.R"),local = TRUE)$value
        ), #End of Tab item
        # Heatmap tab content
        
        # Heatmap region tab content
        tabItem(
          tabName = "heatmap_region",
          source (file.path("ui","tab-non-targeted.R"),local = TRUE)$value
        )# End of Tab item
        
      )}
    else {
      login
    }
  })
  
  
  
  
}

