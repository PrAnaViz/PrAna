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
                            pswd = c("123","chloramphenicol1","Ckhepoler.20","r3sist-r3sistanc3"))
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
  
  data01 <- reactive({ 
    importdmd("C:/dmdDataLoader/excel/")
  })
   
  ### Outputs
  
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
          menuItem(
            "SNOMED",
            tabName = "snomed",
            icon = icon("table")
          )
          
          
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
                          dataTableOutput("tab_data01" ,height="450px"))
                 
                 
          ) # End of tabBox     
        ) # End of fluidRow
      )
         
        
      )}
    else {
      login
    }
  })
  
  
  ## Data tables
  
  output$tab_data01<- renderDataTable(
    withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
    data01()
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
 
}
