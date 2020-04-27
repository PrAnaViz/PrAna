

options(shiny.maxRequestSize = 5000000*1024^6)
ui <- dashboardPage(
  
  #tags$head(HTML("<script type='text/javascript' src='myscript.js'></script>")),
  header = dashboardHeader(title = 'PrAnaViz'),
  body = dashboardBody(
    #tags$head(tags$script(src = "myscript.js")),
    uiOutput("body")
  ),
  sidebar = dashboardSidebar(
    uiOutput("sidebarpanel"))
  
  
  
  
)
