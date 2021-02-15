options(shiny.maxRequestSize = 5000000*1024^6)
ui <- shinydashboard::dashboardPage(
  header = shinydashboard::dashboardHeader(title = 'PrAnaViz'),
  body = shinydashboard::dashboardBody(
   shiny::uiOutput("body")
  ),
  sidebar = shinydashboard::dashboardSidebar(
    shiny::uiOutput("sidebarpanel"))
)
