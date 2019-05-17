
# CSS to use in the app
appCSS <-
  ".mandatory_star { color: red; }
.shiny-input-container { margin-top: 25px; }
#submit_msg { margin-left: 15px; }
#error { color: red; }
body { background: #fcfcfc; }
#header { background: #fff; border-bottom: 1px solid #ddd; margin: -20px -15px 0; padding: 15px 15px 10px; }
"


ui <- dashboardPage(
  dashboardHeader(title = 'PDA 1.0'),
  dashboardSidebar(
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(appCSS),
    #tags$head(includeScript("google-analytics.js")),
    uiOutput("sidebarpanel")
  ),

dashboardBody(
  uiOutput("body")
  )


)