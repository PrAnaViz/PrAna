# Load packages
library(shiny)
library(bs4Dash)
library(echarts4r)
library(thematic)
library(plotly)
library(waiter)

source("global.R")

thematic_shiny()

# toast options
toastOpts <- list(
  autohide = TRUE,
  icon = "fas fa-home",
  close = FALSE,
  position = "bottomRight"
)

# echarts4r theme #3d444c
echarts_dark_theme <- list(
  options = '{
    "color":["#6610f2", "#ffc107", "#e83e8c", "#ff851b", "#17a2b8", "#3d9970"], 
    "backgroundColor": "#343a40", 
    "textStyle": {
        color: "#fff"
    }
  }',
  name = "dark_theme"
)

shinyApp(
  ui = bs4Dash::dashboardPage(
    preloader = list(html = tagList(spin_1(), "Loading ..."), color = "#343a40"),
    dark = TRUE,
    fullscreen = TRUE,
    scrollToTop = TRUE,
    header = dashboardHeader(
      title = dashboardBrand(
        title = "PrAnaViz",
        color = "primary",
        opacity = 0.8
      ),
      fixed = TRUE,
      
      
      rightUi = tagList(
        dropdownMenu(
          badgeStatus = NULL,
          headerText = "Database String",
          .list = NULL,
          icon  = icon("database",lib = "font-awesome"),
          shiny::textInput("usernm","User name:"),
          shiny::passwordInput("pwd", "Password:"),
          shiny::textInput("hostnm", "host name:", "localhost"),
          shiny::actionButton("mysql_submit", "Submit")
        )
      )  ),
    sidebar = dashboardSidebar(
      fixed = TRUE,
      skin = "light",
      status = "primary",
      id = "sidebar",
      
      sidebarMenu(
        id = "current_tab",
        flat = FALSE,
        compact = FALSE,
        childIndent = TRUE,
        menuItem(
          "Targeted",
          tabName = "targeted",
          icon = icon("prescription")
        ),
        menuItem(
          "Non-Targeted",
          tabName = "nontargeted",
          icon = icon("map-marked-alt")
        )
      )
    ),
    body = dashboardBody(
      e_theme_register(echarts_dark_theme$options, name = echarts_dark_theme$name),
      tabItems(
        targeted_tab,
        nontarget_tab
      )
    ),
    controlbar = dashboardControlbar(
      id = "controlbar",
      skin = "light",
      pinned = TRUE,
      overlay = FALSE,
      controlbarMenu(
        id = "controlbarMenu",
        type = "pills",
        controlbarItem(
          "Skin",
          skinSelector()
        )
      )
    ),
    footer = dashboardFooter(
      fixed = FALSE,
      left = a(
        href = "https://pranaviz.github.io/",
        target = "_blank", "PrAnaViz"
      ),
      right = "2020"
    ),
    title = "PrAnaViz"
  ),
  server = function(input, output, session) {
    useAutoColor()
    
    
    # tab sources
    source(file.path("server", "tab-targeted.R"),  local = TRUE)$value
    source(file.path("server", "tab-non-targeted.R"),  local = TRUE)$value
    
    # alerts ------------------------------------------------------------------
    
    observeEvent(input$show_alert, {
      print("created")
      createAlert(
        id = "alert_anchor",
        options = list(
          title = "Be Careful!",
          status = "danger",
          closable = TRUE,
          width = 12,
          content = "Danger alert preview. This alert is dismissable. 
          A wonderful serenity has taken possession of my entire soul, 
          like these sweet mornings of spring which 
          I enjoy with my whole heart."
        )
      )
    })
    
    observeEvent(input$hide_alert, {
      print("deleted")
      closeAlert(id = "alert_anchor")
    })
    
    # alert callback event
    observeEvent(input$alert_anchor, {
      alertStatus <- if (input$alert_anchor) "opened" else "closed"
      toastColor <- if (input$alert_anchor) "bg-lime" else "bg-fuchsia"
      toast(
        title = sprintf("Alert succesfully %s!", alertStatus),
        options = list(
          class = toastColor,
          autohide = TRUE,
          position = "bottomRight"
        )
      )
    })
    
    
    # current theme info ---------------------------------------------------------
    
    observeEvent(input$dark_mode, {
      toast(
        title = if (input$dark_mode) "Dark theme on!" else "Light theme on",
        options = list(position = "topRight", class = "bg-warning", autohide = TRUE)
      )
    })
    
    
    # controlbar input --------------------------------------------------------
    
    observeEvent(input$controlbar, {
      toastOpts <- list(
        autohide = TRUE,
        icon = "fas fa-home",
        close = TRUE,
        position = "bottomRight"
      )
      toastOpts$class <- if (input$controlbar) "bg-success" else "bg-danger"
      toast(
        title = if (input$controlbar) "Controlbar opened!" else "Controlbar closed!",
        options = toastOpts
      )
    })
    
    observe({
      print(input$controlbar)
    })
    
  }
)