nontarget_tab <- tabItem(
  tabName = "nontargeted",
  fluidRow(
    
    column(
      width = 7,
      box(
        width = NULL, 
        status = "warning",
        solidHeader = TRUE, 
        collapsible = TRUE,
        closable = FALSE, 
        maximizable = TRUE,
        tags$style(type = "text/css", 
                   ".leaflet .legend i{
                   width: 18px;
                   height: 18px;
                   float: left;
                   margin: 0 8px 0 0;
                   }",
                   ".leaflet .legend {
                    padding: 5px 5px;
                    font: 14px Arial, Helvetica, sans-serif;
                     /*background: white;*/
                    background: rgba(255, 255, 255, 0.8);
                    /*box-shadow: 0 0 15px rgba(0, 0, 0, 0.2);*/
                    border-radius: 5px;
                    line-height: 24px;
                    color: #555;
                    }",
                   ".leaflet .info {
                    position: initial;
                    left: 20px;
                    top: 90px;
                    width: fit-content;
                    height: fit-content;
                    text-align: left;
                    }"    
                   
        ),
        fluidRow(
          column(6,
                 uiOutput("selected_api_input"),
                 uiOutput("regioninput2"),
                 radioGroupButtons(
                   inputId = "selPlot_nt",
                   choiceNames =
                     list(icon("chart-bar"), icon("map-marked-alt")),
                   choiceValues =
                     list( "bar", "map"),
                   label = "Type", 
                   status = "default"
                 )
          ),
          column(4,
                 selectInput('selectyear02', 'Select Year:',
                             c(
                               '2015' = '2015',
                               '2016' = '2016',
                               '2017' = '2017',
                               '2018' = '2018'), 
                             selected = '2018'),
                 selectInput("sel_month", "Select Month:",
                             c(
                               'January' = 'January',
                               'February' = 'February',
                               'March' = 'March',
                               'April' = 'April',
                               'May' = 'May',
                               'June' ='June', 
                               'July'= 'July',
                               'August' = 'August',
                               'September' = 'September',
                               'November'='November',
                               'December' ='December')),
                 actionButton(inputId = "gen_leaflet01",
                              label = "Generate Graph", 
                              class="btn btn-success action-button")
          )  
        ),
        tags$hr(),
        conditionalPanel("input.selPlot_nt == 'map'",
                         
                         fluidRow(
                           column(
                             width = 12,
                             tags$style("#txt_leaflet 
                                            {
                                            font:16px Arial, Helvetica, sans-serif;
                                            line-height: 1.1; 
                                            text-align: left;
                                            font-weight: bold;
                                            }"
                             ),
                             htmlOutput("txt_leaflet")
                           )
                         ),
                         leafletOutput("postcodemap", height="700px")
        ),
        conditionalPanel("input.selPlot_nt == 'bar'",
                         fluidRow(
                           column(
                             width = 12,
                             tags$style("#txt_barplot_nt 
                                            {
                                            font:16px Arial, Helvetica, sans-serif;
                                            line-height: 1.1; 
                                            text-align: left;
                                            font-weight: bold;
                                            }"
                             ),
                             htmlOutput("txt_barplot_nt")
                           )
                         ),
                         plotlyOutput("filt_barplot1_nt")
        ),
        tags$hr(),
        fluidRow(
          column(
            width = 12,
            uiOutput("uidownload_nt_01"),
            br()
          )
        )
      )),
    column( 
      width = 5,
      box( 
        width = NULL,
        status = "info",
        solidHeader = TRUE, 
        collapsible = TRUE,
        closable = FALSE, 
        maximizable = TRUE,
        conditionalPanel("input.selPlot_nt == 'map'",
                         
                         tags$style("#txt_line_gp_title1 
                          {
                          font:16px Arial, Helvetica, sans-serif;
                          line-height: 1.1; 
                          text-align: center;
                          font-weight: bold;
                                     }"
                         ),
                         htmlOutput("txt_line_gp_title1"),
                         plotlyOutput("filt_gp_lineplot1",height="350px")
        ),
        
        conditionalPanel("input.selPlot_nt == 'bar'",
                         
                         tags$style("#txt_line_gp_title2 
                          {
                          font:16px Arial, Helvetica, sans-serif;
                          line-height: 1.1; 
                          text-align: center;
                          font-weight: bold;
                                     }"
                         ),
                         htmlOutput("txt_line_gp_title2"),
                         plotlyOutput("filt_gp_lineplot2",height="350px")
        ),
        tags$hr(),
        uiOutput("uidownload_nt_02")
      )
    ) # End of column
  ) # End of fluid row
)# End of TabPanel
