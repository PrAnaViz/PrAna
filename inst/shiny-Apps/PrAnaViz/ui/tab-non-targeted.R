tabPanel(
  title = "Non-Targeted Approach",
  id    = "datasetTab",
  value = "datasetTab",
  name  = "datasetTab",
  class = "fade in",
  br(),
  
  fluidRow(
    
    column(
      width = 7,
      box(
        width = NULL, 
        status = "warning",
        solidHeader = TRUE, 
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
          column(
            width = 4,
            uiOutput("selected_api_input")
          ),
          column(
            width = 6,
            sliderInput("sel_month", "Select Month:",
                        min = 0, max = 12, value = 8),
            actionButton(inputId = "gen_leaflet01",
                         label = "Generate Graph", class="btn btn-success action-button")
          )
        ),
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
        tags$hr(),
        leafletOutput("postcodemap", height="700px"),
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
        tags$style("#txt_line_gp_title 
                                            {
                                            font:16px Arial, Helvetica, sans-serif;
                                            line-height: 1.1; 
                                            text-align: center;
                                            font-weight: bold;
                                            }"
        ),
        htmlOutput("txt_line_gp_title"),
        plotlyOutput("filt_gp_lineplot1",height="350px"),
        tags$hr(),
        uiOutput("uidownload_nt_02")
      )
    ) # End of column
  ) # End of fluid row
)# End of TabPanel