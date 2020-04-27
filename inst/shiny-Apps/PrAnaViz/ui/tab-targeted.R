tabPanel(
  title = "Targeted Approach",
  id    = "datasetTab",
  value = "datasetTab",
  name  = "datasetTab",
  class = "fade in",
  br(),
  fluidRow(
    column(12,
           box(width = NULL,
               br(),
               fluidRow(
                 column(4,
                        uiOutput("uifile1")
                 ),
                 column(4,
                        radioButtons('select_bar', "Select Period:",
                                     c(
                                       'Month' = 'monthwise',
                                       'Year' = 'yearwise'),                                                               
                                     selected = 'yearwise',
                                     inline = TRUE
                        ),
                        actionButton(inputId = "gen_barplot01",
                                     label = "Generate Graph", class="btn btn-success action-button")
                        
                 )
               ),
               tags$hr(),
               conditionalPanel("input.select_bar == 'yearwise'",
                                tags$style("#txt_tot_barplot1_title {
                                                        font:16px Arial, Helvetica, sans-serif;
                                                        line-height: 1.1; 
                                                        text-align: center;
       
                                                        }"
                                ),
                                htmlOutput('txt_tot_barplot1_title'),
                                plotlyOutput("filt_total_barplot1",height="400px")
               ),
               conditionalPanel("input.select_bar == 'monthwise'",
                                tags$style("#txt_lineplot11_title {
                                                        font:16px Arial, Helvetica, sans-serif;
                                                        line-height: 1.1; 
                                                        text-align: center;
       
                                                        }"
                                ),
                                htmlOutput('txt_lineplot11_title'),
                                plotlyOutput("filt_total_lineplot11",height="400px")
               ),
               tags$hr(),
               fluidRow(
                 column(7,
                        conditionalPanel("input.select_bar == 'yearwise'",
                                         uiOutput("uidownload_t_01")
                        ),
                        conditionalPanel("input.select_bar == 'monthwise'",
                                         uiOutput("uidownload_t_02")
                        )
                 )
               )
           )
    ),
    column(12,
           tabBox(width = NULL,
                  tabPanel("Period",value = "filt_tab_period",
                           icon  = icon("calendar-alt",lib = "font-awesome"),
                           br(),
                           tags$style("#txt_tot_lineplot1_title {
                                                 font:16px Arial, Helvetica, sans-serif;
                                                 line-height: 1.1; 
                                                 text-align: center;
 
                                                  }"
                           ),
                           uiOutput("uidownload_t_03"),
                           br(),
                           htmlOutput('txt_tot_lineplot1_title'),
                           plotlyOutput("filt_total_lineplot1",height="450px")
                  ),
                  tabPanel("Practice",value = "filt_tab_practice",
                           icon  = icon("hospital",lib = "font-awesome"),
                           br(),
                           tags$style("#txt_tot_lineplot2_title {
                                                 font:16px Arial, Helvetica, sans-serif;
                                                 line-height: 1.1; 
                                                 text-align: center;
 
                                                  }"
                           ),
                           uiOutput("uidownload_t_04"),
                           br(),
                           htmlOutput('txt_tot_lineplot2_title'),
                           plotlyOutput("filt_total_lineplot2",height="450px")
                  ),
                  tabPanel("Postcode",value = "filt_tab_postcode",
                           icon  = icon("map-marked-alt",lib = "font-awesome"),
                           br(),
                           tags$style("#txt_tot_lineplot3_title {
                                                 font:16px Arial, Helvetica, sans-serif;
                                                 line-height: 1.1; 
                                                 text-align: center;
 
                                                  }"
                           ),
                           uiOutput("uidownload_t_05"),
                           br(),
                           htmlOutput('txt_tot_lineplot3_title'),
                           plotlyOutput("filt_total_lineplot3",height="450px")
                  ),
                  
                  tabPanel("Medicinal Form",value = "filt_tab_medform",
                           icon  = icon("pills",lib = "font-awesome"),
                           br(),
                           tags$style("#txt_tot_lineplot4_title {
                                                 font:16px Arial, Helvetica, sans-serif;
                                                 line-height: 1.1; 
                                                 text-align: center;
 
                                                  }"
                           ),
                           uiOutput("uidownload_t_06"),
                           br(),
                           htmlOutput('txt_tot_lineplot4_title'),
                           plotlyOutput("filt_total_lineplot4",height="450px")
                  ),
                  tabPanel("Practice - Monthwise",value = "filt_tab_monthwise",
                           icon  = icon("calendar-plus",lib = "font-awesome"),
                           br(),
                           tags$style("#txt_tot_monthwise_gp_title {
                                                 font:16px Arial, Helvetica, sans-serif;
                                                 line-height: 1.1; 
                                                 text-align: center;
 
                                                  }"
                           ),
                           htmlOutput('txt_tot_monthwise_gp_title'),
                           plotlyOutput("plot_compound_monthwise",height="450px")
                  )
           ) # End of TabBox
    )# End of column  
  ) # End of Fluidrow
)# End of TabPanel
