## Ui
targeted_tab <- tabItem(
  tabName = "targeted",
  fluidRow(
    column(12,
           box(width = NULL,
               id = "NULL",
               collapsible = TRUE,
               closable = FALSE, 
               maximizable = TRUE,
               fluidRow(
                 column(12,
                        fluidRow(
                          column(3, 
                                 uiOutput("uifile1"),
                                 checkboxInput("sel_target", label = "Sample targets", value = TRUE),
                                 actionButton(inputId = "gen_barplot01",
                                              label = "Generate Graph", 
                                              class="btn btn-success action-button")
                                 
                          ),
                          column(4, offset = 1,
                                 selectInput('selectyear01', 'Select Prescirption Year:',
                                             c(
                                               '2015' = '2015',
                                               '2016' = '2016',
                                               '2017' = '2017',
                                               '2018' = '2018'), 
                                             selected = '2018'),
                                 radioButtons('select_bar', "Select Graph type:",
                                              c(
                                                'Month' = 'monthwise',
                                                'Year' = 'yearwise'),                                                               
                                              selected = 'yearwise',
                                              inline = TRUE)),
                          column(4,
                                 uiOutput("regioninput1"),
                                 uiOutput("settinginput1"))
                        ),
                 ),
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
           tabBox(width = NULL, id = "tabbox01",
                  
                  # collapsible = TRUE,
                  #  closable = FALSE, 
                  #  maximizable = TRUE,
                  tabPanel(
                    id = NULL,
                    title =  "Period",
                    tabName = "Period",value = "filt_tab_period",
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
                  tabPanel(
                    id = NULL,
                    title = "Practice",
                    tabName = "Practice",value = "filt_tab_practice",
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
                  tabPanel(
                    id = NULL,
                    title = "Postcode",
                    tabName = "Postcode",value = "filt_tab_postcode",
                    icon  = icon("map-marker-alt",lib = "font-awesome"),
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
                  
                  tabPanel(
                    id = NULL,
                    title = "Medicinal Form",
                    tabName = "Medicinal Form",value = "filt_tab_medform",
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
                  )
                  
           ) # End of TabBox
    )# End of column  
  ) # End of Fluidrow,
  
)