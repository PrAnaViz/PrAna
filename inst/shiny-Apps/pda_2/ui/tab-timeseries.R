tabPanel(
  title = "Time series - Bar Plot Complete",
  id    = "datasetTab",
  value = "datasetTab",
  name  = "datasetTab",
  class = "fade in",
  icon  = icon("table"),
         br(),
         
         column(9,
                box(width = "1300px",
                    br(),
                    # downloadButton ('downdat.time_barplot1',label = "Yearwise"),
                    # downloadButton ('downdat.data_api_monthwise_full2',label = "Monthwise"),
                    plotlyOutput("timeseries_bar_plot1",height="600px",width = "1000px"))
                
                
         ),
         column(12,
                tabBox(width = NULL,
                       tabPanel("Month wise - with Average",value = "filt_tab_year_01",
                                br()
                                # downloadButton ('downdat.data_time_comp04',label = "Selected API"),
                                # downloadButton ('downdat.data_time_comp03',label = "Complete"),
                                #plotlyOutput("timeseries_line_comp_plot1",height="550px")
                       ),
                       #               #             tabPanel("Data Table",
                       #               #                     dataTableOutput("tab_data_time_comp07")
                       #               #                     ),
                       #               # tabPanel("Data Table 2",
                       #               #          dataTableOutput("tab_data_time_comp07_02")
                       #               # ),
                       tabPanel("Month wise",value = "filt_tab_year_02",
                                br()
                                # downloadButton ('downdat.data_time_comp04',label = "Selected API"),
                                # downloadButton ('downdat.data_time_comp03',label = "Complete"),
                                # plotlyOutput("timeseries_line_comp_plot1",height="550px")
                                #plotlyOutput("timeseries_line_plot4",height="450px")
                       )
                       
                       
                       
                       
                )
         )# End of column
)