#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    fluidRow(
        column(width = 3,
               box( 
                    selectInput('selectdb', 'Select DB:',
                                    c(
                                        'support' = 'support',
                                        'demo_one' = 'demo_one'
                                       ), selected = 'support'
                                    ),
                    uiOutput("uifile1"),
                    textInput("text", "Table name:", "type here"),
                    br(),
                    actionButton("submit", "Upload")
                   
               ) # End of Box
        ), # End of Column
        
        column(9,
               tabBox(width = NULL,
                      
                      tabPanel("tab sql",
                               dataTableOutput("tab_sqldata")      
                      ),
                      tabPanel("tab from sql",
                               selectInput('selectdb3', 'Select DB:',
                                           c(
                                               'support' = 'support',
                                               'demo_one' = 'demo_one'
                                           ), selected = 'support'
                               ),
                               uiOutput("tables_output"), 
                               dataTableOutput("tab_datafromsql")  
                      )
               )
        )
        
    ) # End of Fluid Row

))
