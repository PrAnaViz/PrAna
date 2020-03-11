
# Subset yearwise data
data_yearwise <- reactive ({ 
  tbl(aggr01_2014_2018, "Five_catchment_2014_2018") %>%
    filter(site %in% input$searchoption2) %>% 
    group_by(site, CPD, PRACTICE, PERIOD) %>%
    summarize(sum = sum(gram, na.rm = T)) %>%
    filter(tolower(CPD) %in% targetdata()$V1) %>%
    mutate(Year=(substr(PERIOD, 1, 4))) %>%
    group_by(site, CPD, Year) %>%
    summarize(gram = sum(sum, na.rm = T)) %>%
    mutate(kg = gram/1000)
})

output$timeseries_bar_plot1 <- renderPlotly({ 
  withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
    
    p <- ggplot(data=data_yearwise(), aes(x=CPD, y=kg, fill=Year, key = Year )) +  
      geom_bar(stat="identity", position=position_dodge())+ 
      ylab("Total CPD (kg)") +
      xlab("CPD")+
      ggtitle("Total Prescription for the year 2014, 2016, 2017, 2018 in kg")+
      theme(axis.title.x = element_text(face="bold", size=10),
            axis.title.y = element_text(face="bold", size=10),
            axis.text.x  = element_text(angle=45, vjust=0.5, size=10))
    ggplotly(p, source = "time_bar01",tooltip =c("CPD","kg","Year") )# %>% layout(dragmode = "select")
  })
})


# output$timeseries_line_comp_plot1 <- renderPlotly({ 
#   withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
#     s <- event_data("plotly_click", source = "time_bar01")
#     if (length(s) == 0) {
#       NULL
#     }   
#     else {
#       dat <- data_time_comp07()
#       a1 <- subset(dat,API_YF %in% dat$API_YF[s$pointNumber+1]  )
#       a2 <- a1[!grepl("_YF", a1$key ),]
#       a3 <- a1[grepl("_YF", a1$key ), ]
#       a4 <- as.data.frame( cbind(Year =as.numeric(as.character(a3$"Year")),kg = a3$"kg"))
#       
#       
#       p <- ggplot(data=a2) +  
#         geom_point( aes(x=Month, y=kg, color = Year))+ 
#         geom_hline(data = a4, aes(yintercept =a4$kg))+
#         facet_grid( Year~. )+
#         ylab("Total API (kg)") +
#         xlab("Month")+
#         ggtitle(paste0(dat$API_YF[s$pointNumber+1], " prescription for the year 2014, 2016, 2017, 2018 in kg"))+
#         theme(axis.title.x = element_text(face="bold", size=10),
#               axis.title.y = element_text(face="bold", size=10),
#               axis.text.x  = element_text(angle=45, vjust=0.5, size=10))
#     }
#   })
# })