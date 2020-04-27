# For drug targets
targetdata <- reactive({
  inFile <- input$file1
  if (is.null(inFile))
    return(NULL)
  
  fread(inFile$datapath, header = FALSE,
        sep = ",", quote = '"')
})

# Select db for the gp list
gp_list_year  <- reactive ({ 
  if (input$selectdb_01 == "ccg_wise_commissioner") {
    gp_list_ccg_c 
  }
  else if (input$selectdb_01 == "shape_wise") {
    gp_list_shape 
  }
})

list_practices <- reactive({
  if (input$setting_select_01 == "all") {
    gp_list_year () %>%
      dplyr::rename (organisation_code = PRACTICE ) %>%
      merge(ccg_gp_full[,c("organisation_code","setting_type")] , by = "organisation_code") %>%
      dplyr::rename (PRACTICE = organisation_code ) %>% 
      dplyr::rename (Year = year ) 
  }
  else {
    gp_list_year () %>%
      dplyr::rename (organisation_code = PRACTICE ) %>%
      merge(ccg_gp_full[,c("organisation_code","setting_type")] , by = "organisation_code") %>%
      filter (setting_type %in% !!input$setting_select_01) %>% 
      dplyr::rename (PRACTICE = organisation_code ) %>% 
      dplyr::rename (Year = year ) 
  }
})

# Get the list of all the practices based on year and region
list_practices_year <- reactive({
  list_practices () %>%
    filter (Year %in% !!input$selectyear01) %>%
    filter (region %in% !!input$region_select_01) %>%
    select(PRACTICE)
  
})

# Filter out the practices based on selected region and practices
tbls_yearwise <- reactive ({
  as.data.frame(list_practices_year())$"PRACTICE"
})

## Need to change the database once we have all regions in a sql database
dfList_banes <- reactive({
  lapply(paste(sql.demo_one, tbls_yearwise()), function(s) {
    tryCatch({ return(dbGetQuery(aggr_wodb, s)) 
    }, error = function(e) return(as.character(e)))
  })
  
}) 

## Bind all the practices
bind_practices <- reactive({
  base::do.call(rbind, dfList_banes())  %>%
    distinct()
})

# Subset yearwise data
filt_total_NM <- reactive ({
  bind_practices() %>%
    filter (!grepl("Error: Table",CPD)) %>% 
    dplyr::mutate(NM = tolower(NM) ) %>% 
    dplyr::filter (str_detect(NM , paste(paste0("^",!!targetdata()$V1,collapse = '|')))) %>%
    mutate(Year=(substr(PERIOD, 1, 4))) %>%
    filter (Year %in% !!input$selectyear01) %>%
    mutate(gram2 = as.numeric(gram)) %>%
    group_by(NM, Year) %>%
    summarize(gram_sum = sum(gram2, na.rm = T)) %>%
    mutate(kg = gram_sum/1000) 
})

# Subset data - Period
filt_total_period  <- reactive ({ 
  bind_practices() %>%
    filter (!grepl("Error: Table",CPD)) %>% 
    dplyr::mutate(NM = tolower(NM) ) %>% 
    dplyr::filter (str_detect(NM , paste(!!targetdata()$V1,collapse = '|'))) %>%
    mutate(Year=(substr(PERIOD, 1, 4))) %>%
    filter (Year %in% !!input$selectyear01) %>%
    mutate(gram2 = as.numeric(gram)) %>%
    group_by(NM, PERIOD) %>%
    summarize(gram_sum = sum(gram2, na.rm = T)) %>%
    mutate(kg = gram_sum/1000) 
})

# Subset yearwise data - Practice
filt_total_practice <- reactive ({ 
  bind_practices() %>%
    filter (!grepl("Error: Table",CPD)) %>% 
    dplyr::mutate(NM = tolower(NM) ) %>% 
    dplyr::filter (str_detect(NM , paste(!!targetdata()$V1,collapse = '|'))) %>%
    mutate(Year=(substr(PERIOD, 1, 4))) %>%
    filter (Year %in% !!input$selectyear01) %>%
    mutate(gram2 = as.numeric(gram)) %>%
    group_by(NM, PRACTICE) %>%
    summarize(gram_sum = sum(gram2, na.rm = T)) %>%
    mutate(kg = gram_sum/1000) 
})

# Subset yearwise data - Postcode
filt_total_postcode <- reactive ({ 
  bind_practices() %>%
    filter (!grepl("Error: Table",CPD)) %>% 
    dplyr::mutate(NM = tolower(NM) ) %>% 
    dplyr::filter (str_detect(NM , paste(!!targetdata()$V1,collapse = '|'))) %>%
    mutate(Year=(substr(PERIOD, 1, 4))) %>%
    filter (Year %in% !!input$selectyear01) %>%
    mutate(gram2 = as.numeric(gram)) %>%
    left_join(gp_postcode_full(), by=c("PRACTICE"="PRACTICE", "PERIOD" = "PERIOD")) %>%
    group_by(NM, postcode) %>%
    summarize(gram_sum = sum(gram2, na.rm = T)) %>%
    mutate(kg = gram_sum/1000) 
})

# Subset yearwise data - Medicinal Form
filt_total_dform <- reactive ({
  bind_practices() %>%
    filter (!grepl("Error: Table",CPD)) %>% 
    dplyr::mutate(NM = tolower(NM) ) %>% 
    dplyr::filter (str_detect(NM , paste(!!targetdata()$V1,collapse = '|'))) %>%
    mutate(Year=(substr(PERIOD, 1, 4))) %>%
    filter (Year %in% !!input$selectyear01) %>%
    mutate(gram2 = as.numeric(gram)) %>%
    mutate(DForm = as.numeric(DForm)) %>%
    mutate(CD = DForm) %>% 
    left_join(dform_desc, by="CD") %>%
    group_by(NM, DESC) %>%
    summarize(gram_sum = sum(gram2, na.rm = T)) %>%
    mutate(kg = gram_sum/1000) 
})

# Subset yearwise data - Practice monthwise
filt_total_practice_month <- reactive ({ 
  bind_practices() %>%
    filter (!grepl("Error: Table",CPD)) %>% 
    dplyr::mutate(NM = tolower(NM) ) %>% 
    dplyr::filter (str_detect(NM , paste(!!targetdata()$V1,collapse = '|'))) %>%
    mutate(Year=(substr(PERIOD, 1, 4))) %>%
    filter (Year %in% !!input$selectyear01) %>%
    mutate(gram2 = as.numeric(gram)) %>%
    group_by(NM, PRACTICE, PERIOD) %>%
    summarize(gram_sum = sum(gram2, na.rm = T)) %>%
    mutate(kg = gram_sum/1000) 
})

# Data for download csv

# barplot data
data.barplot_t_01 <- reactive ({
  dcast(as.data.frame(filt_total_NM()), NM~Year,value.var= "kg") 
})

data.lineplot_t_01 <- reactive ({
  dcast(as.data.frame(filt_total_period()), NM~PERIOD,value.var= "kg") 
})


### Outputs

## UI
# ui for file input 01
output$uifile1 <- renderUI({
  tipify(fileInput('file1', 'Upload Targets:',
                   accept = c(
                     'text/csv',
                     'text/comma-separated-values',
                     'text/tab-separated-values',
                     'text/plain',
                     '.csv',
                     '.tsv'
                   ),
                   placeholder = "Upload Targets file (.csv)"
  ),
  "Click here to upload your targets file")
})

# ui for download 01
output$uidownload_t_01 <- renderUI({
  withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
    tags$span(
      downloadButton('downloaddata_t_01', 'Download CSV'),
      downloadButton('downloadpdf_t_01', 'Download PDF'),
      downloadButton('downloadeps_t_01', 'Download EPS')
    )
  })
})

# ui for download 02
output$uidownload_t_02 <- renderUI({
  withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
    tags$span(
      downloadButton('downloaddata_t_02', 'Download CSV'),
      downloadButton('downloadpdf_t_02', 'Download PDF'),
      downloadButton('downloadeps_t_02', 'Download EPS')
    )
  })
})


# ui for download 03
output$uidownload_t_03 <- renderUI({
  withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
    tags$span(
      downloadButton('downloaddata_t_03', 'Download CSV'),
      downloadButton('downloadpdf_t_03', 'Download PDF'),
      downloadButton('downloadeps_t_03', 'Download EPS')
    )
  })
})

# ui for download 04
output$uidownload_t_04 <- renderUI({
  withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
    tags$span(
      downloadButton('downloaddata_t_04', 'Download CSV'),
      downloadButton('downloadpdf_t_04', 'Download PDF'),
      downloadButton('downloadeps_t_04', 'Download EPS')
    )
  })
})

# ui for download 05
output$uidownload_t_05 <- renderUI({
  withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
    tags$span(
      downloadButton('downloaddata_t_05', 'Download CSV'),
      downloadButton('downloadpdf_t_05', 'Download PDF'),
      downloadButton('downloadeps_t_05', 'Download EPS')
    )
  })
})

# ui for download 06
output$uidownload_t_06 <- renderUI({
  withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
    tags$span(
      downloadButton('downloaddata_t_06', 'Download CSV'),
      downloadButton('downloadpdf_t_06', 'Download PDF'),
      downloadButton('downloadeps_t_06', 'Download EPS')
    )
  })
})




## Text outputs


output$txt_lineplot11_title <- renderUI({ HTML( paste0("Total compounds prescribed at each month\nover ",input$region_select_01, "\nfor the year ", input$selectyear01," (in kg)"  ) )})

output$txt_tot_lineplot1_title <- renderUI({ 
  s <- event_data("plotly_click", source = "filt_total_barplot1")
  if (length(s) == 0) {
    NULL
  }   
  else {
    HTML( paste0("Quantity of ",s[["x"]]," prescribed at each month\nover ",input$region_select_01, "\nfor the year ", input$selectyear01," (in kg)")) 
  }
})

output$txt_tot_lineplot2_title <- renderUI({ 
  s <- event_data("plotly_click", source = "filt_total_barplot1")
  if (length(s) == 0) {
    NULL
  }   
  else {
    HTML( 
      paste0("Total quantity of ",s[["x"]]," prescribed at each GP practice\nover ",input$region_select_01, "\nfor the year ", input$selectyear01," (in kg)")
    ) 
  }
})

output$txt_tot_lineplot3_title <- renderUI({ 
  s <- event_data("plotly_click", source = "filt_total_barplot1")
  if (length(s) == 0) {
    NULL
  }   
  else {
    HTML( 
      paste0("Total quantity of ",s[["x"]]," prescribed at each Postcode \nover ",input$region_select_01, "\nfor the year ", input$selectyear01," (in kg)")
    ) 
  }
})

output$txt_tot_lineplot4_title <- renderUI({ 
  s <- event_data("plotly_click", source = "filt_total_barplot1")
  if (length(s) == 0) {
    NULL
  }   
  else {
    HTML( paste0("Total quantity of ",s[["x"]]," prescribed by different medicinal form\nover ",input$region_select_01, "\nfor the year ", input$selectyear01," (in kg)")
    ) 
  }
})

output$txt_tot_monthwise_gp_title <- renderUI({ 
  s <- event_data("plotly_click", source = "filt_total_barplot1")
  if (length(s) == 0) {
    NULL
  }   
  else {
    HTML( paste0("Total quantity of ",s[["x"]]," prescribed every month at different GP practice\nover ",input$region_select_01, "\nfor the year ", input$selectyear01," (in kg)")
    ) 
  }
})

## Plots

# Plotly barplot 01 - Yearwise

observeEvent(input$gen_barplot01, {
  
  output$txt_tot_barplot1_title <- renderUI({ HTML( paste0("Total Compounds prescribed at ",input$region_select_01, "\nover the year ",input$selectyear01, " (in kg)")) })
  
  output$filt_total_barplot1 <- renderPlotly({ 
    withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
      plot_ly(as.data.frame(filt_total_NM()), x = ~NM, y = ~kg,  type = "bar", color = ~NM, source = "filt_total_barplot1") %>%
        layout(title = FALSE,
               font = title_font,
               xaxis = list(title = "API"), 
               yaxis = list(title = "kg"))      
    }) 
  })
  
})

# Plotly lineplot 00 - Monthwise all targets
output$filt_total_lineplot11 <- renderPlotly({ 
  withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
    a1 <- as.data.frame(filt_total_period())
    a1$PERIOD <-  as.character(a1$PERIOD)
    a2 <-  plot_ly(a1, x = ~PERIOD, y = ~kg, type = 'scatter',mode = 'lines+markers',color = ~NM, source = "filt_total_lineplot11")%>%
      layout(title =FALSE,
             font = title_font,
             xaxis = list(title = "Year / Month"),
             yaxis = list (title = "kg / month"))
    a2
    
  })
})

# Plotly lineplot 01 - period
output$filt_total_lineplot1 <- renderPlotly({ 
  withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
    s <- event_data("plotly_click", source = "filt_total_barplot1")
    if (length(s) == 0) {
      NULL
    }   
    else { 
      a1 <- subset (as.data.frame(filt_total_period()), NM %in%  s[["x"]])
      a1$PERIOD <-  as.character(a1$PERIOD)
      a2 <-  plot_ly(a1, x = ~PERIOD, y = ~kg, type = 'scatter',mode = 'lines+markers')%>%
        layout(title = FALSE,
               font = title_font,
               xaxis = list(title = "Year / Month"),
               yaxis = list (title = "kg / month"))
      a2
    }
  })
})

# Plotly lineplot 02 - practice
output$filt_total_lineplot2 <- renderPlotly({ 
  withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
    s <- event_data("plotly_click", source = "filt_total_barplot1")
    if (length(s) == 0) {
      NULL
    }   
    else {
      a1 <- subset (as.data.frame(filt_total_practice()),NM %in% s[["x"]])
      a2 <-  plot_ly(a1, x = ~PRACTICE, y = ~kg, type = 'scatter',mode = 'lines+markers')%>%
        layout(title = FALSE ,
               font = title_font,
               xaxis = list(title = "GP Practice Code"),
               yaxis = list (title = "kg /  GP Practice"))
      a2
    }
  })
})

# Plotly lineplot 03 - postcode
output$filt_total_lineplot3 <- renderPlotly({ 
  withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
    s <- event_data("plotly_click", source = "filt_total_barplot1")
    if (length(s) == 0) {
      NULL
    }   
    else {
      a1 <- subset (as.data.frame(filt_total_postcode()),NM %in% s[["x"]])
      a2 <-  plot_ly(a1, x = ~postcode, y = ~kg, type = 'scatter',mode = 'lines+markers')%>%
        layout(title = FALSE ,
               font = title_font,
               xaxis = list(title = "Postcode"),
               yaxis = list (title = "kg /  Postcode"))
      a2
    }
  })
})

# Plotly lineplot 04 - medform
output$filt_total_lineplot4 <- renderPlotly({ 
  withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
    s <- event_data("plotly_click", source = "filt_total_barplot1")
    if (length(s) == 0) {
      NULL
    }   
    else {
      a1 <- subset (as.data.frame(filt_total_dform()),NM %in% s[["x"]])
      a2 <-  plot_ly(a1, x = ~DESC, y = ~kg, type = 'scatter',mode = 'lines+markers')%>%
        layout(title = FALSE ,
               font = title_font,
               xaxis = list(title = "Medicinal Form"),
               yaxis = list (title = "kg /  medicinal form"))
      a2
    }
  })
})

# Plotly lineplot 05 - monthwise
output$plot_compound_monthwise <- renderPlotly({ 
  withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
    s <- event_data("plotly_click", source = "filt_total_barplot1")
    if (length(s) == 0) {
      NULL
    }   
    else {
      a1 <- subset(as.data.frame(filt_total_practice_month()), NM %in%    s[["x"]])
      a1$PERIOD <-  as.character(a1$PERIOD)
      a2 <-  plot_ly(a1, x = ~PERIOD, y = ~kg, type = 'scatter',mode = 'lines+markers',color=~PRACTICE)%>%
        layout(title = FALSE ,
               font = title_font,
               xaxis = list(title = "PERIOD (YYYYMM)"),
               yaxis = list (title = "kg / month"))
      a2
      
    }
  })
})

## Download buttons

# Download csv
output$downloaddata_t_01 = downloadHandler(
  filename = function (){ paste(input$region_select_01,'_',input$selectyear01,'_yearwise','.csv',sep = '')},
  content = function(file) {
    write.csv(as.data.frame(data.barplot_t_01()), file, row.names = TRUE)
  }
)

output$downloaddata_t_02 = downloadHandler(
  filename = function (){ paste(input$region_select_01,'_',input$selectyear01,'_monthwise', '.csv',sep = '')},
  content = function(file) {
    write.csv(as.data.frame(data.lineplot_t_01()), file, row.names = TRUE)
  }
)

# Download eps
output$downloadeps_t_01= downloadHandler(
  filename = function(){ paste(input$region_select_01,'_',input$selectyear01,'_yearwise','.eps',sep = '')},
  content = function(file) {
    postscript(file,
               width = 11.69 , height = 8.27, # inches
               horizontal = TRUE, onefile = TRUE, paper = "special")
    g = ggplot(data=as.data.frame(filt_total_NM()), aes(x=NM, y=kg, fill=NM)) +
      geom_bar(stat="identity") +
      labs(title = paste("Total Compounds prescribed at",input$region_select_01, "\nover the year",input$selectyear01, "(in kg)") )+
      labs(x = "API", y = "kg", color = "API")+
      theme(
        plot.title = element_text(size = 18, face = "bold"  , hjust = 0.5 ),
        axis.title.x = element_text(size = 16, face = "bold"   ),
        axis.title.y = element_text(size = 16, face = "bold"   ),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 16   ),
        axis.text.y = element_text(size = 16   ),
        legend.text = element_text(size = 16   ),
        legend.title = element_text(size = 16, face = "bold"   )
      )
    print(g)
    dev.off()
    
  }
)

output$downloadeps_t_02 = downloadHandler(
  filename = function(){ paste(input$region_select_01,'_',input$selectyear01,'_monthwise','.eps',sep = '')},
  content = function(file) {
    postscript(file,
               width = 11.69 , height = 8.27, # inches
               horizontal = TRUE, onefile = TRUE, paper = "special")
    a1 <- as.data.frame(filt_total_period())
    a1$PERIOD <-  as.character(a1$PERIOD)
    g = ggplot(data=a1, aes(x=PERIOD, y=kg, group = NM)) +
      geom_line(aes(color = NM ) ,
                size = 0.5) +  
      geom_point(aes(color = NM ),
                 shape = 20,    
                 size = 4) +
      labs(title = paste("Total compounds prescribed at each month over",input$region_select_01, "\nfor the year", input$selectyear01,"(in kg)"  ))+
      labs(y = "kg / Month", x = "Year / month (YYYYMM)", color = "API")+
      theme(
        plot.title = element_text(size = 18, face = "bold"  , hjust = 0.5 ),
        axis.title.x = element_text(size = 16, face = "bold"   ),
        axis.title.y = element_text(size = 16, face = "bold"   ),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 16   ),
        axis.text.y = element_text(size = 16   ),
        legend.text = element_text(size = 16,   family= "Arial" ),
        legend.title = element_text(size = 16, face = "bold"   )
      )
    print(g)
    dev.off()
    
  }
)

output$downloadeps_t_03 = downloadHandler(
  filename = function(){ paste(input$region_select_01,'_',input$selectyear01,'_monthwise','.eps',sep = '')},
  content = function(file) {
    postscript(file,
               width = 11.69 , height = 8.27, # inches
               horizontal = TRUE, onefile = TRUE, paper = "special")
    s <- event_data("plotly_click", source = "filt_total_barplot1")
    if (length(s) == 0) {
      NULL
    }   
    else {
      a1 <- subset (as.data.frame(filt_total_period()), NM %in%  s[["x"]])
      a1$PERIOD <-  as.character(a1$PERIOD)
      g = ggplot(data=a1, aes(x=PERIOD, y=kg, group = NM)) +
        geom_line(aes(color = NM ) ,
                  size = 0.5) +  
        geom_point(aes(color = NM ),
                   shape = 20,    
                   size = 4) +
        labs(title = paste("Quantity of", s[["x"]], "at each month over\n",input$region_select_01, "\nfor the year", input$selectyear01,"(in kg)"  ))+
        labs(y = "kg / Month", x = "Year / month (YYYYMM)")+
        theme(
          plot.title = element_text(size = 18, face = "bold"  , hjust = 0.5 ),
          axis.title.x = element_text(size = 16, face = "bold"   ),
          axis.title.y = element_text(size = 16, face = "bold"   ),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 16   ),
          axis.text.y = element_text(size = 16   ),
          legend.text = element_text(size = 16   ),
          legend.title = element_text(size = 16, face = "bold"   )
        )
      print(g)
      dev.off()
    }
  }
)

output$downloadeps_t_04 = downloadHandler(
  filename = function(){ paste(input$region_select_01,'_',input$selectyear01,'_monthwise','.eps',sep = '')},
  content = function(file) {
    postscript(file,
               width = 11.69 , height = 8.27, # inches
               horizontal = TRUE, onefile = TRUE, paper = "special")
    s <- event_data("plotly_click", source = "filt_total_barplot1")
    if (length(s) == 0) {
      NULL
    }   
    else {
      a1 <- subset (as.data.frame(filt_total_practice()),NM %in% s[["x"]])
      g = ggplot(data=a1, aes(x=PRACTICE, y=kg, group = NM)) +
        geom_line(aes(color = NM ) ,
                  size = 0.5) +  
        geom_point(aes(color = NM ),
                   shape = 20,    
                   size = 4) +
        labs(title = paste("Total quantity of", s[["x"]], "prescribed at each GP Practice over\n",input$region_select_01, "\nfor the year", input$selectyear01,"(in kg)"  ))+
        labs(y = "kg / GP Practice", x = "GP Practice code", color = "API")+
        theme(
          plot.title = element_text(size = 18, face = "bold"  , hjust = 0.5 ),
          axis.title.x = element_text(size = 16, face = "bold"   ),
          axis.title.y = element_text(size = 16, face = "bold"   ),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 16   ),
          axis.text.y = element_text(size = 16   ),
          legend.text = element_text(size = 16,   family= "Arial" ),
          legend.title = element_text(size = 16, face = "bold"   )
        )
      print(g)
      dev.off()
    }  
  }
)

output$downloadeps_t_05 = downloadHandler(
  filename = function(){ paste(input$region_select_01,'_',input$selectyear01,'_monthwise','.eps',sep = '')},
  content = function(file) {
    postscript(file,
               width = 11.69 , height = 8.27, # inches
               horizontal = TRUE, onefile = TRUE, paper = "special")
    s <- event_data("plotly_click", source = "filt_total_barplot1")
    if (length(s) == 0) {
      NULL
    }   
    else {
      a1 <- subset (as.data.frame(filt_total_postcode()),NM %in% s[["x"]])
      g = ggplot(data=a1, aes(x=postcode, y=kg, group = NM)) +
        geom_line(aes(color = NM ) ,
                  size = 0.5) +  
        geom_point(aes(color = NM ),
                   shape = 20,    
                   size = 4) +
        labs(title = paste("Total quantity of", s[["x"]],  "prescribed at each postcode over\n",input$region_select_01, "\nfor the year", input$selectyear01,"(in kg)"  ))+
        labs(y = "kg / postcode", x = "Postcode", color = "API")+
        theme(
          plot.title = element_text(size = 18, face = "bold"  , hjust = 0.5 ),
          axis.title.x = element_text(size = 16, face = "bold"   ),
          axis.title.y = element_text(size = 16, face = "bold"   ),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 16   ),
          axis.text.y = element_text(size = 16   ),
          legend.text = element_text(size = 16   ),
          legend.title = element_text(size = 16, face = "bold"   )
        )
      print(g)
      dev.off()
    } 
  }
)

output$downloadeps_t_06 = downloadHandler(
  filename = function(){ paste(input$region_select_01,'_',input$selectyear01,'_monthwise','.eps',sep = '')},
  content = function(file) {
    postscript(file,
               width = 11.69 , height = 8.27, # inches
               horizontal = TRUE, onefile = TRUE, paper = "special")
    s <- event_data("plotly_click", source = "filt_total_barplot1")
    if (length(s) == 0) {
      NULL
    }   
    else {
      a1 <- subset (as.data.frame(filt_total_dform()),NM %in% s[["x"]])
      g = ggplot(data=a1, aes(x=DESC, y=kg, group = NM)) +
        geom_line(aes(color = NM ) ,
                  size = 0.5) +  
        geom_point(aes(color = NM ),
                   shape = 20,    
                   size = 4) +
        labs(title = paste("Total quantity of", s[["x"]], "prescribed by each medicinal form over\n",input$region_select_01, "\nfor the year", input$selectyear01,"(in kg)"  ))+
        labs(y = "kg / medicinal form", x = "medicinal form", color = "API")+
        theme(
          plot.title = element_text(size = 18, face = "bold"  , hjust = 0.5 ),
          axis.title.x = element_text(size = 16, face = "bold"   ),
          axis.title.y = element_text(size = 16, face = "bold"   ),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 16   ),
          axis.text.y = element_text(size = 16   ),
          legend.text = element_text(size = 16   ),
          legend.title = element_text(size = 16, face = "bold"   )
        )
      print(g)
      dev.off()
    }
  }
)

# Download pdf
output$downloadpdf_t_01 = downloadHandler(
  filename = function(){ paste(input$region_select_01,'_',input$selectyear01,'_yearwise', '.pdf',sep = '')},
  content = function(file) {
    pdf(file, paper = "a4r",width = 14)
    g = ggplot(data=as.data.frame(filt_total_NM()), aes(x=NM, y=kg, fill=NM)) +
      geom_bar(stat="identity") +
      labs(title = paste("Total Compounds prescribed at",input$region_select_01, "\nover the year",input$selectyear01, "(in kg)") )+
      labs(x = "API", y = "kg", color = "API")+
      theme(
        plot.title = element_text(size = 18, face = "bold"  , hjust = 0.5 ),
        axis.title.x = element_text(size = 16, face = "bold"   ),
        axis.title.y = element_text(size = 16, face = "bold"   ),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 16   ),
        axis.text.y = element_text(size = 16   ),
        legend.text = element_text(size = 16   ),
        legend.title = element_text(size = 16, face = "bold"   )
      )
    print(g)
    dev.off()
    
  }
)

output$downloadpdf_t_02 = downloadHandler(
  filename = function(){ paste(input$region_select_01,'_',input$selectyear01,'_monthwise','.pdf',sep = '')},
  content = function(file) {
    pdf(file, paper = "a4r",width = 14)
    a1 <- as.data.frame(filt_total_period())
    a1$PERIOD <-  as.character(a1$PERIOD)
    g = ggplot(data=a1, aes(x=PERIOD, y=kg, group = NM)) +
      geom_line(aes(color = NM ) ,
                size = 0.5) +  
      geom_point(aes(color = NM ),
                 shape = 20,    
                 size = 4) +
      labs(title = paste("Total qcompounds prescribed at each month over",input$region_select_01, "\nfor the year", input$selectyear01,"(in kg)"  ))+
      labs(y = "kg / Month", x = "Year / month (YYYYMM)", color = "API")+
      theme(
        plot.title = element_text(size = 18, face = "bold"  , hjust = 0.5 ),
        axis.title.x = element_text(size = 16, face = "bold"   ),
        axis.title.y = element_text(size = 16, face = "bold"   ),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 16   ),
        axis.text.y = element_text(size = 16   ),
        legend.text = element_text(size = 16   ),
        legend.title = element_text(size = 16, face = "bold"   )
      )
    print(g)
    dev.off()
    
  }
)

output$downloadpdf_t_03 = downloadHandler(
  filename = function(){ paste(input$region_select_01,'_',input$selectyear01,'_monthwise','.eps',sep = '')},
  content = function(file) {
    pdf(file, paper = "a4r",width = 14)
    s <- event_data("plotly_click", source = "filt_total_barplot1")
    if (length(s) == 0) {
      NULL
    }   
    else {
      a1 <- subset (as.data.frame(filt_total_period()), NM %in%  s[["x"]])
      a1$PERIOD <-  as.character(a1$PERIOD)
      g = ggplot(data=a1, aes(x=PERIOD, y=kg, group = NM)) +
        geom_line(aes(color = NM ) ,
                  size = 0.5) +  
        geom_point(aes(color = NM ),
                   shape = 20,    
                   size = 4) +
        labs(title = paste("Quantity of", s[["x"]], "at each month over\n",input$region_select_01, "\nfor the year", input$selectyear01,"(in kg)"  ))+
        labs(y = "kg / Month", x = "Year / month (YYYYMM)")+
        theme(
          plot.title = element_text(size = 18, face = "bold"  , hjust = 0.5 ),
          axis.title.x = element_text(size = 16, face = "bold"   ),
          axis.title.y = element_text(size = 16, face = "bold"   ),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 16   ),
          axis.text.y = element_text(size = 16   ),
          legend.text = element_text(size = 16   ),
          legend.title = element_text(size = 16, face = "bold"   )
        )
      print(g)
      dev.off()
    }
  }
)

output$downloadpdf_t_04 = downloadHandler(
  filename = function(){ paste(input$region_select_01,'_',input$selectyear01,'_monthwise','.eps',sep = '')},
  content = function(file) {
    pdf(file, paper = "a4r",width = 14)
    s <- event_data("plotly_click", source = "filt_total_barplot1")
    if (length(s) == 0) {
      NULL
    }   
    else {
      a1 <- subset (as.data.frame(filt_total_practice()),NM %in% s[["x"]])
      g = ggplot(data=a1, aes(x=PRACTICE, y=kg, group = NM)) +
        geom_line(aes(color = NM ) ,
                  size = 0.5) +  
        geom_point(aes(color = NM ),
                   shape = 20,    
                   size = 4) +
        labs(title = paste("Total quantity of", s[["x"]], "prescribed at each GP Practice over\n",input$region_select_01, "\nfor the year", input$selectyear01,"(in kg)"  ))+
        labs(y = "kg / GP Practice", x = "GP Practice code", color = "API")+
        theme(
          plot.title = element_text(size = 18, face = "bold"  , hjust = 0.5 ),
          axis.title.x = element_text(size = 16, face = "bold"   ),
          axis.title.y = element_text(size = 16, face = "bold"   ),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 16   ),
          axis.text.y = element_text(size = 16   ),
          legend.text = element_text(size = 16   ),
          legend.title = element_text(size = 16, face = "bold"   )
        )
      print(g)
      dev.off()
    }  
  }
)

output$downloadpdf_t_05 = downloadHandler(
  filename = function(){ paste(input$region_select_01,'_',input$selectyear01,'_monthwise','.eps',sep = '')},
  content = function(file) {
    pdf(file, paper = "a4r",width = 14)
    s <- event_data("plotly_click", source = "filt_total_barplot1")
    if (length(s) == 0) {
      NULL
    }   
    else {
      a1 <- subset (as.data.frame(filt_total_postcode()),NM %in% s[["x"]])
      g = ggplot(data=a1, aes(x=postcode, y=kg, group = NM)) +
        geom_line(aes(color = NM ) ,
                  size = 0.5) +  
        geom_point(aes(color = NM ),
                   shape = 20,    
                   size = 4) +
        labs(title = paste("Total quantity of", s[["x"]],  "prescribed at each postcode over\n",input$region_select_01, "\nfor the year", input$selectyear01,"(in kg)"  ))+
        labs(y = "kg / postcode", x = "Postcode", color = "API")+
        theme(
          plot.title = element_text(size = 18, face = "bold"  , hjust = 0.5 ),
          axis.title.x = element_text(size = 16, face = "bold"   ),
          axis.title.y = element_text(size = 16, face = "bold"   ),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 16   ),
          axis.text.y = element_text(size = 16   ),
          legend.text = element_text(size = 16   ),
          legend.title = element_text(size = 16, face = "bold"   )
        )
      print(g)
      dev.off()
    } 
  }
)

output$downloadpdf_t_06 = downloadHandler(
  filename = function(){ paste(input$region_select_01,'_',input$selectyear01,'_monthwise','.eps',sep = '')},
  content = function(file) {
    pdf(file, paper = "a4r",width = 14)
    s <- event_data("plotly_click", source = "filt_total_barplot1")
    if (length(s) == 0) {
      NULL
    }   
    else {
      a1 <- subset (as.data.frame(filt_total_dform()),NM %in% s[["x"]])
      g = ggplot(data=a1, aes(x=DESC, y=kg, group = NM)) +
        geom_line(aes(color = NM ) ,
                  size = 0.5) +  
        geom_point(aes(color = NM ),
                   shape = 20,    
                   size = 4) +
        labs(title = paste("Total quantity of", s[["x"]], "prescribed by each medicinal form over\n",input$region_select_01, "\nfor the year", input$selectyear01,"(in kg)"  ))+
        labs(y = "kg / medicinal form", x = "medicinal form", color = "API")+
        theme(
          plot.title = element_text(size = 18, face = "bold"  , hjust = 0.5 ),
          axis.title.x = element_text(size = 16, face = "bold"   ),
          axis.title.y = element_text(size = 16, face = "bold"   ),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 16   ),
          axis.text.y = element_text(size = 16   ),
          legend.text = element_text(size = 16 ),
          legend.title = element_text(size = 16, face = "bold"   )
        )
      print(g)
      dev.off()
    }
  }
)

########