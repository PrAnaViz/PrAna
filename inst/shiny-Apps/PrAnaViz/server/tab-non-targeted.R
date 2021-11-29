## Server
# ui for file input 01
output$uifile_shape <- renderUI({
  tipify(fileInput('file_shape', 'Upload shape files at once (.shp, .dbf, .shx and .prj):',
                   accept=c('.shp','.dbf','.sbn','.sbx','.shx',".prj"), 
                   multiple=TRUE,
                   placeholder = "Upload shape files (.shp, .dbf, .shx and .prj)"
  ),
  "Click here to upload your shape files")
  
})


output$regioninput2 <- renderUI({
  withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
    selectInput(inputId="region_select_02",
                label="Select region:",
                choices=map()$region_name,
                selected ="NHS Bath and North East Somerset CCG"
    )
  })
})

# Leaflet color function
getColor <- function(data02) {
  sapply(data02$kg, function(kg) {
    if(kg < 0.1) {
      "green"
    } 
    else if(kg < 0.4) {
      "orange"
    } 
    else {
      "red"
    } })
}

gp_postcode_full <-  reactive ({ 
  gp_data_full_latnlong() %>%
    dplyr::filter (Year %in% !!input$selectyear02)  %>%
    dplyr::select(PERIOD, PRACTICE, SURGERY_NAME, postcode, latitude, longitude) %>%
    dplyr::distinct()
})

postcode_api <- reactive({
  bind_practices () %>%
    dplyr::mutate(gram2 = as.numeric(gram)) %>%
    dplyr::filter (!grepl("Error: Table",CPD)) %>%
    dplyr::filter ( tolower(NM) %in% !!input$selected_api) %>%
    dplyr::left_join(gp_postcode_full(), by=c("PRACTICE"="PRACTICE", "PERIOD" = "PERIOD")) %>%
    dplyr::group_by(SURGERY_NAME, NM, PERIOD, PRACTICE, postcode, latitude, longitude) %>%
    dplyr::summarise(gram_sum = sum(gram2, na.rm = T)) %>%
    dplyr::mutate(kg = gram_sum/1000) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(PERIOD02 =  zoo::as.Date(zoo::as.yearmon(as.character(PERIOD),"%Y%m"))) %>%
    dplyr::mutate(period = format(PERIOD02, format ="%B %Y")) %>%
    dplyr::mutate(MONYear = format(PERIOD02, format ="%b %Y")) %>%
    separate(period, c("month", "Year"),sep = " ") 
  
})


## Barplot

## Data for barplot

data_for_plot <- reactive({
  postcode_api() %>%
    dplyr::select(postcode, PRACTICE, NM, SURGERY_NAME, PERIOD, Year, month, gram_sum ) %>% 
    dplyr::filter ( month %in% !!input$sel_month) %>%
    dplyr::group_by(postcode, NM, PERIOD, month) %>%
    dplyr::summarise(gram_sum = sum(gram_sum, na.rm = T)) %>%
    dplyr::mutate(kg = gram_sum/1000) %>%
    dplyr::ungroup() 
})



# barplot
output$filt_barplot1_nt <- renderPlotly({
  withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
    plot01 <- plot_ly(as.data.frame(data_for_plot()),
                      x = ~postcode, y = ~kg,  type = "bar",
                      color = ~postcode,
                      colors = color_palette(),
                      source = "filt_barplot1_nt") %>%
      layout(title = FALSE,
             paper_bgcolor = "transparent", plot_bgcolor = "transparent",
             font = title_font,
             xaxis = list(title = "Post code"),
             yaxis = list(title = "kg"))
    
    if (input$dark_mode) plot01 <- plot01 %>%
        layout(
          xaxis = list(
            color = "white"),
          yaxis = list(
            color = "white"),
          legend = list(
            font = list(
              color = "white")
          )
        )
    else
      plot01
  })
})

## End of Barplot


##lineplot 02
output$filt_gp_lineplot2 <- renderPlotly({ 
  s <- event_data("plotly_click", source = "filt_barplot1_nt")
  
  if (length(s) == 0) {
    NULL
  }   
  else {
    
    dat_lineplotgp2 <- reactive ({ 
      postcode_api() %>%
        filter (postcode %in% s[[3]] )
    })
    
    a1 <- as.data.frame(dat_lineplotgp2())
    
    a1$PERIOD <-  as.character(a1$PERIOD)
    
    a2 <-  plot_ly(a1, x = ~PERIOD, 
                   y = ~kg, 
                   color = ~SURGERY_NAME,
                   colors = color_palette()) %>%
      add_markers(showlegend = TRUE) %>%
      add_lines(showlegend = FALSE) %>%
      layout(
        paper_bgcolor = "transparent", plot_bgcolor = "transparent",
        title =  FALSE,
        autosize = TRUE,
        showlegend = TRUE,
        xaxis = list(title = "Year / Month"),
        yaxis = list (title = "kg / month"),
        legend = list(orientation = "h",
                      x = 0.3, 
                      y = -0.3)
      )
    if (input$dark_mode) a2 <- a2 %>%  
      layout(
        xaxis = list(
          color = "white"),
        yaxis = list(
          color = "white"),
        legend = list(
          font = list(
            color = "white")
        ) 
      )
    else
      a2
  }
})



## End of line plot 02
## Leaflet
## Base leaflet map
output$postcodemap <- renderLeaflet({
  withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
    leaflet() %>%
      addTiles() %>%
      setView(-2.3590, 51.3811, zoom = 10) %>%
      addProviderTiles("OpenStreetMap", group = "Street map") %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
      addMiniMap(position = "bottomright") %>%
      addLayersControl(
        position = "topright",
        baseGroups = c("Street map","Satellite"),
        overlayGroups = c("CCG")
      )
  })
})




observeEvent(input$gen_leaflet01, {
  postcode_api_month <- reactive ({
    postcode_api() %>%
      filter (month %in% !!input$sel_month)
  })
  # # leaflet icons
  icons <- reactive({ awesomeIcons(
    icon = 'plus-sign',
    iconColor = 'white',
    library = 'glyphicon',
    markerColor = getColor(postcode_api_month())
  )})
  
  leafletProxy("postcodemap") %>%
    addAwesomeMarkers (
      data = postcode_api_month(),
      lng = ~longitude,
      lat = ~latitude,
      layerId = ~postcode,
      group = paste(input$selected_api),
      icon=icons(),
      label=~ paste0('click to view'),
      popup = paste(
        "<center>","<b>",input$selected_api,"</b>","</center>",
        "<br>",
        "Postcode:",  postcode_api_month()$postcode,
        "<br>",
        "Prescribed quantity (kg):",postcode_api_month()$kg
      )
    ) %>%
    addLegend("bottomright", 
              colors =c("green",  "orange", "red"),
              labels= c("kg < 0.1", "kg < 0.4","kg > 0.4"),
              title= "Quantity (kg/month)",
              opacity = 1)
  
})


# For lineplot
observeEvent(input$postcodemap_marker_click, {
  
  dat_lineplotgp <- reactive ({ 
    postcode_api() %>%
      filter (postcode %in% !!input$postcodemap_marker_click$id) 
  })
  
  # Plotly lineplot 01 - Period
  output$filt_gp_lineplot1 <- renderPlotly({ 
    withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
      a1 <- as.data.frame(dat_lineplotgp())
      a1$PERIOD <-  as.character(a1$PERIOD)
      a2 <-  plot_ly(a1, x = ~PERIOD, 
                     y = ~kg, 
                     color = ~SURGERY_NAME,
                     colors = color_palette()) %>%
        add_markers(showlegend = TRUE) %>%
        add_lines(showlegend = FALSE) %>%
        layout(
          paper_bgcolor = "transparent", plot_bgcolor = "transparent",
          title =  FALSE,
          autosize = TRUE,
          showlegend = TRUE,
          xaxis = list(title = "Year / Month"),
          yaxis = list (title = "kg / month"),
          legend = list(orientation = "h",
                        x = 0.3, 
                        y = -0.3)
        )
      if (input$dark_mode) a2 <- a2 %>%  
        layout(
          xaxis = list(
            color = "white"),
          yaxis = list(
            color = "white"),
          legend = list(
            font = list(
              color = "white")
          ) 
        )
      else
        a2
      
    })
  })
  
  
  # Download eps 01
  output$downloadeps_nt_01 <- downloadHandler(
    filename = function(){ paste(input$postcodemap_marker_click$id,'_',input$selected_api,'_',input$selectyear02, '.eps',sep = '')},
    content = function(file) {
      postscript(file,
                 width = 11.69 , height = 8.27, # inches
                 horizontal = TRUE, onefile = TRUE, paper = "special")
      a1 <- as.data.frame(dat_lineplotgp())
      a1$PERIOD <-  as.character(a1$PERIOD)
      g = ggplot(data=a1, aes(x=PERIOD, y=kg,group = SURGERY_NAME)) +
        geom_line(aes(color = SURGERY_NAME ) ,
                  size = 0.5) +  
        geom_point(aes(color = SURGERY_NAME ),
                   shape = 20,    
                   size = 4) +
        scale_color_manual(values = color_palette()) +
        labs(title = paste("Quantity of",input$selected_api,"prescribed at",input$postcodemap_marker_click$id, "monthwise for the year", input$selectyear02,"(in kg)"))+
        labs(x = "Year / Month", y = "kg / month")+
        theme_bw()+
        theme(
          plot.title = element_text(size = 16, face = "bold"  , hjust = 0.5 ),
          axis.title.x = element_text(size = 14, face = "bold"   ),
          axis.title.y = element_text(size = 14, face = "bold"   ),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 12  ),
          axis.text.y = element_text(size = 12 ),
          legend.text = element_text(size = 8 ),
          legend.title = element_text(size = 10, face = "bold"   )
        )
      print(g)
      dev.off()
      
    }
  )
  
  # Download pdf 01
  output$downloadpdf_nt_01 <- downloadHandler(
    filename = function(){ paste(input$postcodemap_marker_click$id,'_',input$selected_api,'_',input$selectyear02, '.pdf',sep = '')},
    content = function(file) {
      pdf(file, paper = "a4r",width = 14)
      a1 <- as.data.frame(dat_lineplotgp())
      a1$PERIOD <-  as.character(a1$PERIOD)
      g <- ggplot(data=a1, aes(x=PERIOD, y=kg,group = SURGERY_NAME)) +
        geom_line(aes(color = SURGERY_NAME ) ,
                  size = 0.5) +  
        geom_point(aes(color = SURGERY_NAME ),
                   shape = 20,    
                   size = 2) +
        scale_color_manual(values = color_palette()) +
        
        labs(title = paste("Quantity of",input$selected_api,"prescribed at",input$postcodemap_marker_click$id, "monthwise for the year", input$selectyear02,"(in kg)"))+
        labs(x = "Year / Month", y = "kg / month")+
        theme_bw()+
        theme(
          plot.title = element_text(size = 16, face = "bold"  , hjust = 0.5 ),
          axis.title.x = element_text(size = 14, face = "bold"   ),
          axis.title.y = element_text(size = 14, face = "bold"   ),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 12  ),
          axis.text.y = element_text(size = 12 ),
          legend.text = element_text(size = 8 ),
          legend.title = element_text(size = 10, face = "bold"   )
        )
      print(g)
      dev.off()
    }
  )
  
})

# Total by postcode
tot_postcode <- reactive({
  bind_practices () %>%
    dplyr::filter (!grepl("Error: Table",CPD)) %>%
    dplyr::mutate(NM = tolower(NM) ) %>% 
    dplyr::filter (tolower(NM) %in% !!input$selected_api) %>%
    dplyr::mutate(Year=(substr(PERIOD, 1, 4))) %>%
    dplyr::mutate(gram2 = as.numeric(gram)) %>%
    dplyr::left_join(gp_postcode_full(), by=c("PRACTICE"="PRACTICE", "PERIOD" = "PERIOD")) %>%
    dplyr::group_by(NM, Year, postcode) %>%
    dplyr::summarise(gram_sum = sum(gram2, na.rm = T)) %>%
    dplyr::mutate(kg = gram_sum/1000) 
  
})

## Observe event
observeEvent(input$postcodemap_marker_click, {
  click2 <- input$postcodemap_marker_click
  
  if(is.null(click2))
    return()
  text11 <- tot_postcode()[grepl(click2$id,tot_postcode()$postcode),] 
  ## Text outputs
  output$txt_leaflet <- renderUI({ 
    HTML( paste("Total quantity of ",input$selected_api, " prescribed at ",input$postcodemap_marker_click$id, 
                "in the year ",input$selectyear02, " :" , text11$kg, " (kg)" ) 
    ) 
  })
  
})


output$txt_barplot_nt <- renderUI({ 
  s <- event_data("plotly_click", source = "filt_barplot1_nt")
  if (length(s) == 0) {
    NULL
  }   
  else {
    HTML( paste0(
      "Total quantity of ",input$selected_api, " prescribed at ",s[[3]], 
      " in ",input$sel_month, " :" ,s[[4]] , " (kg)" 
    ) 
    )
  }
})


output$txt_line_gp_title2 <- renderUI({ 
  s <- event_data("plotly_click", source = "filt_barplot1_nt")
  if (length(s) == 0) {
    NULL
  }   
  else {
    
    HTML( paste("Quantity of ",input$selected_api,
                "prescribed at<br/>",s[[3]], "monthwise for<br/>the year ",input$sel_month, 
                "(in kg)") ) 
  }
})


# Data for download csv
# leaflet data monthwise
downheatmap_period <- reactive ({
  postcode_api() %>%
    dplyr::select(postcode,NM, PERIOD, kg) %>%
    pivot_wider(names_from = PERIOD, values_from = kg, values_fill = 0  )
})

#leaflet data yearwise
downheatmap_year <- reactive ({
  dcast(as.data.frame(postcode_api()), postcode+NM~Year,value.var= "kg",fun.aggregate = sum) 
})

# for each postcode - monthwise with GP
lineplot_postcode <- reactive ({
  dcast(as.data.frame(postcode_api()), postcode+SURGERY_NAME+ NM ~ PERIOD,value.var= "kg") %>%
    dplyr::filter (postcode %in% !!input$postcodemap_marker_click$id)
})

### Outputs

## UI
# ui for api select
output$selected_api_input <- renderUI({
  withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
    selectInput(inputId="selected_api",
                label="Select BNF Chemical Substance:",
                choices=unique(bnf_group()$BNF_CHEMICAL_SUBSTANCE),
                selected = "metformin hydrochloride",  multiple = FALSE
    )
  })
})

# ui for download 01
output$uidownload_nt_01  <- renderUI({
  withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
    tags$span(
      downloadButton('downloadcsv_nt_01', 'Download CSV (monthwise)'),
      downloadButton('downloadcsv_nt_02', 'Download CSV (year)')
    )
  })
})

# ui for download 02
output$uidownload_nt_02 <- renderUI({
  withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
    tags$span(
      downloadButton('downloadcsv_nt_03', 'Download CSV'),
      downloadButton('downloadeps_nt_01', 'Download EPS'),
      downloadButton('downloadpdf_nt_01', 'Download PDF'),
    )
  })
})

## Text outputs

output$txt_line_gp_title1 <- renderUI({ HTML( paste("Quantity of ",input$selected_api,"prescribed at<br/>",input$postcodemap_marker_click$id, "monthwise for<br/>the year ", input$selectyear02,"(in kg)") ) })

output$txt_gp_pie_title <- renderUI({ HTML( paste("Total quantity of ",input$selected_api, "<br/>prescribed at ",input$postcodemap_marker_click$id, "in the<br/>year ",input$selectyear02, " (Group by medicinal form)") ) })


## Download Buttons


# Download csv 01
output$downloadcsv_nt_01 <- downloadHandler(
  filename = function (){ paste(input$selected_api,'_',input$region_select_02,'_',input$selectyear02,'_month_',input$sel_month,'.csv',sep = '')},
  content = function(file) {
    write.csv(as.data.frame( downheatmap_period()), file, row.names = TRUE)
  }
)

# Download csv 02
output$downloadcsv_nt_02 <- downloadHandler(
  filename = function (){ paste(input$selected_api,'_',input$region_select_02,'_',input$selectyear02,'.csv',sep = '')},
  content = function(file) {
    write.csv(as.data.frame( downheatmap_year()), file, row.names = TRUE)
  }
)

# Download csv 03
output$downloadcsv_nt_03 <- downloadHandler(
  filename = function(){ paste(input$postcodemap_marker_click$id,'_',input$selected_api,'_',input$selectyear02, '.csv',sep = '')},
  content = function(file) {
    write.csv(as.data.frame( lineplot_postcode()), file, row.names = TRUE)
  }
)





###########
