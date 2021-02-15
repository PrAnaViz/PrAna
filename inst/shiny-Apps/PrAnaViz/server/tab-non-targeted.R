## Shape file

# ui for file input 01
output$uifile_shape <- renderUI({
  tipify(fileInput('file_shape', 'Upload shape files at once (.shp, .dbf, .shx and .prj):',
                   accept=c('.shp','.dbf','.sbn','.sbx','.shx',".prj"), 
                   multiple=TRUE,
                   placeholder = "Upload shape files (.shp, .dbf, .shx and .prj)"
  ),
  "Click here to upload your shape files")
  
})

map <- reactive({
  shpdf <- input$file_shape
  if(is.null(shpdf)){
    return()
  }
  previouswd <- getwd()
  uploaddirectory <- dirname(shpdf$datapath[1])
  setwd(uploaddirectory)
  for(i in 1:nrow(shpdf)){
    file.rename(shpdf$datapath[i], shpdf$name[i])
  }
  setwd(previouswd)
  map <- sf::st_read(paste(uploaddirectory, shpdf$name[grep(pattern="*.shp$", shpdf$name)], sep="/"), quiet = TRUE ) %>%
    rename(region_name = 3)
  map
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
    filter (Year %in% !!input$selectyear01)  %>%
    dplyr::select(PERIOD, PRACTICE, SURGERY_NAME, postcode, latitude, longitude) %>%
    distinct()
})

# Add zero before the month
month_sel <- reactive ({
  if (input$sel_month >= 10) {
    input$sel_month
  }
  else if (input$sel_month < 10) {
    paste("0",input$sel_month, sep="" )
  }
})

postcode_api <- reactive({
  bind_practices () %>%
    mutate(Year=(substr(PERIOD, 1, 4)),gram2 = as.numeric(gram)) %>%
    filter (!grepl("Error: Table",CPD)) %>%
    dplyr::filter ( tolower(NM) %in% !!input$selected_api) %>%
    left_join(gp_postcode_full(), by=c("PRACTICE"="PRACTICE", "PERIOD" = "PERIOD")) %>%
    group_by(SURGERY_NAME, NM, Year, PERIOD, PRACTICE, postcode, latitude, longitude) %>%
    summarise(gram_sum = sum(gram2, na.rm = T)) %>%
    mutate(kg = gram_sum/1000) %>%
    mutate(month=(substr(PERIOD, 5, 6))) 
})

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
      filter (month %in% month_sel())
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
  )
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
    a2 <-  plot_ly(a1, x = ~PERIOD, y = ~kg, color = ~SURGERY_NAME) %>%
      add_markers(showlegend = TRUE) %>%
      add_lines(showlegend = FALSE) %>%
      layout(
        title =  F,
        autosize = T,
        showlegend = TRUE,
        xaxis = list(title = "Year / Month"),
        yaxis = list (title = "kg / month"),
        legend = list(orientation = "h",
                      x = 0.3, 
                      y = -0.3)
      )
    a2
    
  })
})
})

# Total by postcode
tot_postcode <- reactive({
  bind_practices () %>%
    filter (!grepl("Error: Table",CPD)) %>%
    dplyr::mutate(NM = tolower(NM) ) %>% 
    filter (tolower(NM) %in% !!input$selected_api) %>%
    mutate(Year=(substr(PERIOD, 1, 4))) %>%
    mutate(gram2 = as.numeric(gram)) %>%
    left_join(gp_postcode_full(), by=c("PRACTICE"="PRACTICE", "PERIOD" = "PERIOD")) %>%
    group_by(NM, Year, postcode) %>%
    summarise(gram_sum = sum(gram2, na.rm = T)) %>%
    mutate(kg = gram_sum/1000) 
   
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
                "in the year ",input$selectyear01, " :" , text11$kg, " (kg)" ) 
    ) 
  })
  
})

# Data for download csv
# leaflet data monthwise
downheatmap_period <- reactive ({
  dcast(as.data.frame(postcode_api()), postcode+NM ~ PERIOD,value.var= "kg") 
})

#leaflet data yearwise
downheatmap_year <- reactive ({
  dcast(as.data.frame(postcode_api()), postcode+NM~Year,value.var= "kg",fun.aggregate = sum) 
})

# for each postcode - monthwise with GP
lineplot_postcode <- reactive ({
  dcast(as.data.frame(postcode_api()), postcode+SURGERY_NAME+ NM ~ PERIOD,value.var= "kg") %>%
    filter (postcode %in% !!input$postcodemap_marker_click$id)
})

### Outputs

## UI
# ui for api select
output$selected_api_input <- renderUI({
  withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
    selectizeInput(inputId="selected_api",
                   label="Select BNF Chemical Substance:",
                   choices=unique(bnf_group()$BNF_CHEMICAL_SUBSTANCE),selected =head(unique(bnf_group()$BNF_CHEMICAL_SUBSTANCE)),  multiple = FALSE
    )
  })
})

# ui for download 01
output$uidownload_nt_01  <- renderUI({
  withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
    tags$span(
      downloadButton('downloadhtml_nt_01', 'Download Map'),
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

output$txt_line_gp_title <- renderUI({ HTML( paste("Quantity of ",input$selected_api,"prescribed at<br/>",input$postcodemap_marker_click$id, "monthwise for<br/>the year ", input$selectyear01,"(in kg)") ) })

output$txt_gp_pie_title <- renderUI({ HTML( paste("Total quantity of ",input$selected_api, "<br/>prescribed at ",input$postcodemap_marker_click$id, "in the<br/>year ",input$selectyear01, " (Group by medicinal form)") ) })

## Download Buttons
# Download html 01
output$downloadhtml_nt_01 <- downloadHandler(
  filename = function (){ paste(input$selected_api,'_',input$region_select_01,'_',input$selectyear01,'_month_',input$sel_month,'.html',sep = '')},
  content = function(file){
    saveWidget(
      widget = postcodemap()
      , file = file
    )
  }
)

# Download csv 01
output$downloadcsv_nt_01 <- downloadHandler(
  filename = function (){ paste(input$selected_api,'_',input$region_select_01,'_',input$selectyear01,'_month_',input$sel_month,'.csv',sep = '')},
  content = function(file) {
    write.csv(as.data.frame( downheatmap_period()), file, row.names = TRUE)
  }
)

# Download csv 02
output$downloadcsv_nt_02 <- downloadHandler(
  filename = function (){ paste(input$selected_api,'_',input$region_select_01,'_',input$selectyear01,'.csv',sep = '')},
  content = function(file) {
    write.csv(as.data.frame( downheatmap_year()), file, row.names = TRUE)
  }
)

# Download csv 03
output$downloadcsv_nt_03 <- downloadHandler(
  filename = function(){ paste(input$postcodemap_marker_click$id,'_',input$selected_api,'_',input$selectyear01, '.csv',sep = '')},
  content = function(file) {
    write.csv(as.data.frame( lineplot_postcode()), file, row.names = TRUE)
  }
)

# Download eps 01
output$downloadeps_nt_01 <- downloadHandler(
  filename = function(){ paste(input$postcodemap_marker_click$id,'_',input$selected_api,'_',input$selectyear01, '.eps',sep = '')},
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
      labs(title = paste("Quantity of",input$selected_api,"prescribed at",input$postcodemap_marker_click$id, "monthwise for the year", input$selectyear01,"(in kg)"))+
      labs(x = "Year / Month", y = "kg / month")+
      theme(
        axis.title.x = element_text(size = 16, face = "bold", family= "Arial" ),
        axis.title.y = element_text(size = 16, face = "bold", family= "Arial" ),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    print(g)
    dev.off()
    
  }
)

# Download pdf 01
output$downloadpdf_nt_01 <- downloadHandler(
  filename = function(){ paste(input$postcodemap_marker_click$id,'_',input$selected_api,'_',input$selectyear01, '.pdf',sep = '')},
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
      labs(title = paste("Quantity of",input$selected_api,"prescribed at",input$postcodemap_marker_click$id, "monthwise for the year", input$selectyear01,"(in kg)"))+
      labs(x = "Year / Month", y = "kg / month")+
      theme(
        axis.title.x = element_text(size = 16, face = "bold", family= "Arial" ),
        axis.title.y = element_text(size = 16, face = "bold", family= "Arial" ),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    print(g)
    dev.off()
  }
)



###########
