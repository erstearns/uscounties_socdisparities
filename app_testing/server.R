
############################################
# code author: erin stearns
# script objective: us counties geodisparities shiny mock up server script
# date: 25 april 2019
###########################################

######################################################################################################
# -------------------------------------- server function ------------------------------------------- #
######################################################################################################
server <- function(input, output, session) {
  
  
  ########################################################################################################
  # -------------------------------------  DATA EXPLORER -------------------------------------------------
  ########################################################################################################
  
  # Creating base leaflet map ---------------------------------------------------------------------------
  # -- base map --
  # ->- conductor - parent of: map endpoint
  #               - child of: 
  output$map <- renderLeaflet({
    print('render map')
    leaflet() %>% #addTiles() %>% 
      addProviderTiles("Esri.OceanBasemap", group = "Esri.OceanBasemap") %>%
      addProviderTiles("OpenStreetMap.Mapnik", group = "OpenStreetmap") %>%
      addProviderTiles("Esri.WorldImagery", group = "Esri.WorldImagery") %>%
      addLayersControl(baseGroups = c("OpenStreetmap","Esri.OceanBasemap", 'Esri.WorldImagery'),
                       options = layersControlOptions(collapsed = TRUE, autoZIndex = F)) %>%
      fitBounds(-124.848974, 24.396308, -66.885444, 49.384358) %>% #manually input us centroid to center view
      addScaleBar(position = "bottomleft") %>%
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to Level 1",
        onClick=JS("function(btn, map){ map.setZoom(1); }")))
  })
  
  # Subsetting data ----------------------------------------------------------------------------------
  
  # -- making data into a reactive object
  df <- geodata
  makeReactiveBinding('df')
  
  # -- updating data geography if differs from previous selection -- 
  # ->- conductor - parent of: map endpoint, observeEvent(input$state)
  #               - child of: state input
  # subsetting to selected data using filters -- aspatial for graphs
  asp_selected_data <- reactive({
    print('reactive: subset df to selected inputs')
    if (input$state == 'All'){
      adata %>%
        filter(
          year %in% input$year
        )
    } else {
      adata %>% 
        filter(
          state_name %in% input$state,
          year %in% input$year
        )
    }
    
  })
  
  # -- updating data geography if differs from previous selection -- 
  # ->- conductor - parent of: map endpoint, observeEvent(input$state)
  #               - child of: state input
  # subsetting to selected data using filters
  selected_data <- reactive({
    print('reactive: subset df to selected inputs')
    if (input$state == 'All'){
      geodata %>%
        filter(
          year %in% input$year
        )
    } else {
      geodata %>% 
        filter(
          state_name %in% input$state,
          year %in% input$year
        )
    }
    
  })
  
  #trigger subsetting
  # >- endpoint - child of: state_selected()
  observeEvent(c(input$state,input$year),{
    print('observeEvent: updating df and clearing map features')
    leafletProxy('map') %>%
      clearShapes()
    df <<- selected_data()
    
    print(paste0("df data class is: ", class(df)))
    
  })
  
  # update map bounding box
  # ->- conductor - parent of: map endpoint
  #               - child of: 
  coords <- reactive({
    print('reactive: coords')
    print(paste0('calculating bbox - df is: ', class(df)))
    if(input$state == 'All'){
      leafletProxy('map') %>% 
        fitBounds(-124.848974, 24.396308, -66.885444, 49.384358)
    } else {
      bbox <- st_bbox(selected_data())
      print
      leafletProxy('map') %>% 
        fitBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]])
    }
    
  })
  
  #adjust mapview based on state selected
  observeEvent(input$state, {
    coords()
  })
  
  #for adjusting the map view, could find centroid of subset shape and use 'setView' and those coords
  
  # Updating xvar, yvar & color var input options ---------------------------------------------------
  
  # dependent on subsetting of df above
  output$xvar <- renderUI(selectInput('xvar',label='x Var',
                                      choices = names(df)[!names(df) %in% c('GEOID','state_name', 'state_code', 'year', 'source', 'NAME', 'variable', 'estimate', 'moe', 'geometry')],
                                      selected =  names(df)[!names(df) %in% c('GEOID','state_name', 'state_code', 'year', 'source', 'NAME', 'variable', 'estimate', 'moe', 'geometry')][1]))
  output$yvar <- renderUI(selectInput('yvar',label='y Var',
                                      choices = names(df)[!names(df) %in% c('GEOID','state_name', 'state_code', 'year', 'source', 'NAME', 'variable', 'estimate', 'moe', 'geometry')],
                                      selected =  names(df)[!names(df) %in% c('GEOID','state_name', 'state_code', 'year', 'source', 'NAME', 'variable', 'estimate', 'moe', 'geometry')][2]))
  output$cvar <- renderUI(selectInput('color',label='Color Var',
                                      choices = names(df)[!names(df) %in% c('GEOID','state_name', 'state_code', 'year', 'source', 'NAME', 'variable', 'estimate', 'moe', 'geometry')],
                                      selected =  names(df)[!names(df) %in% c('GEOID','state_name', 'state_code', 'year', 'source', 'NAME', 'variable', 'estimate', 'moe', 'geometry')][1]))
  
  # Defining xvar, yvar & color var reactives -------------------------------------------------------
  xvar_ <- ''
  xVar <- reactive({
    print('reactive: xVar')
    if(is.null(input$xvar)) return(names(df)[!names(df) %in% c('GEOID','state_name', 'state_code', 'year', 'source', 'NAME', 'variable', 'estimate', 'moe', 'geometry')][1])
    xvar_ <<- input$xvar
    input$xvar})
  yVar <- reactive({
    print('reactive: yVar')
    if(is.null(input$yvar)) return(names(df)[!names(df) %in% c('GEOID','state_name', 'state_code', 'year', 'source', 'NAME', 'variable', 'estimate', 'moe', 'geometry')][2])
    input$yvar})
  colorVar <- reactive({
    print('reactive: colVar')
    if(is.null(input$color)) 
      return(names(df)[!names(df) %in% c('GEOID','state_name', 'state_code', 'year', 'source', 'NAME', 'variable', 'estimate', 'moe', 'geometry')][1])
    input$color
  })
  
  # create charts & circle size options ------------------------------------------------------------
  #bivariate scatter plot 
  output$biscatter <- renderPlot({
    print('plotting bivariate scatter plot')
    
    ggplot(asp_selected_data(),aes_string(input$xvar, input$yvar, colour = input$color)) +
      geom_point(alpha = 0.5, size = 1.5) +
      geom_line(stat = "smooth", method = "lm", alpha = 0.3, colour = "red") +
      scale_colour_distiller(palette = "YlOrRd", direction = 1) +
      theme(legend.position = "none")
    
  })
  
  # xvar scatter plot
  output$xscatter <- renderPlot({
    print('plotting xvar scatter plot')
    
    #linking to brushed points from bivariate scatter
    brushed <- brushedPoints(asp_selected_data(), input$bibrush)
    
    ggplot(asp_selected_data(), aes_string(input$xvar)) +
      geom_density(alpha = 0.2, fill = "blue") +
      geom_density(data=brushed, fill = "red") +
      theme(legend.position = "none")
    
  })
  
  # yvar scatter plot
  output$yscatter <- renderPlot({
    print('plotting yvar scatter plot')
    
    #linking to brushed points from bivariate scatter
    brushed <- brushedPoints(asp_selected_data(), input$bibrush)
    
    ggplot(asp_selected_data(), aes_string(input$yvar)) +
      geom_density(alpha = 0.2, fill = "blue") +
      geom_density(data=brushed, fill = "red") +
      theme(legend.position = "none")
    
  })
  
  # map updates --------------------------------------------------------------------------------------
  #create colorData to be able to create palette
  colorData <- reactive({
    print("reactive: subsetting to colorVar in data")
    df1 <- df
    df1$geometry <- NULL
    df1[,colorVar()]
  })
  
  #creating palette for colorvar
  #creating color palette
  colorpal <- reactive({
    print('reactive: create color palette')
    colorNumeric("YlOrRd", colorData())
  })
  pal <- reactive({
    print('reactive: create palette for leaflet arg')
    colorpal()(colorData())
  })
  
  #get reactive x and y data for html labels
  xData <- reactive({
    print("reactive: subsetting to x Var in data")
    df1 <- df
    df1$geometry <- NULL
    df1[,xVar()]
  })
  
  yData <- reactive({
    print("reactive: subsetting to y Var in data")
    df1 <- df
    df1$geometry <- NULL
    df1[,yVar()]
  })
  
  
  #create html labels
  map.labels <- reactive({
    sprintf(
      "%s<br/>%s<br/>%s",
      paste0(input$xvar,": ", round(xData(),digits = 2)),
      paste0(input$yvar, ": ", round(yData(),digits = 2)),
      paste0(input$color, ": ", round(colorData(),digits = 2))
    ) %>% lapply(htmltools::HTML)
    
  })
  
  # Update map to be chloropleth of colorvar w/legend ---------------------------------------------
  
  # update map with polygons
  # - child of: pal()
  observe({
    print('observe: updating map to be chloropleth of colorvar')
    print(paste0("df class: ",class(df)))
    leafletProxy('map') %>%
      addPolygons(
        data = df,
        fillColor = pal(),
        weight = 1,
        opacity = 1,
        color = "black",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = map.labels(), popup = map.labels(), #~htmlEscape(input$color)
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")
      )
  })
  
  #add legend
  observe({
    print("observe: legend")
    leafletProxy("map") %>%
      clearControls() %>%
      addLegend(opacity = 0.99,position = "bottomright",title = colorVar(),
                pal = colorpal(), values = rev(colorData()))
  })
  
  
} # ---------------------------------------------------------------------- end server function 


######################################################################################################
# -------------------------------------- shiny app call -------------------------------------------- #
######################################################################################################
shinyApp(ui = ui, server = server)
