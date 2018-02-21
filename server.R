##############################
#  NYC Zip Code Recommender  #
#     CKM : PRE : Elfta      #
#      by Sean Kickham       #
##############################



shinyServer(function(input, output) {
  
  # ------------------------------ datatable ------------------------------ 
  output$ziptable = DT::renderDataTable({
    # weighthttp://127.0.0.1:3889/#tab-3958-1s()$b
    zip.data
  
  })
  
  
  # ------------------------------ leafletmap ------------------------------
  output$map <- renderLeaflet({
   
    leaflet() %>%
      
      #mixing maps
      addProviderTiles("Stamen.TerrainBackground",
                       options = providerTileOptions(opacity = 0.35)) %>%      #Thunderforest.Transport   Stamen.TonerLite
      addProviderTiles(providers$Esri.OceanBasemap,
                       options = providerTileOptions(opacity = 0.35)) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas,
                       options = providerTileOptions(opacity = 0.65)) %>%
      
      #adding zip code boundaries
      addPolygons(data = DAT,                              # shapefile data
                  fillColor = 'transparent',               # transparent for initial
                  fillOpacity = 0.8,                       # 
                  color = "#444444",                       # color of boundaries
                  weight = 3,                              # thickness of boundaries
                  highlightOptions =                       # options for mouseover
                    highlightOptions(color = "white", 
                                     weight = 4,
                                     bringToFront = TRUE,
                                     opacity = 1),
                  popup= zip_popup()
                  ) %>% 
      
      #centered on NYC
      setView(lng = -74.006, lat = 40.7128, zoom = 11)
   
  }) 
  
  # ------------------------------ map popups ------------------------------
  zip_popup = reactive({
    # update DAT to include scores
    DAT = sp::merge(DAT, weights()$b, by="ZIPCODE", all=TRUE, duplicateGeoms = TRUE)
    
    paste("<strong>Zip: \t</strong>", DAT$ZIPCODE,
          "<br><strong>Score: \t</strong>", DAT$scores,
          "<br><strong>Cost: </strong>", DAT$Avg.PricePerSqFt,
          "<br><strong>Crime: </strong>", DAT$Avg.Crimes.by.Pop,
          "<br><strong>Complaints: </strong>", DAT$Avg.Complaints.by.Pop,
          "<br><strong>Fires: </strong>", DAT$Avg.Fires.by.Pop,
          "<br><strong>Population: </strong>", DAT$Population)
  })
  
  # ------------------------------ calculating scores ------------------------------
  weights = eventReactive(input$do, {

    #which columns to use as metrics
    col_options = c("Avg.PricePerSqFt", 
                    "Avg.Crimes.by.Pop",
                    "Avg.Complaints.by.Pop",
                    "Avg.Fires.by.Pop",
                    "Population")
    
    chosen_metrics = c(input$choosecost,
                       input$choosecrime,
                       input$choosecomplaints,
                       input$choosefire,
                       input$choosepop)
    
    kept_metrics = which(chosen_metrics)
    kept_cols = col_options[kept_metrics]
    
    #weights matrix
    kept_weights = c(as.numeric(input$w1),
                as.numeric(input$w2),
                as.numeric(input$w3),
                as.numeric(input$w4),
                as.numeric(input$w5))[kept_metrics]
    
    
    # wtsmat = matrix(weights, length(kept_metrics), 1)

    #slider ranges
    # ranges = rbind(input$lim1,
    #                input$lim2,
    #                input$lim3,
    #                input$lim4,
    #                input$lim5)

    # use slider ranges to filter observations
    # zip.data %>% filter()....
    zip_filtered = zip.data
    
    
    # apply weights to remaining observations to achieve score
    ZIPCODE = zip_filtered$Zip          #save the filtered zipcodes
    zip_subset = zip_filtered %>% select(kept_cols)
    zip_subset = t(kept_weights*t(zip_subset))        #multiply weights by features
    zip_subset = data.frame(zip_subset)
    scores = round(rowSums(zip_subset)/sum(kept_weights),1)    #weighted average scores
    
    #join scores and zipcode into single dataframe now
    back_together = data.frame(cbind(ZIPCODE, scores))
    
    
    best_zips = as.data.table(back_together)[order(-scores)][1:5]
    # worst_zips = back_together[order(scores)][1:5]
    
    #return the zipcodes & scores
    list(z = ZIPCODE, s = scores, b = back_together)#, best = best_zips, worst = worst_zips)
      
      
  })
  
  
  
  
  # ------------------------------ update map to recalculate ------------------------------
  observeEvent(input$do,{
    
    mypallete    = mypallete_spec_10
    mycolorlabel = labels_spec_10
    colortitle   = 'Score'
    
    # pal <- colorBin(mypallete, c(0,100), bins = 10, pretty = FALSE, na.color = "transparent", alpha = FALSE, reverse = TRUE)
    pal <- colorQuantile("Greens", NULL, n = 8)
    # pal <- colorQuantile(mypallete, NULL, n = 20, reverse = TRUE, na.color = "transparent")
    # pal <- colorBin("RdBu", c(-10,10), bins = 11, pretty = TRUE, na.color = "#808080", alpha = FALSE, reverse = FALSE)
 
    # update DAT to include scores
    DAT = sp::merge(DAT, weights()$b, by="ZIPCODE", all=TRUE, duplicateGeoms = TRUE)

    
    # update polygons
    proxy <- leafletProxy("map")
    proxy %>%
      clearShapes() %>%
      addPolygons(data=DAT, 
                  fillColor = pal(DAT$scores),
                  fillOpacity = 0.8, 
                  color = "#444444",
                  weight = 3, 
                  highlightOptions =                       # options for mouseover
                    highlightOptions(color = "white", 
                                     weight = 4,
                                     bringToFront = TRUE,
                                     opacity = 1),
                  popup= zip_popup()) 
    
    
    
  }) 
  
  # ------------------------------ distribution graphs (for later) ------------------------------
  # ggplot(zip.data, aes(x = Avg.Complaints.by.Pop)) + geom_histogram()
  
})
