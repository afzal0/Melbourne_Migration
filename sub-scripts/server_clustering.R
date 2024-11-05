# Server logic for K-means Clustering tab

output$clustering_map <- renderLeaflet({
  # Filter data based on selected year
  year_data <- dominant_country_data_combined_kmeans %>% filter(Year == input$year)
  
  # Handle NA clusters
  year_data <- year_data %>% mutate(cluster = ifelse(is.na(cluster), 1, cluster))
  
  pal <- colorFactor(viridis::viridis(length(unique(year_data$cluster))), domain = year_data$cluster)
  
  leaflet(data = year_data) %>%
    addTiles() %>%
    addPolygons(
      fillColor = ~pal(cluster),
      fillOpacity = 0.7,
      color = "white",
      weight = 1,
      label = ~paste("LGA:", LGA, "<br> Cluster:", cluster, "<br> Migrant Percentage:", round(center, 2), "%"),
      highlightOptions = highlightOptions(weight = 3, color = "blue", bringToFront = TRUE)
    ) %>%
    addLegend(
      pal = pal, values = ~cluster, opacity = 0.7, title = "Clusters",
      position = "bottomright"
    )
})

output$text_output_summary <- renderUI({
  year <- input$year
  text <- switch(as.character(year),
                 "2001" = tags$div(
                   tags$h3("Map 1 (2001):"),
                   tags$p("This map depicts the distribution of migrant country clusters across local government areas in Victoria as of 2001. The clusters are represented by different colors, with each color signifying a distinct cluster. The map reveals that migrant populations are concentrated in specific regions, particularly in and around Melbourne. Shepparton and Mildura also emerge as areas with notable migrant clusters.")
                 ),
                 "2006" = tags$div(
                   tags$h3("Map 2 (2006):"),
                   tags$p("The 2006 map illustrates the migrant country clusters by local government areas in Victoria. Compared to the 2021 map, the distribution of clusters appears more dispersed, with fewer high-density migrant populations. However, the Melbourne metropolitan area still exhibits a significant concentration of migrant clusters. Regional cities such as Shepparton, Mildura, and Wodonga also showcase the presence of migrant communities.")
                 ),
                 "2011" = tags$div(
                   tags$h3("Map 3 (2011):"),
                   tags$p("In 2011, the migrant country clusters map of Victoria displays a distribution pattern similar to that of 2006. The Melbourne region continues to be the primary hub for migrant populations, with various clusters represented. Regional centers like Mildura, Shepparton, and Wodonga maintain their status as areas with notable migrant communities. The map indicates a gradual evolution in the spatial distribution of migrant clusters across the state.")
                 ),
                 "2016" = tags$div(
                   tags$h3("Map 4 (2016):"),
                   tags$p("The 2016 map presents a more pronounced concentration of migrant country clusters in the Melbourne metropolitan area compared to previous years. The city center and its surrounding suburbs exhibit a higher density of migrant populations. Regional cities such as Mildura, Shepparton, and Wodonga continue to show significant migrant clusters. The map suggests an ongoing trend of migrant populations gravitating towards urban centers.")
                 ),
                 "2021" = tags$div(
                   tags$h3("Map 5 (2021):"),
                   tags$p("The 2021 map, identical to Image 1, showcases the most recent distribution of migrant country clusters in Victoria. The Melbourne region remains the focal point, with a high concentration of migrant populations. Shepparton and Mildura also stand out as regional areas with substantial migrant clusters. This map serves as a culmination of the evolving spatial patterns of migrant communities over the years, highlighting the current state of migrant settlement in Victoria.")
                 ),
                 default = tags$div(
                   tags$p("No text available for the selected year.")
                 )
  )
  text
})