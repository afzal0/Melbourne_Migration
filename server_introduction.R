# Load and prepare the data
geocoded_data <- read_csv('geocoded_data.csv')
migration_data <- read_csv('Final_DB.csv')

# Transform migration data to be year-specific
migration_data <- migration_data %>%
  pivot_longer(cols = starts_with("year_"), names_to = "Year", values_to = "Migrants") %>%
  mutate(Year = as.numeric(str_replace_all(Year, "year_", ""))) %>%
  group_by(country_by_birth, Year) %>%
  summarise(Migrants = sum(Migrants, na.rm = TRUE), .groups = 'drop')

# Join geocoded data with migration data
migration_flow <- geocoded_data %>%
  left_join(migration_data, by = "country_by_birth") %>%
  filter(country_by_birth != "Australia", Migrants >= 100)  # Exclude Australia and filter by Migrant count

# Define the common destination coordinates (e.g., Canberra, Australia)
canberra <- data.frame(lon = 149.128684, lat = -35.282001)

# Determine max migrants for color scaling
max_migrants <- max(migration_flow$Migrants, na.rm = TRUE)

# Reactive expression to read and process data based on selected day
sankeyData <- reactive({
  req(input$yearInput)
  
  # Read the CSV file
  data <- read.csv("Final_DB.csv", stringsAsFactors = FALSE)
  
  # Filter and group the data
  year_col <- paste0("year_", input$yearInput)
  data <- data %>%
    filter(!is.na(!!sym(year_col)), !!sym(year_col) > 0,
           !country_by_birth %in% c("Australia", "Not stated", "Country of birth not stated", "Country not stated")) %>%
    group_by(country_by_birth) %>%
    summarise(Total_Migrants = sum(!!sym(year_col))) %>%
    ungroup() %>%
    filter(Total_Migrants > 0)  # Filter out countries with 0 migration
  
  # Arrange by descending order of migrants and select top 30
  data <- data %>%
    arrange(desc(Total_Migrants)) %>%
    slice_head(n = 30)  # Select the top 30 nodes
  
  # Nodes and links preparation
  nodes <- unique(c(data$country_by_birth, "Australia"))
  nodes <- data.frame(name = nodes)
  
  links <- data %>%
    mutate(source = match(country_by_birth, nodes$name) - 1,
           target = which(nodes$name == "Australia") - 1,
           value = Total_Migrants) %>%
    select(source, target, value)
  
  list(nodes = nodes, links = links)
})

output$map <- renderLeaflet({
  req(input$yearInput)  # Ensure that the year input is initialized
  
  # Filter data for the selected year
  data_selected <- migration_flow %>% 
    filter(Year == input$yearInput | is.na(Year))
  
  # Create the map with curved flow lines and proportional symbols
  map <- leaflet(data_selected) %>%
    addTiles() %>%
    setView(lng = 10, lat = 20, zoom = 2)  # Set a default view
  
  # Determine the maximum number of migrants for color scaling
  max_migrants <- max(data_selected$Migrants, na.rm = TRUE)
  
  # Add proportional symbols and curved flow lines for each country to Canberra
  if(nrow(data_selected) > 0) {
    for(i in 1:nrow(data_selected)) {
      migration_pct <- data_selected$Migrants[i] / max_migrants  # Calculate migration percentage
      
      map <- map %>%
        addCircles(lng = data_selected$lon[i], lat = data_selected$lat[i],
                   radius = sqrt(data_selected$Migrants[i]) * 1000,
                   weight = 1, color = '#ffa500', fillColor = '#ffa500', fillOpacity = 0.8,
                   popup = paste("Migration from", data_selected$country_by_birth[i], 
                                 ":", data_selected$Migrants[i], "migrants"),
                   layerId = data_selected$country_by_birth[i]) %>%
        addPolylines(
          lng = c(data_selected$lon[i], canberra$lon),
          lat = c(data_selected$lat[i], canberra$lat),
          color = colorNumeric(palette = "viridis", domain = c(0, 1))(migration_pct),
          weight = 3,
          opacity = 0.8,
          popup = paste("Migration from", data_selected$country_by_birth[i], "in",
                        data_selected$Year[i], ":", round(migration_pct * 100, 2), "% of max migration"),
          group = "lines"
        )
    }
  }
  
  # Add legends
  map %>% addLegend("bottomleft", pal = colorNumeric(palette = "viridis", domain = c(0, 1)), values = c(0, 1),
                    title = "Migration Intensity",
                    opacity = 0.5) %>%
    addLegend("topleft", pal = colorNumeric(palette = "viridis", domain = c(0, max_migrants)), values = ~Migrants,
              title = "Migrants Count",
              labFormat = labelFormat(suffix = " migrants"),
              opacity = 0.5)
})

observeEvent(input$map_shape_click, {
  country <- input$map_shape_click$id
  if (!is.null(country)) {
    updateSankeyDiagram(country)
    updateBarChart(country)
    
  }
})
updateBarChart <- function(country) {
  req(input$yearInput)
  
  # Filter data for the selected year and country
  year_col <- paste0("year_", input$yearInput)
  data <- read.csv("Final_DB.csv", stringsAsFactors = FALSE)
  data <- data %>%
    filter(country_by_birth == country, !is.na(!!sym(year_col)), !!sym(year_col) > 0) %>%
    group_by(suburb) %>%
    summarise(Total_Migrants = sum(!!sym(year_col))) %>%
    ungroup() %>%
    arrange(desc(Total_Migrants)) %>%
    slice_head(n = 30)  # Select the top 30 suburbs
  
  data$percentage <- data$Total_Migrants / sum(data$Total_Migrants) * 100
  
  # Update the bar chart in viz2
  output$interactive_viz2 <- renderPlotly({
    data <- data %>%
      slice_head(n = 10)  # Select the top 10 suburbs
    
    plot_ly(data, x = ~Total_Migrants, y = ~reorder(suburb, Total_Migrants), type = 'bar',
            text = ~paste(suburb, ': ', Total_Migrants, 'migrants'),
            hoverinfo = 'text',
            orientation = 'h') %>%
      layout(title = paste('Top Suburbs of Migrants in', country, 'in', input$yearInput),
             xaxis = list(title = 'Number of Migrants'),
             yaxis = list(title = 'Suburb'),
             showlegend = FALSE) %>%
      config(displayModeBar = FALSE)
  })
}

updateSankeyDiagram <- function(country) {
  req(input$yearInput)
  
  # Filter data for the selected year and country
  year_col <- paste0("year_", input$yearInput)
  data <- read.csv("Final_DB.csv", stringsAsFactors = FALSE)
  data <- data %>%
    filter(country_by_birth == country, !is.na(!!sym(year_col)), !!sym(year_col) > 0) %>%
    group_by(suburb) %>%
    summarise(Total_Migrants = sum(!!sym(year_col))) %>%
    ungroup() %>%
    arrange(desc(Total_Migrants)) %>%
    slice_head(n = 30)  # Select the top 30 suburbs
  
  # Nodes and links preparation
  nodes <- unique(c(data$suburb, country))
  nodes <- data.frame(name = nodes)
  
  links <- data %>%
    mutate(source = match(suburb, nodes$name) - 1,
           target = which(nodes$name == country) - 1,
           value = Total_Migrants) %>%
    select(source, target, value)
  
  # Update the Sankey diagram
  output$sankey <- renderSankeyNetwork({
    req(links, nodes)
    sankeyNetwork(Links = links, Nodes = nodes,
                  Source = "source", Target = "target", Value = "value", NodeID = "name",
                  units = "Migrants", fontSize = 12, nodeWidth = 30, iterations = 2,
                  sinksRight = TRUE)  # Set sinksRight to FALSE to rotate the diagram
  })
}

output$sankey <- renderSankeyNetwork({
  sankeyData <- sankeyData()
  sankeyNetwork(Links = sankeyData$links, Nodes = sankeyData$nodes,
                Source = "source", Target = "target", Value = "value", NodeID = "name",
                units = "Migrants", fontSize = 12, nodeWidth = 30, iterations = 0,
                sinksRight = TRUE)  # Set sinksRight to FALSE to rotate the diagram
})

observeEvent(input$resetButton, {
  # Reset the Sankey diagram to show all countries
  output$sankey <- renderSankeyNetwork({
    sankeyData <- sankeyData()
    sankeyNetwork(Links = sankeyData$links, Nodes = sankeyData$nodes,
                  Source = "source", Target = "target", Value = "value", NodeID = "name",
                  units = "Migrants", fontSize = 12, nodeWidth = 30, iterations = 0,
                  sinksRight = TRUE)  # Set sinksRight to FALSE to rotate the diagram
  })
  
  # Reset the map view to the default view
  leafletProxy("map") %>%
    setView(lng = 10, lat = 20, zoom = 2)
})


# Render interactive visualization 1: Heatmap of migration data
output$interactive_viz1 <- renderPlotly({
  req(input$yearInput)
  
  year_col <- paste0("year_", input$yearInput)
  data <- read.csv("Final_DB.csv", stringsAsFactors = FALSE)
  data <- data %>%
    filter(!is.na(!!sym(year_col)), !!sym(year_col) > 0,
           !country_by_birth %in% c("Australia", "Not stated","Born elsewhere overseas" ,"Country of birth not stated", "Country not stated")) %>%
    group_by(country_by_birth, suburb) %>%
    summarise(Total_Migrants = sum(!!sym(year_col)), .groups = "drop") %>%
    ungroup()
  
  # Reshape data into matrix format
  mat <- data %>%
    pivot_wider(names_from = suburb, values_from = Total_Migrants, values_fill = 0) %>%
    column_to_rownames(var = "country_by_birth")
  mat <- as.matrix(mat)
  
  # Create heatmap
  heatmaply(mat, 
            dendrogram = "none",
            xlab = "", ylab = "", 
            main = "",
            scale = "column",
            margins = c(60,100,40,20),
            grid_color = "white",
            grid_width = 0.00001,
            titleX = FALSE,
            hide_colorbar = TRUE,
            branches_lwd = 0.1,
            label_names = c("Country", "Suburb", "Migrants"),
            fontsize_row = 5, fontsize_col = 5,
            labCol = colnames(mat),
            labRow = rownames(mat),
            heatmap_layers = theme(axis.line = element_blank())
  ) %>% layout(title = paste("Migration Heatmap in", input$yearInput))
})

# Render interactive visualization 2: Top source countries of migrants
output$interactive_viz2 <- renderPlotly({
  req(input$yearInput)
  
  year_col <- paste0("year_", input$yearInput)
  data <- read.csv("Final_DB.csv", stringsAsFactors = FALSE)
  data <- data %>%
    filter(!is.na(!!sym(year_col)), !!sym(year_col) > 0,
           !country_by_birth %in% c("Australia", "Not stated", "Country of birth not stated", "Country not stated")) %>%
    group_by(country_by_birth) %>%
    summarise(Total_Migrants = sum(!!sym(year_col))) %>%
    ungroup() %>%
    arrange(desc(Total_Migrants)) %>%
    slice_head(n = 10)  # Select the top 10 source countries
  
  plot_ly(data, x = ~Total_Migrants, y = ~reorder(country_by_birth, Total_Migrants), type = 'bar',
          text = ~paste(country_by_birth, ': ', Total_Migrants, 'migrants'),
          hoverinfo = 'text',
          orientation = 'h') %>%
    layout(title = paste('Top Source Countries of Migrants in', input$yearInput),
           xaxis = list(title = 'Number of Migrants'),
           yaxis = list(title = 'Country of Birth'),
           showlegend = FALSE) %>%
    config(displayModeBar = FALSE)
})