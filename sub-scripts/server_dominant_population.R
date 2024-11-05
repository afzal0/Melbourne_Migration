# Load data
dominant_data_combined <- readRDS("./data/dominant_data_combined.rds")
dominant_country_data_combined <- readRDS("./data/simplified_dominant_country_data_0.001.rds") #change this as well
raw_bar_chart <- read.csv("./data/raw_bar_chart.csv")

# Filter data based on user inputs for Map 1
filtered_data1 <- reactive({
  req(input$year1)
  withProgress(message = "Loading data for Map 1", {
    dominant_data_combined %>%
      filter(Year == as.integer(input$year1))
  })
})

# Render Map 1
output$map1 <- renderLeaflet({
  data <- filtered_data1()
  # Fetch the reactive data
  
  if (nrow(data) == 0) {
    leaflet() %>%
      addTiles()  # Base map in case data is not available
  } else {
    # Create color palette for countries
    countries <- unique(data$country_by_birth)
    pal_country <- colorFactor(palette = "Set1", domain = countries)
    
    leaflet(data) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal_country(country_by_birth),
        fillOpacity = 0.7,
        color = "#BDBDC3",
        weight = 1,
        popup = ~paste("<strong>Suburb:</strong>", suburb,
                       "<br><strong>Dominant Migrant Country:</strong>", country_by_birth,
                       "<br><strong>Migrant Percentage:</strong>", sprintf("%.2f%%", Migrant_Percentage)),
        group = "polygons",
        layerId = ~suburb
      ) %>%
      addLegend(
        "topright",
        pal = pal_country,
        values = ~country_by_birth,
        title = "Dominant Migrant Country"
      )
  }
})

# Filter data based on user inputs for Map 2
filtered_data2 <- reactive({
  req(input$year2)
  withProgress(message = "Loading data for Map 2", {
    if (input$showDominant) {
      dominant_data_combined %>%
        filter(Year == as.integer(input$year2))
    } else {
      req(input$country)
      dominant_country_data_combined %>%
        filter(Year == as.integer(input$year2), country_by_birth == input$country)
    }
  })
})

# Update the country selection choices based on the selected year
observe({
  year <- as.integer(input$year2)
  choices <- unique(dominant_country_data_combined$country_by_birth[dominant_country_data_combined$Year == year])
  updateSelectInput(session, "country", choices = choices)
})

# Render Map 2
output$map2 <- renderLeaflet({
  data <- filtered_data2()
  # Fetch the reactive data
  
  if (nrow(data) == 0) {
    leaflet() %>%
      addTiles()  # Base map in case data is not available
  } else {
    if (input$showDominant) {
      # Create color palette for countries
      countries <- unique(data$country_by_birth)
      pal_country <- colorFactor(palette = "Set1", domain = countries)
      
      leaflet(data) %>%
        addTiles() %>%
        addPolygons(
          fillColor = ~pal_country(country_by_birth),
          fillOpacity = 0.7,
          color = "#BDBDC3",
          weight = 1,
          popup = ~paste("<strong>Suburb:</strong>", suburb,
                         "<br><strong>Dominant Migrant Country:</strong>", country_by_birth,
                         "<br><strong>Migrant Percentage:</strong>", sprintf("%.2f%%", Migrant_Percentage)),
          group = "polygons",
          layerId = ~suburb
        ) %>%
        addLegend(
          "topright",
          pal = pal_country,
          values = ~country_by_birth,
          title = "Dominant Migrant Country"
        )
    } else {
      # Create color palette for population
      pal_population <- colorNumeric(palette = "viridis", domain = data$Population)
      
      leaflet(data) %>%
        addTiles() %>%
        addPolygons(
          fillColor = ~pal_population(Population),
          fillOpacity = 0.7,
          color = "#BDBDC3",
          weight = 1,
          popup = ~paste("<strong>Suburb:</strong>", suburb,
                         "<br><strong>Country of Birth:</strong>", country_by_birth,
                         "<br><strong>Population:</strong>", Population)
        ) %>%
        addLegend(
          "topright",
          pal = pal_population,
          values = ~Population,
          title = "Population"
        )
    }
  }
})

# Show charts when a suburb is clicked (only for dominant population)
observeEvent(input$map2_shape_click, {
  if (input$showDominant) {
    clicked_suburb <- input$map2_shape_click$id
    
    withProgress(message = "Loading charts for selected suburb", {
      filtered_data <- raw_bar_chart %>%
        filter(suburb == clicked_suburb, country_by_birth != "Australia") %>%
        filter(Population > 0)  # Remove N/A or 0 population values
      
      # Create stacked bar chart
      bar_chart <- ggplot(filtered_data, aes(x = country_by_birth, y = Population, fill = factor(Year))) +
        geom_bar(stat = "identity", width = 0.7) +
        labs(
          x = "Country of Birth", y = "Population", fill = "Year",
          title = paste("Population by Country of Birth -", clicked_suburb)
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
          axis.title = element_text(size = 10),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 8),
          plot.title = element_text(size = 14, face = "bold")
        )
      
      # Create line chart
      line_chart <- ggplot(filtered_data %>%
                             group_by(Year) %>%
                             summarise(Total_Population = sum(Population), .groups = "drop"),
                           aes(x = Year, y = Total_Population)
      ) +
        geom_line(color = "steelblue", size = 1) +
        geom_point(color = "steelblue", size = 3) +
        labs(
          x = "Year", y = "Total Migrant Population",
          title = paste("Total Migrant Population -", clicked_suburb)
        ) +
        theme_minimal() +
        theme(
          axis.title = element_text(size = 10),
          axis.text = element_text(size = 8),
          plot.title = element_text(size = 14, face = "bold")
        )
      
      # Convert charts to plotly objects
      bar_chart <- ggplotly(bar_chart)
      line_chart <- ggplotly(line_chart)
      
      # Show charts in a modal dialog
      showModal(modalDialog(
        title = paste("Charts for", clicked_suburb),
        tabsetPanel(
          tabPanel("Bar Chart", plotlyOutput("bar_chart", height = "400px")),
          tabPanel("Line Chart", plotlyOutput("line_chart", height = "400px"))
        ),
        size = "l",
        easyClose = TRUE
      ))
      
      output$bar_chart <- renderPlotly({ bar_chart })
      output$line_chart <- renderPlotly({ line_chart })
    })
  }
})