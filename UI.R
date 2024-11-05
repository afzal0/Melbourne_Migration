libraries <- c("shiny", "leaflet", "plotly", "dplyr", "readr", "viridis", "tidyr", "networkD3", 
               "stringr", "heatmaply", "tibble", "shinycssloaders", "shinyWidgets", "sf",
               "shinythemes")

for (lib in libraries) {
  if (!require(lib, character.only = TRUE)) {
    install.packages(lib)
    library(lib, character.only = TRUE)
  }
}

# Load the necessary datasets
dominant_data_combined <- readRDS("data/dominant_data_combined.rds")
dominant_country_data_combined <- readRDS("data/simplified_dominant_country_data_0.001.rds") 
#Reduced file, please change to dominant_country_data_combined.rds from gdrive and also change in server_dominant_population.R
raw_bar_chart <- read_csv("data/raw_bar_chart.csv")
dominant_country_data_combined_kmeans <- readRDS("data/victoria_migrant_clusters_all_years.rds")

# Ensure the CRS is set correctly
dominant_country_data_combined_kmeans <- st_transform(dominant_country_data_combined_kmeans, crs = "+proj=longlat +datum=WGS84")

# Get unique years from the dataset
unique_years <- sort(unique(dominant_country_data_combined_kmeans$Year))

ui <- fluidPage(
  theme = shinytheme("yeti"),
  
  # Custom CSS styles
  tags$head(
    tags$style(HTML("
    .intro-text {
      margin-bottom: 20px;
      font-size: 18px;
      line-height: 1.6;
    }
    
    .image-container {
      margin-bottom: 20px;
    }
    
    .fact-card {
      padding: 20px;
      border-radius: 5px;
      box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
      margin-bottom: 20px;
      color: white;
    }
    
    .fact-card h3 {
      font-size: 18px;
      margin-bottom: 10px;
    }
    
    .fact-card p {
      font-size: 14px;
    }
    
    .fact-card-1 {
      background-color: #ff6b6b;
    }
    
    .fact-card-2 {
      background-color: #4ecdc4;
    }
    
    .fact-card-3 {
      background-color: #ffe66d;
    }
  "))
  ),
  
  navbarPage(
    title = "Migration Analysis Dashboard",
    tabPanel("Home",
             fluidRow(
               column(
                 width = 12,
                 div(
                   class = "intro-text",
                   h2("Welcome to the Migration Analysis Dashboard"),
                   p("Explore the ethnic diversity and migration patterns in Victoria, Australia."),
                   p("This dashboard provides insights into the changing demographics and cultural landscape of Victoria over the past two decades."),
                   p("Navigate through the tabs to access interactive visualizations, maps, and statistical analyses."),
                   p("Gain a deeper understanding of the migrant communities that contribute to Victoria's vibrant and multicultural society.")
                 )
               )
             ),
             fluidRow(
               column(
                 width = 12,
                 div(
                   class = "image-container",
                   img(src = "pic.jpg", alt = "Multicultural Society Image", width = "100%")
                 )
               )
             ),
             fluidRow(
               column(
                 width = 4,
                 div(
                   class = "fact-card fact-card-1",
                   h3("Migration Fact 1"),
                   p("Over 30% of Victoria's population was born overseas.")
                 )
               ),
               column(
                 width = 4,
                 div(
                   class = "fact-card fact-card-2",
                   h3("Migration Fact 2"),
                   p("Victoria is home to people from more than 200 countries.")
                 )
               ),
               column(
                 width = 4,
                 div(
                   class = "fact-card fact-card-3",
                   h3("Migration Fact 3"),
                   p("The top source countries of migrants in Victoria are India, China, and the United Kingdom.")
                 )
               )
             )
    ),
  
    tabPanel("Introduction",
             fluidRow(
               column(
                 width = 12,
                 h3("Introduction"),
                 tags$head(
                   tags$style(HTML("
                     #tooltip {
                       position: absolute;
                       width: auto;
                       padding: 10px;
                       background: white;
                       border: 1px solid #777;
                       border-radius: 5px;
                       pointer-events: none;
                       opacity: 0;
                       transition: opacity 0.3s;
                     }
                   ")),
                   tags$script(HTML("
                     $(document).on('shiny:connected', function(event) {
                       var tooltipDiv = $('#tooltip');
                       $('body').on('mouseover', '.node, .link', function(e) {
                         var title = $(this).find('title').text();
                         var rect = $(this).find('rect');
                         if (rect.length) {
                           var rectPos = rect.offset();
                           var rectWidth = rect.width();
                           tooltipDiv.html(title)
                                     .css({
                                       'opacity': 1,
                                       'left': (rectPos.left + rectWidth + 10) + 'px',
                                       'top': rectPos.top + 'px'
                                     });
                         } else {
                           var path = $(this).find('path');
                           if (path.length) {
                             var pathPos = path.offset();
                             var pathWidth = path.width();
                             tooltipDiv.html(title)
                                       .css({
                                         'opacity': 1,
                                         'left': (pathPos.left + pathWidth / 2 + 10) + 'px',
                                         'top': pathPos.top + 'px'
                                       });
                           }
                         }
                       }).on('mouseout', '.node, .link', function() {
                         tooltipDiv.css('opacity', 0);
                       });
                     });
                   ")),
                 ),
                 fluidRow(
                   column(
                     width = 12,
                     h4("Interactive Map"),
                     withSpinner(leafletOutput("map", height = "600px")),
                     absolutePanel(
                       top = 20, right = 50,
                       sliderInput("yearInput", "Select Year:", min = 2001, max = 2021, value = 2001, step = 5, width = "100%"),
                       actionButton("resetButton", "Reset Map and the Graphs")
                     )
                   )
                 ),
                 fluidRow(
                   column(
                     width = 12,
                     h4("Analysing the Flow of Migration"),
                     tags$ul(
                       tags$li("This interactive map visualization provides a global perspective on migration patterns and intensities, based on the selected year filter. The map displays the world's landmasses, with lines of varying colors originating from different countries or regions. These lines represent migration flows, and their thickness corresponds to the number of migrants, as indicated by the 'Migrants Count' legend. Thicker lines signify a larger migrant population, while thinner lines represent smaller migrant groups. By hovering over a specific line, a tooltip appears, providing detailed information about the migration flow."),
                       tags$li("The map also includes a 'Migration Intensity' legend, which uses a color gradient from purple to green to represent the intensity or concentration of migration flows. Areas with darker shades of purple indicate higher migration intensities, while lighter shades of green signify lower intensities. Below the map, there is a legend that associates different colors with specific countries or regions of origin for the migration flows. For instance, the blue line represents migrants from the United Kingdom, while the purple line corresponds to migrants born elsewhere overseas, and the red line represents migrants from Italy. This visualization tool allows users to explore and analyze migration patterns on a global scale, enabling them to identify major migration corridors, source countries or regions, and the relative magnitudes of different migration flows. By selecting different years from the 'Select Year' slider, users can observe how migration patterns have evolved over time, facilitating a deeper understanding of global migration dynamics and trends.")
                     )
                   )
                 ),
                 fluidRow(
                   column(
                     width = 12,
                     tags$div(
                       style = "position: relative;",
                       withSpinner(sankeyNetworkOutput("sankey", width = "100%", height = "400px")),
                       tags$div(id = "tooltip")
                     )
                   )
                 ),
                 fluidRow(
                   column(
                     width = 12,
                     h4("Visualising Migration Change through Sankey and Bar Plot"),
                     tags$ul(
                       tags$li("The two visualizations provide complementary information about migration patterns and statistics in Victoria, Australia. The first visualization is an interactive Sankey diagram that displays migration flows from various countries or regions to Australia. The flows represent different migration corridors, with their thickness indicating the magnitude of the migrant population. By hovering over a specific line, users can view the source country/region and the corresponding number of migrants."),
                       tags$li("The second visualization is a horizontal bar chart titled 'Top Source Countries of Migrants in 2001.' This chart displays the top source countries or regions contributing migrants to Victoria in the year 2001. Each bar represents a different country or region, and its length corresponds to the number of migrants from that particular source. By default, when no specific country is selected on the flow map, these visualizations provide an overview of the overall migration patterns and statistics for the entire state of Victoria. The flow map shows the migration corridors from different parts of the world, and the bar chart displays the top source countries or regions contributing to the total migrant population in Victoria."),
                       tags$li("However, when a specific country or region is selected on the flow map, the visualizations dynamically update to display migration statistics specifically related to that selected country or region. In this case, the bar chart will show the distribution of migrants from the selected country or region across different Local Government Areas (LGAs) or suburbs within Victoria. This interactive feature allows users to analyze migration patterns and statistics at both the state level and the more granular level of individual source countries or regions."),
                       tags$li("By exploring the visualizations together, users can gain insights into the overall migration landscape in Victoria, as well as the specific contributions and distributions of migrants from different parts of the world across the state's various local areas.")
                     )
                   )
                 ),
                 fluidRow(
                   column(width = 12, withSpinner(plotlyOutput("interactive_viz2")))
                 ),
                 fluidRow(
                   column(width = 12, withSpinner(plotlyOutput("interactive_viz1")))
                 ),
                 fluidRow(
                   column(
                     width = 12,
                     h4("Migration Heatmap Analysis"),
                     tags$ul(
                       tags$li("The migration heatmap provides a compelling visual representation of the distribution and concentration of overseas visitors and migrants across various suburbs and regions in Victoria, Australia. By leveraging color intensity to denote the relative number of migrants in each area, the heatmap enables viewers to quickly grasp the spatial patterns and hotspots of migration within the state."),
                       tags$li("The dynamic nature of the heatmap, with its interactive year slider, adds a temporal dimension to the visualization. This feature allows users to explore how migration patterns have evolved over time, revealing trends, shifts, and emerging areas of migrant concentration. The ability to analyze these changes year by year provides valuable insights into the historical context and trajectory of migration in Victoria."),
                       tags$li("The heatmap effectively highlights the suburbs and regions that have been significant destinations for migrants. The darker shades on the map draw attention to areas with higher migrant populations, such as Hume, Banyule, and Greater Dandenong. This visual emphasis helps identify the key multicultural hubs within Victoria and underscores the cultural diversity that enriches these communities."),
                       tags$li("By presenting the migration data in a visually engaging and intuitive format, the heatmap facilitates a deeper understanding of the spatial distribution of migrants across Victoria. It serves as a powerful tool for policymakers, researchers, and the general public to comprehend the impact and extent of migration in the state. The heatmap not only communicates the quantitative aspects of migration but also tells a story of Victoria's rich multicultural tapestry and the significant role that migrants play in shaping its social, economic, and cultural landscape."),
                       tags$li("In summary, the migration heatmap is an invaluable addition to the report, offering a visually striking and informative depiction of Victoria's migration patterns. It enables readers to explore the spatial and temporal dimensions of migration, identify key migrant hubs, and appreciate the cultural diversity that defines the state. The heatmap serves as a testament to Victoria's long-standing history of welcoming migrants from around the world and highlights the ongoing importance of migration in driving the state's growth and vibrancy.")
                     )
                   )
                 )
               )
             )
    ),
    tabPanel("Spatial Analysis",
             fluidRow(
               column(
                 width = 12,
                 h3("Interactive Maps"),
                 tabsetPanel(
                   tabPanel(
                     "Dominant Migrants",
                     fluidRow(
                       column(
                         width = 12,
                         sliderInput("year1", "Select Year:", min = 2001, max = 2021, value = 2021, step = 5, sep = "", width='25%', animate = FALSE),
                         withSpinner(leafletOutput("map1", height = "600px"))
                       ),
                       tags$style("
                         #text_panel {
                           background-color: rgba(255, 255, 255, 0.8);
                           padding: 10px;
                           border-radius: 5px;
                           box-shadow: 0 0 10px rgba(0, 0, 0, 0.3);
                         }
                       "),
                       tags$div(
                         style = "font-family: Arial, sans-serif; margin-top: 20px;",
                         tags$h4("Introduction:"),
                         tags$p("These choropleth maps depict the most common migrant population in terms of native countries in different suburbs of Victoria, Australia, for several years. In all the maps, similar colors are used, but each suburb has its color showing the migrant country that has the highest number of people in a particular year chosen from this suburb. Such visualization approach is effective when it comes to spatially showing where different communities settle within a particular region and how concentrated they are. These maps serve as indicators of immigrant variety and their settlements across Victoria's diverse areas. One can see changes occurring over time periods which indicate trends happening among these prominent immigrant groups living within each separate neighborhood while analyzing them separately by year; therefore these visualizations are dynamic in nature reflecting on shifting population dynamics due to migration processes in various parts of Australia.")
                       )
                     )
                   ),
                   tabPanel(
                     "LGA Analysis",
                     fluidRow(
                       column(
                         width = 12,
                         sliderInput("year2", "Select Year:", min = 2001, max = 2021, value = 2021, step = 5, sep = "", width='25%', animate = FALSE),
                         checkboxInput("showDominant", "Show Dominant Population", value = TRUE),
                         conditionalPanel(
                           condition = "input.showDominant == false",
                           selectInput("country", "Select Country of Birth:", choices = NULL)
                         ),
                         withSpinner(leafletOutput("map2", height = "800px")),
                         tags$div(
                           style = "font-family: Arial, sans-serif; margin-top: 20px;",
                           tags$h4("Introduction (Map 1):"),
                           tags$p("Migration analysis visualizations such as line charts and stacked bar charts provide useful information about population movements and demographic changes in particular regions. Using line charts, the general direction of migrant populations can be observed within time-span so that one can spot increase, decrease or a possible stability. On the other hand, stacked bar charts elaborate more on migrant population segments showing their sources. In-depth analysis assists users appreciate different migrant groups present in a specific place as well as how their ratios change in time. Users can view the total trends in the population of migrants and its distribution by place of birth using the two types of visuals. These visualization tools help our researchers, policy makers, and urban planners alike understand these trends, plan resources usage accordingly, and adopt specific strategies aimed at meeting special needs or addressing issues confronting varied immigrant societies located within their areas of focus."),
                           tags$h4("Introduction (Map 2):"),
                           tags$p("This interactive map visualization allows users to explore the geographical distribution of migrant populations originating from a specific country of birth across different regions or suburbs. By selecting a country from the dropdown menu, the map dynamically updates to display the population counts for that chosen country of birth, color-coded and clustered within the respective suburban or regional boundaries.\n This type of visualization is particularly useful for understanding the spatial distribution patterns of migrant communities originating from different countries. It enables users to identify areas with higher concentrations of specific migrant populations, which can inform decision-making processes related to resource allocation, community services, and integration initiatives. Furthermore, by comparing the maps for different countries of birth, users can gain insights into the diversity and composition of migrant populations across various regions, facilitating a more comprehensive understanding of migration dynamics and demographic trends.")
                         )
                       )
                     )
                   )
                 )
               )
             )
    ),
    tabPanel("Advanced Analysis",
             fluidRow(
               column(
                 width = 12,
                 h3("Clustering"),
                 column(
                   width = 12,
                   h2("Migrant Country Clusters by Local Government Areas in Victoria"),
                   sliderTextInput("year", "Select Year", choices = unique_years, selected = unique_years[1], grid = TRUE),
                   withSpinner(leafletOutput("clustering_map", height = "calc(100vh - 300px)")),
                   absolutePanel(
                     id = "text_panel",
                     class = "panel panel-default",
                     fixed = TRUE,
                     draggable = TRUE,
                     top = 100, left = "auto", right = 20, bottom = "auto",
                     width = 300, height = "auto",
                     h3("Description"),
                     uiOutput("text_output_summary")
                   ),
              tags$style(
                "#text_panel {
                              background-color: rgba(255, 255, 255, 0.8);
                              padding: 10px;
                              border-radius: 5px;
                              box-shadow: 0 0 10px rgba(0, 0, 0, 0.3);
                              }"
              ),
              tags$div(
                style = "font-family: Arial, sans-serif; margin-top: 20px;",
                tags$h3("Introduction:"),
                tags$p("From 2006 to 2021, the spatial distribution of migrant country clusters across local government areas in Victoria, Australia is represented on provided series of maps. These maps give an idea about how migrants have settled in different parts of the state, identifying where their numbers are high and how they have changed over time. If policy makers, researchers and community organizations study them, they will have a deeper appreciation of what drives migration and this will help them in decision making on sharing resources, service provision, and keeping society together."),
                tags$h3("K-means Clustering:"),
                tags$p("In this situation, the concept of analyzing local government authorities utilizes a computer-based technique which groups them according to how their migrant populations resemble one another using statistics. K-means is a method of cluster analysis that separates data points into various groups. \"k\" stands for the number of clusters that users define. Accordingly, the maps demonstrate clusters from 1 through 8 with each bearing a different color. This implies that if there are k local government areas there is also an equal amount of variance between their average characteristic values while at the same time minimizing them within particular ones by means of dividing them into k parts- highest possible numbers will present dissimilarities between regions within such divisions whereas smallest disparities would appear within boundaries set by these sub-categories including all other kinds of considerations.")
                   )
                 )
               )
             )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Source of the server files
  source("sub-scripts/server_introduction.R", local = TRUE)
  source("sub-scripts/server_dominant_population.R", local = TRUE)
  source("sub-scripts/server_clustering.R", local = TRUE)
}

# Run the application
shinyApp(ui = ui, server = server)

