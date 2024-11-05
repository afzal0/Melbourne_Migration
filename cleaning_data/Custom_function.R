library(dplyr)
library(tidyr)
library(stringr)
library(sf)
library(ggplot2)

process_migration_data_dominant <- function(lga_map_path, final_db_path, shapefile_path, year) {
  # Load and process LGA_map.csv to extract numeric LGA codes
  LGA_map <- read.csv(lga_map_path, stringsAsFactors = FALSE) %>%
    mutate(LGA_Code = as.character(stringr::str_extract(LGA.Code, "\\d+"))) %>%
    select(Suburb, LGA_Code)
  
  # Load and preprocess Final_DB.csv
  Final_DB <- read.csv(final_db_path, stringsAsFactors = FALSE) %>%
    filter(!country_by_birth %in% c("Country of birth not stated", "Born elsewhere", "Born elsewhere overseas", "Not stated", "Overseas visitors")) %>%
    mutate(country_by_birth = trimws(country_by_birth, which = "right"))
  
  # Combine similar entries
  Final_DB <- Final_DB %>%
    mutate(country_by_birth = case_when(
      country_by_birth == "Vietnam " ~ "Vietnam",
      country_by_birth == "United Kingdom, Channel Islands and Isle of Man" ~ "United Kingdom",
      country_by_birth == "England" ~ "United Kingdom",
      TRUE ~ country_by_birth
    ))
  
  # Reshape the data to long format and filter by the selected year
  Final_DB_long <- Final_DB %>%
    pivot_longer(cols = starts_with("year_"), names_to = "Year", values_to = "Population") %>%
    mutate(Year = as.integer(stringr::str_extract(Year, "\\d{4}"))) %>%
    filter(Year == as.integer(year), !is.na(Year))
  
  # Summarize migration data
  migration_summary <- Final_DB_long %>%
    filter(country_by_birth != "Australia") %>%
    group_by(suburb, Year, country_by_birth) %>%
    summarise(Total_Migrants = sum(Population), .groups = 'drop')
  
  # Calculate total population by suburb and year
  Total_Population_Suburb <- Final_DB_long %>%
    group_by(suburb, Year) %>%
    summarise(Total_Population = sum(Population), .groups = 'drop')
  
  # Join and calculate percentages
  migration_data <- left_join(migration_summary, Total_Population_Suburb, by = c("suburb", "Year")) %>%
    mutate(Migrant_Percentage = (Total_Migrants / Total_Population) * 100)
  
  # Find the prevalent migrant country for each suburb and year
  prevalent_country <- migration_data %>%
    group_by(suburb, Year) %>%
    top_n(1, Migrant_Percentage) %>%
    ungroup() %>%
    distinct(suburb, Year, .keep_all = TRUE)
  
  LGA_map <- LGA_map %>% rename(suburb = Suburb)
  
  # Join the prevalent country data with LGA codes
  prevalent_country <- left_join(prevalent_country, LGA_map, by = "suburb")
  
  # Load and transform the shapefile
  victoria_shape <- st_read(shapefile_path)
  victoria_shape <- st_transform(victoria_shape, 4326)
  victoria_shape$LGA_COD <- as.character(victoria_shape$LGA_COD)
  
  # Join shapefile data
  victoria_data_merged <- left_join(victoria_shape, prevalent_country, by = c("LGA_COD" = "LGA_Code"))
  victoria_data_merged <- victoria_data_merged %>% filter(!is.na(Year))
  
  return(victoria_data_merged)
}



process_migration_data_total <- function(lga_map_path, final_db_path, shapefile_path) {
  # Load LGA map and extract numeric LGA codes
  LGA_map <- read.csv(lga_map_path, stringsAsFactors = FALSE) %>%
    mutate(LGA_Code = as.character(str_extract(LGA.Code, "\\d+"))) %>%
    select(Suburb, LGA_Code) %>%
    rename(suburb = Suburb)
  
  # Load and preprocess Final_DB.csv
  Final_DB <- read.csv(final_db_path, stringsAsFactors = FALSE)
  
  # Reshape the data to long format
  Final_DB_long <- Final_DB %>%
    pivot_longer(cols = starts_with("year_"), names_to = "Year", values_to = "Population") %>%
    mutate(Year = as.integer(str_extract(Year, "\\d{4}"))) %>%
    filter(!is.na(Year))
  
  # Clean data by removing records for "Australia" and "Not stated"
  migration_summary <- Final_DB_long %>%
    filter(country_by_birth != "Australia", country_by_birth != "Not Stated") %>%
    group_by(suburb, Year) %>%
    summarise(Total_Migrants = sum(Population))
  
  # Calculate the total population and migration percentage by suburb and year
  Total_Population_Suburb <- Final_DB_long %>%
    group_by(suburb, Year) %>%
    summarise(Total_Population = sum(Population))
  
  migration_data <- left_join(migration_summary, Total_Population_Suburb, by = c("suburb", "Year")) %>%
    mutate(Migrant_Percentage = (Total_Migrants / Total_Population) * 100) %>%
    mutate(Migrant_Percentage = ifelse(is.na(Migrant_Percentage), 2, Migrant_Percentage))
  
  # Join the migration data with LGA codes
  migration_data <- left_join(migration_data, LGA_map, by = "suburb")
  
  # Load the shapefile
  victoria_shape <- st_read(shapefile_path)
  victoria_shape$LGA_COD <- as.character(victoria_shape$LGA_COD)
  
  # Join the migration data with the shapefile
  victoria_data_merged <- left_join(victoria_shape, migration_data, by = c("LGA_COD" = "LGA_Code"))
  victoria_data_merged <- victoria_data_merged %>% filter(!is.na(Year))
  
  return(victoria_data_merged)
}

process_migration_data_dominant_country <- function(lga_map_path_specific, final_db_path_specific, shapefile_path_specific, year_selected, country_selected) {
  # Load and process LGA_map.csv to extract numeric LGA codes
  lga_map_data <- read.csv(lga_map_path_specific, stringsAsFactors = FALSE) %>%
    mutate(LGA_Code = as.character(stringr::str_extract(LGA.Code, "\\d+"))) %>%
    select(Suburb, LGA_Code) %>%
    rename(suburb_name = Suburb)
  
  # Load and preprocess Final_DB.csv
  final_db_data <- read.csv(final_db_path_specific, stringsAsFactors = FALSE) %>%
    filter(!country_by_birth %in% c("Country of birth not stated", "Born elsewhere", "Born elsewhere overseas", "Not stated", "Overseas visitors")) %>%
    mutate(country_by_birth = trimws(country_by_birth, which = "right"))
  
  # Combine similar entries for simplification
  final_db_cleaned <- final_db_data %>%
    mutate(country_by_birth = case_when(
      country_by_birth == "Vietnam " ~ "Vietnam",
      country_by_birth == "United Kingdom, Channel Islands and Isle of Man" ~ "United Kingdom",
      country_by_birth == "England" ~ "United Kingdom",
      TRUE ~ country_by_birth
    ))
  
  # Join LGA_map_data to ensure LGA_Code_Unique is present in the dataset
  final_db_cleaned <- left_join(final_db_cleaned, lga_map_data, by = c("suburb" = "suburb_name"))
  
  # Reshape the data to long format and filter by the selected year and country
  final_db_long_formatted <- final_db_cleaned %>%
    pivot_longer(cols = starts_with("year_"), names_to = "Year", values_to = "Population") %>%
    mutate(Year = as.integer(stringr::str_extract(Year, "\\d{4}"))) %>%
    filter(Year == as.integer(year_selected), !is.na(Year))
  
  if (country_selected != "All") {
    final_db_long_formatted <- final_db_long_formatted %>%
      filter(country_by_birth == country_selected)
  }
  
  # Debug: Print column names before join
  print(names(final_db_long_formatted))
  
  # Load and transform the shapefile
  victoria_shapefile_loaded <- st_read(shapefile_path_specific)
  victoria_shapefile_transformed <- st_transform(victoria_shapefile_loaded, 4326)
  victoria_shapefile_renamed <- victoria_shapefile_transformed %>%
    rename(LGA_Code = LGA_COD)  # Make sure the renaming is correct
  
  
  # Debug: Print column names before join
  print(names(victoria_shapefile_renamed))
  
  # Join the final_db_long_formatted data directly to the shapefile data
  victoria_data_merged <- left_join(victoria_shapefile_renamed, final_db_long_formatted, by = "LGA_Code")
  victoria_data_merged_filtered <- victoria_data_merged %>% filter(!is.na(Year))
  
  return(victoria_data_merged_filtered)
}


