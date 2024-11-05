# Load required libraries
library(dplyr)
library(tidyr)
library(stringr)
library(sf)
library(ggplot2)

# Assume the custom functions are sourced and available
source("Custom_function.R")

# Define the paths to the required files
lga_map_path <- "LGA_map.csv"
final_db_path <- "Final_DB.csv"
shapefile_path <- "GDA23/filtered_shapefile_Victoria.shp"

# Define the years to iterate over
years <- c("2001", "2006", "2011", "2016", "2021")

# Process data for dominant migrant population
dominant_data_list <- lapply(years, function(year) {
  process_migration_data_dominant(lga_map_path, final_db_path, shapefile_path, year)
})

# Combine the dominant data into a single data frame
dominant_data_combined <- do.call(rbind, dominant_data_list)

# Process data for total migration
total_data <- process_migration_data_total(lga_map_path, final_db_path, shapefile_path)

# Get the unique countries from the Final_DB.csv file
countries <- read.csv(final_db_path) %>%
  distinct(country_by_birth) %>%
  pull(country_by_birth)

# Process data for all countries
dominant_country_data_list <- lapply(countries, function(country) {
  lapply(years, function(year) {
    process_migration_data_dominant_country(lga_map_path, final_db_path, shapefile_path, year, country)
  })
})

# Combine the dominant country data into a single data frame
dominant_country_data_combined <- do.call(rbind, lapply(dominant_country_data_list, function(x) do.call(rbind, x)))

# Save the processed data as RDS files
saveRDS(dominant_data_combined, "dominant_data_combined.rds")
saveRDS(total_data, "total_data.rds")
saveRDS(dominant_country_data_combined, "dominant_country_data_combined.rds")