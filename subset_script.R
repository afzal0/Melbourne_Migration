# Load necessary libraries
library(dplyr)
library(sf)

# Load the RDS file
data <- readRDS("dominant_country_data_combined.rds")

# Remove specific columns
data_cleaned <- data %>%
  select(-c(STE_COD, STE_NAM, AUS_COD, AUS_NAM, AREASQK, LOCI_UR))

# Convert to regular data frame
data_df <- st_drop_geometry(data_cleaned)

# Determine the size of the subset (20% of the total)
set.seed(123) # Setting seed for reproducibility
subset_size <- round(0.2 * nrow(data_df))

# Randomly sample 35% of the data
subset_data_df <- data_df %>% sample_n(subset_size)

# Get the corresponding geometry for the sampled data
subset_data <- data_cleaned %>% filter(LGA_Code %in% subset_data_df$LGA_Code)

# Save the subset to a new RDS file
saveRDS(subset_data, "subset_dominant_country_data_combined.rds")

