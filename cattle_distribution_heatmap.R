# Load csv file into R
file_path <- "C:/Users/tmn0031/OneDrive - Auburn University/Bovine spongiform encephalopathy/cow_inventory_beef_2022.csv"
# Change "~/Documents/BSE project/cow_inventory_beef_2022.csv" with your actual path
raw <- read.csv(file_path)

# Load necessary libraries
library(tidyverse)     # to manipulate data
library(tigris)     # to download spatial data
library(sf)     # to manipulate spatial data
library(rmapshaper)     # (optional) to simplify spatial data
library(scales)     # to control appearance of legend labels

# Data cleanup #1
beef <- subset(raw, select = -c(Program, Year, Period, Week.Ending, Geo.Level, State.ANSI, Ag.District, Ag.District.Code, County.ANSI, Zip.Code, Region, watershed_code, Watershed, Commodity, Data.Item, Domain, Domain.Category, CV....))     # to remove unnecessary columns
beef <- beef %>% 
     dplyr::filter(Value != " (D)") %>%     # to remove rows with "(D)" in column Value
     mutate(Value = gsub(",", "", Value)) %>%     # to globally substitute "," to empty strings in column Value, preparing for numerical conversion
     mutate(Value = as.numeric(Value))     # to convert strings in column Value into numbers, allowing arithmetic calculations 

# Download US county and state shapefiles
counties_sf <- counties(cb = TRUE, resolution = "20m", year = 2022)
states_sf <- states(cb = TRUE, resolution = "20m", year = 2022)

# Data cleanup #2: Remove Alaska, District of Columbia, Hawaii, and U.S. territories
beef <- beef %>% dplyr::filter(State != "ALASKA") %>% dplyr::filter(State != "HAWAII")
counties_sf <- counties_sf %>% dplyr::filter(STUSPS != "AK") %>% dplyr::filter(STUSPS != "DC") %>% dplyr::filter(STUSPS != "HI")
states_sf <- states_sf %>% dplyr::filter(STUSPS != "AK") %>% dplyr::filter(STUSPS != "AS") %>% dplyr::filter(STUSPS != "DC") %>% dplyr::filter(STUSPS != "GU") %>% dplyr::filter(STUSPS != "HI") %>% dplyr::filter(STUSPS != "MP") %>% dplyr::filter(STUSPS != "PR") %>% dplyr::filter(STUSPS != "VI")

# Prepare data for merge()
beef$County <- tolower(beef$County)
beef$State <- tolower(beef$State)
counties_sf$NAME <-tolower(counties_sf$NAME)
counties_sf$STATE_NAME <-tolower(counties_sf$STATE_NAME)

# Merge dataframes
beef_map <- merge(counties_sf, beef, by.x = c("STATE_NAME", "NAME"), by.y = c("State", "County"), all.x = TRUE) %>% 
     st_as_sf()     # to convert output from merge() into a spatial data frame

# Obtain centroids of U.S. states for state labeling
library(usmap)      # to obtain U.S. map
state_centroid <- usmapdata::centroid_labels("states")     # to obtain centroids of U.S. states
state_centroid <- state_centroid %>% dplyr::filter(full != "Alaska") %>% dplyr::filter(full != "District of Columbia") %>% dplyr::filter(full != "Hawaii")     # to remove Alaska, District of Columbia, and Hawaii

# Plot heatmap of beef distribution across the U.S. only 
ggplot() + geom_sf(data = beef_map, aes(fill = `Value`), color = "gray30", size = 5) 
     + geom_sf(data = states_sf, fill = NA, color = "black", size = 5) 
     + geom_sf_text (data = state_centroid, aes(label = abbr), color = "white", size = 3.5, fontface = "bold") 
     + coord_sf(crs = st_crs ("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96")) 
     + scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "gray90", label = comma) 
     + labs(title = "Beef cattle population by U.S. counties", fill = "Inventory (Animal heads)") 
     + theme_void()

# Create dataframe of states with positive BSE cases
BSE_state_name <- c("Texas", "Alabama", "California", "Florida", "South Carolina")     # to create a vector with names of states with positive BSE cases
BSE_state <- state_centroid %>% dplyr::filter(NAME %in% BSE_state_name) %>%     # to filter rows under NAME of us_state, keeping only rows with values matching BSE_state_name
     mutate(bse_status = "State with positive BSE case(s)")     # to create a new column bse_status with each row value set to be "State with positive BSE case(s)"

# Plot heatmap of beef distribution across the U.S. with outlines of states with positive BSE cases highlighted. No counties 
beef_state <- beef %>% group_by(State) %>% summarize("Value" = sum(Value)) %>% dplyr::filter(State != "ALASKA") %>% dplyr::filter(State != "HAWAII")
beef_map_state <- merge(states_sf, beef_state, by.x = "NAME", by.y = "State", all.x = TRUE) %>% st_as_sf()
ggplot() + 
     geom_sf(data = beef_map_state, aes(fill = `Value`), color = "black", size = 5) + 
     geom_sf(data = BSE_state, aes(color = bse_status), fill = NA, linewidth = 1.5) + 
     scale_color_manual(name = NULL, values = c("State with positive BSE case(s)" = "red")) + 
     geom_sf_text (data = state_centroid, aes(label = abbr), color = "white", size = 3.5, fontface = "bold") + 
     coord_sf(crs = st_crs ("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96")) + 
     scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "gray90", label = comma) + 
     labs(title = "Beef cattle population by U.S. states", fill = "Inventory (Animal heads)") + 
     theme_void()
