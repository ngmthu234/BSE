# Load csv file into R
file_path <- "C:/Users/tmn0031/OneDrive - Auburn University/Bovine spongiform encephalopathy/cow_inventory_beef_2022.csv"
# Change "~/Documents/BSE project/cow_inventory_beef_2022.csv" with your actual path
beef_2022 <- read.csv(file_path)

# Load necessary libraries
library(tidyverse)     # to manipulate data
library(tigris)     # to download spatial data
library(sf)     # to manipulate spatial data
library(rmapshaper)     # (optional) to simplify spatial data

# Data cleanup #1
beef_2022 <- subset(beef_2022, select = -c(Program, Year, Period, Week.Ending, Geo.Level, State.ANSI, Ag.District, Ag.District.Code, County.ANSI, Zip.Code, Region, watershed_code, Watershed, Commodity, Data.Item, Domain, Domain.Category, CV....))     # to remove unnecessary columns
beef_2022 <- beef_2022 %>% 
     dplyr::filter(Value != " (D)") %>%     # to remove rows with " (D)" in column Value
     mutate(Value = gsub(",", "", Value)) %>%     # to globally substitute "," to empty strings in column Value, preparing for numerical conversion
     mutate(Value = as.numeric(Value))     # to convert strings in column Value into numbers, allowing arithmetic calculations 

# Data cleanup #2 
beef_2022_bycounty <- beef_2022 %>% 
     group_by(State, County) %>%
     summarize("Inventory (Animal heads)" = sum(Value))     # to create new column "Inventory (Animal heads)" summarizing total befef for each county

# Download US county and state shapefiles
us_counties <- counties(cb = TRUE, resolution = "20m", year = 2024)
us_states <- states(cb = TRUE, resolution = "5m")

# Data cleanup #3: Remove Alaska, Hawaii, District of Columbia, and U.S. territories
beef_2022_bycounty <- beef_2022_bycounty %>% dplyr::filter(State != "ALASKA") %>% dplyr::filter(State != "HAWAII")
us_counties <- us_counties %>% dplyr::filter(STUSPS != "AK") %>% dplyr::filter(STUSPS != "DC") %>% dplyr::filter(STUSPS != "HI")
us_states <- us_states %>% dplyr::filter(STUSPS != "AK") %>% dplyr::filter(STUSPS != "AS") %>% dplyr::filter(STUSPS != "DC") %>% dplyr::filter(STUSPS != "GU") %>% dplyr::filter(STUSPS != "HI") %>% dplyr::filter(STUSPS != "MP") %>% dplyr::filter(STUSPS != "PR") %>% dplyr::filter(STUSPS != "VI")

# Prepare data for merge()
beef_2022_bycounty$County <- tolower(beef_2022_bycounty$County)
beef_2022_bycounty$State <- tolower(beef_2022_bycounty$State)
us_counties$NAME <-tolower(us_counties$NAME)
us_counties$STATE_NAME <-tolower(us_counties$STATE_NAME)

# Merge dataframes
beef_2022_map <- merge(us_counties, beef_2022_bycounty, by.x = c("STATE_NAME", "NAME"), by.y = c("State", "County"), all.x = TRUE) %>% 
     st_as_sf()     # to convert output from merge() into a spatial data frame

# Plot heatmap of beef distribution across the U.S. only 
ggplot() + geom_sf(data = beef_2022_map, aes(fill = `Inventory (Animal heads)`), color = "gray30", size = 0.1) + geom_sf(data = us_states, fill = NA, color = "black", size = 5) + coord_sf(crs = st_crs ("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96")) + scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "gray90") + labs(title = "Beef cattle population by U.S. counties", fill = "Inventory (Animal heads)") + theme_void()

# Create dataframe of states with positive BSE cases
BSE_state_name <- c("Texas", "Alabama", "California", "Florida", "South Carolina")     # to create a vector with names of states with positive BSE cases
BSE_state <- us_states %>% dplyr::filter(NAME %in% BSE_state_name) %>%     # to filter rows under NAME of us_state, keeping only rows with values matching BSE_state_name
     mutate(bse_status = "State with positive BSE case(s)")     # to create a new column bse_status with each row value set to be "State with positive BSE case(s)"

# Plot heatmap of beef distribution across the U.S. with pins pointing at states with positive BSE cases
BSE_state_centroid <- st_centroid(BSE_state)     # to calculate the centroid of target state to place the pin
ggplot() + geom_sf(data = beef_2022_map, aes(fill = `Inventory (Animal heads)`), color = "gray30", size = 0.1) + geom_sf(data = us_states, fill = NA, color = "black", size = 5) + geom_sf(data = BSE_state_centroid, aes(color = bse_status, shape = bse_status), size = 5) + scale_color_manual(name = NULL, values = c("State with positive BSE case(s)" = "red")) + scale_shape_manual(name = NULL, values = c("State with positive BSE case(s)" = 17)) + coord_sf(crs = st_crs ("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96")) + scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "gray90") + labs(title = "Beef cattle population by U.S. counties", fill = "Inventory (Animal heads)") + theme_void()

# Plot heatmap of beef distribution across the U.S. with outlines of states with positive BSE cases highlighted 
ggplot() + geom_sf(data = beef_2022_map, aes(fill = `Inventory (Animal heads)`), color = "gray30", size = 0.1) + geom_sf(data = us_states, fill = NA, color = "black", size = 5) + geom_sf(data = BSE_state, aes(color = bse_status), fill = NA, linewidth = 1.5) + scale_color_manual(name = NULL, values = c("State with positive BSE case(s)" = "red")) + coord_sf(crs = st_crs ("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96")) + scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "gray90") + labs(title = "Beef cattle population by U.S. counties", fill = "Inventory (Animal heads)") + theme_void()

# Plot heatmap of beef distribution across the U.S. with outlines of states with positive BSE cases highlighted. No counties 
beef_2022_bystate <- beef_2022 %>% group_by(State) %>% summarize("Inventory (Animal heads)" = sum(Value)) %>% dplyr::filter(State != "ALASKA") %>% dplyr::filter(State != "HAWAII")
beef_2022_bystate$State <- tolower(beef_2022_bystate$State)
beef_2022_map2 <- merge(us_states, beef_2022_bystate, by.x = "NAME", by.y = "State", all.x = TRUE) %>% st_as_sf()
library(scales)     # to control appearance of legend labels
ggplot() + geom_sf(data = beef_2022_map2, aes(fill = `Inventory (Animal heads)`), color = "gray30", size = 0.1) + geom_sf(data = us_states, fill = NA, color = "black", size = 5) + geom_sf(data = BSE_state, aes(color = bse_status), fill = NA, linewidth = 1.5) + scale_color_manual(name = NULL, values = c("State with positive BSE case(s)" = "red")) + coord_sf(crs = st_crs ("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96")) + scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "gray90", label = comma) + labs(title = "Beef cattle population by U.S. states", fill = "Inventory (Animal heads)") + theme_void()
