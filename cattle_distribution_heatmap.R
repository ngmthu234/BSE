beef_2022 <- read.csv("~/Documents/BSE project/cow_inventory_beef_2022.csv")
# Change "~/Documents/BSE project/cow_inventory_beef_2022.csv" with your actual path

library(tidyverse)
beef_2022 <- beef_2022 %>% dplyr::filter(Value != " (D)") %>% mutate(Value = gsub(",", "", Value)) %>% mutate(Value = as.numeric(Value))

beef_2022_bycounty <- beef_2022 %>% group_by(State, County) %>% summarize("Inventory (Animal heads)" = sum(Value))

library(tigris)
library(sf)
library(rmapshaper)

us_counties <- counties(cb = TRUE, resolution = "20m")
us_states <- states(cb = TRUE, resolution = "5m")

beef_2022_bycounty <- beef_2022_bycounty %>% dplyr::filter(State != "ALASKA") %>% dplyr::filter(State != "HAWAII")
us_counties <- us_counties %>% dplyr::filter(STUSPS != "AK") %>% dplyr::filter(STUSPS != "AS") %>% dplyr::filter(STUSPS != "DC") %>% dplyr::filter(STUSPS != "GU") %>% dplyr::filter(STUSPS != "HI") %>% dplyr::filter(STUSPS != "MP") %>% dplyr::filter(STUSPS != "PR") %>% dplyr::filter(STUSPS != "VI")
us_states <- us_states %>% dplyr::filter(STUSPS != "AK") %>% dplyr::filter(STUSPS != "AS") %>% dplyr::filter(STUSPS != "DC") %>% dplyr::filter(STUSPS != "GU") %>% dplyr::filter(STUSPS != "HI") %>% dplyr::filter(STUSPS != "MP") %>% dplyr::filter(STUSPS != "PR") %>% dplyr::filter(STUSPS != "VI")

beef_2022_bycounty$County <- tolower(beef_2022_bycounty$County)
beef_2022_bycounty$State <- tolower(beef_2022_bycounty$State)
us_counties$NAME <-tolower(us_counties$NAME)
us_counties$STATE_NAME <-tolower(us_counties$STATE_NAME)

beef_2022_map <- merge(us_counties, beef_2022_bycounty, by.x = c("STATE_NAME", "NAME"), by.y = c("State", "County"), all.x = TRUE) %>% st_as_sf()

# To plot heatmap of beef distribution across the U.S. 
ggplot() + geom_sf(data = beef_2022_map, aes(fill = `Inventory (Animal heads)`), color = "gray30", size = 1) + geom_sf(data = us_states, fill = NA, color = "black", size = 500) + coord_sf(crs = st_crs ("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96")) + scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "gray90") + labs(title = "Beef cattle population by U.S. county", fill = "Inventory (Animal heads)") + theme_void()

# To add pin pointing at states with positive BSE cases (lines 32 - 38)

BSE_state_name <- c("Texas", "Alabama", "California", "Florida", "South Carolina")

BSE_state <- us_states %>% dplyr::filter(NAME %in% BSE_state_name) %>% mutate(bse_status = "State with positive BSE case(s)")

BSE_state_centroid <- st_centroid(BSE_state)

ggplot() + geom_sf(data = beef_2022_map, aes(fill = `Inventory (Animal heads)`), color = "gray30", size = 0.1) + geom_sf(data = us_states, fill = NA, color = "black", size = 5) + geom_sf(data = BSE_state_centroid, aes(color = bse_status, shape = bse_status), size = 5) + scale_color_manual(name = NULL, values = c("State with positive BSE case(s)" = "red")) + scale_shape_manual(name = NULL, values = c("State with positive BSE case(s)" = 17)) + coord_sf(crs = st_crs ("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96")) + scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "gray90") + labs(title = "Beef cattle population by U.S. county", fill = "Inventory (Animal heads)") + theme_void()
