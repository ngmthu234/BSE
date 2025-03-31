beef_2022 <- read.csv("~/Documents/BSE project/cow_inventory_beef_2022.csv")

library(tidyverse)
beef_2022 <- beef_2022 %>% dplyr::filter(Value != " (D)") %>% mutate(Value = gsub(",", "", Value)) %>% mutate(Value = as.numeric(Value))

beef_2022_bycounty <- beef_2022 %>% group_by(State, County) %>% summarize("Inventory (Animal heads)" = sum(Value))

library(tigris)
library(sf)
library(rmapshaper)

us_counties <- counties(cb = TRUE, resolution = "5m")
us_states <- states(cb = TRUE, resolution = "500k")

beef_2022_bycounty <- beef_2022_bycounty %>% dplyr::filter(State != "ALASKA") %>% dplyr::filter(State != "HAWAII")
us_counties <- us_counties %>% dplyr:filter(STUSPS != "AK") %>% dplyr:filter(STUSPS != "AS") %>% dplyr:filter(STUSPS != "DC") %>% dplyr:filter(STUSPS != "GU") %>% dplyr:filter(STUSPS != "HI") %>% dplyr:filter(STUSPS != "MP") %>% dplyr:filter(STUSPS != "PR") %>% dplyr:filter(STUSPS != "VI")
us_states <- us_states %>% dplyr:filter(STUSPS != "AK") %>% dplyr:filter(STUSPS != "AS") %>% dplyr:filter(STUSPS != "DC") %>% dplyr:filter(STUSPS != "GU") %>% dplyr:filter(STUSPS != "HI") %>% dplyr:filter(STUSPS != "MP") %>% dplyr:filter(STUSPS != "PR") %>% dplyr:filter(STUSPS != "VI")

beef_2022_bycounty$County <- tolower(beef_2022_bycounty$County)
beef_2022_bycounty$State <- tolower(beef_2022_bycounty$State)
us_counties$NAME <-tolower(us_counties$NAME)
us_counties$STATE_NAME <-tolower(us_counties$STATE_NAME)

beef_2022_map <- merge(us_counties, beef_2022_bycounty, by.x = c("STATE_NAME", "NAME"), by.y = c("State", "County"), all.x = TRUE) %>% st_as_sf()

names(beef_2022_map)[names(beef_2022_map) == "STATE_NAME"] <- "STATE"
names(beef_2022_map)[names(beef_2022_map) == "NAME"] <- "COUNTY"
