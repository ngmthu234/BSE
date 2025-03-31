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

