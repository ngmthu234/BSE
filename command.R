beef_2022 <- read.csv("~/Documents/BSE project/cow_inventory_beef_2022.csv")

library(tidyverse)
beef_2022 <- beef_2022 %>% dplyr::filter(Value != " (D)") %>% mutate(Value = gsub(",", "", Value)) %>% mutate(Value = as.numeric(Value))
