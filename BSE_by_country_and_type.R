# Load necessary libraries
library (tidyverse)     # to manipulate data
library(readxl)     # to read excel file

# Load xlsx file into R
file_path <- "C:/Users/tmn0031/OneDrive - Auburn University/Bovine spongiform encephalopathy/US vs Canada vs EU (annual, 2003-present).xlsx"
# Change "C:/Users/tmn0031/OneDrive - Auburn University/Bovine spongiform encephalopathy/US vs Canada vs EU (annual, 2003-present).xlsx" with your actual file path
BSE_case <- read_excel(file_path, sheet = "raw")

# Data cleanup #1
BSE_case <- BSE_case %>%
     rename(Country = ...1) %>%     # to rename column 1 to column "Country"
     dplyr::filter(Country != "EU total") %>%    # to remove row(s) not to be plotted
     dplyr::mutate(`2003` = round(`2003), `2004` = round(`2004`), `2005` = round(`2005`), `2006` = round(`2006`), `2007` = round(`2007`), `2008` = round(`2008`), `2009` = round(`2009`), `2010` = round(`2010`)     # to round numbers in columns 2003 to 2010 to the nearest integer 

# Format data for ggplot()
BSE_case_format <- BSE_case %>%
     pivot_longer(cols = `2003`:`2023`, names_to = "Year", values_to = "Cases")     # to pivot the data from wide to long format

# Plot line chart of BSE cases from 2003 to 2023
ggplot(format, aes(x = Year, y = Cases, color = Country, group = Country) + geom_line(linewidth = 1) + labs(title = "BSE cases in Canada, Japan, Europe, and the United States between 2003 and 2023", x = "Year", y = "Number of cases", color = "Country")

# Plot stacked bar chart of BSE cases by country and type from 2003 to 2023
BSE_type <- read_excel(file_path, sheet = "bse-type2")
BSE_type_format <- BSE_type %>% pivot_longer(cols = c(`C-BSE`, `H-BSE`, `L-BSE), names_to = "BSE_type", values_to = "Cases")
ggplot(BSE_type_format, aes(fill = Country, y = Cases, x= Year)) + geom_bar(position = "stack", stat = "identity") + facet_wrap(~BSE_type, ncol = 1) + scale_x_continuous(breaks = 2003:2023) + scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12, 14)) + labs(title = "BSE type in Canada, Japan, and the United States between 2003 and 2023", x = "Year", y = "Number of Cases", fill = "Country") + theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "right", panel.grid.major.x = element_blank(), panel.grid.minor = element_blank ())
