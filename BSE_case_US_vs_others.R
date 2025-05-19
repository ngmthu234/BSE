# Load necessary libraries
library (tidyverse)     # to manipulate data
library(readxl)     # to read excel file

# Load xlsx file into R
file_path <- "C:/Users/tmn0031/OneDrive - Auburn University/Bovine spongiform encephalopathy/US vs Canada vs EU (annual, 2003-present).xlsx"
# Change "C:/Users/tmn0031/OneDrive - Auburn University/Bovine spongiform encephalopathy/US vs Canada vs EU (annual, 2003-present).xlsx" with your actual file path
BSE_case <- read_excel(file_path, sheet = "raw")

# Data cleanup #1
BSE_case <- BSE_case %>%
     rename(Country = ...1) %>%     # to remanme column 1 to column "Country"
     dplyr::filter(Country != "EU total") %>%    # to remove row EU total
     dplyr::mutate(`2003` = round(`2003), `2004` = round(`2004`), `2005` = round(`2005`), `2006` = round(`2006`), `2007` = round(`2007`), `2008` = round(`2008`), `2009` = round(`2009`), `2010` = round(`2010`)     # to round numbers in columns 2003 to 2010 to the nearest integer 

# Format data for ggplot()
format <- BSE_case %>%
     pivot_longer(cols = `2003`:`2023`, names_to = "Year", values_to = "Cases)     # to pivot the data from wide to long format

# Plot line chart of BSE cases from 2003 to 2023
ggplot(format, aes(x = Year, y = Cases, color = Country, group = Country) + geom_line(linewidth = 1) + labs(title = "BSE cases in Canada, Japan, Europe, and the United States between 2003 and 2023", x = "Year", y = "Number of cases", color = "")
