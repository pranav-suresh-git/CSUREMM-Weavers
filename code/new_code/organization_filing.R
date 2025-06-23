library(readxl)
library(purrr)
library(writexl)
library(readxl)
library(writexl)   # or use write.csv for CSV output
library(dplyr)

# Load the full file (adjust file path as needed)
full_data <- read_excel("~/Code/birds/data/c_2012-2017_weaver_banding.xlsx")

# Select only the desired columns
subset_data <- full_data %>% select(Date, Metal, Combo, Colony)

# Save to a new Excel file
write_xlsx(subset_data, "identification.xlsx")

# Or save to a CSV file
# write.csv(subset_data, "subset_output.csv", row.names = FALSE)
