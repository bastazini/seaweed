rm(list=ls())
# Load required library
library(tidyr)
library(dplyr)

# Step 1: Read the biadjacency matrix from Excel
df <- read.csv("Basio_All_Occ.csv", header = TRUE, row.names=1)

# Convert row names to a column
df <- tibble::rownames_to_column(df, var = "Species1")

# Convert the data frame to long format
long_table <- df %>%
  pivot_longer(cols = -Species1, names_to = "Species2", values_to = "Value") %>%
  filter(Value != 0)  # Optionally, filter out rows with zero values

# Print the long table
print(long_table)

###remove duplicates form longtable
remove_duplicates <- function(data) {
  unique_data <- unique(data)
  return(unique_data)
}

# Example usage
# Assuming your data frame is named 'df'
unique_df <- remove_duplicates(long_table)
