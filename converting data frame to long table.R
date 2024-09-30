rm(list=ls())
# Load required library
library(tidyr)
library(dplyr)
require(igraph)

# Step 1: Read the biadjacency matrix from Excel
df.epi <- read.csv("Epi_All_Occ.csv", header = TRUE, row.names=1)
df.basio <- read.csv("Basio_All_Occ.csv", header = TRUE, row.names=1)


My_graph <- graph_from_incidence_matrix(df,weighted = TRUE)

# Convert row names to a column
df.epi <- tibble::rownames_to_column(df.epi, var = "Species1")

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
unique_df1 <- remove_duplicates(long_table)
