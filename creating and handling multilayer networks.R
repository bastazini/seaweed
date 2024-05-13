# Load required library
require(mully)

# Empty network
g=mully("kelp_network",direct = F)

###add layers
g=addLayer(g, c("Epifite", "Basio", "Animal"))



#printing nodes
print_nodes <- function(data, column_index) {
  # Create an empty vector to store the species that have been printed
  printed_species <- c()
  
  for (i in 1:nrow(data)) {
    species <- as.character(data[i, column_index])
    
    # Check if the species has already been printed
    if (!(species %in% printed_species)) {
      # Print the node only if it's not a duplicate
      cat('g=addNode(g,"', species, '","Epifite")\n')
      
      # Add the species to the vector of printed species
      printed_species <- c(printed_species, species)
    }
  }
}

####printing based on row names
print_nodes(long_table,1)

print(getNodeAttributes(g))

######
print_edges <- function(data) {
  for (i in 1:nrow(data)) {
    species1 <- as.character(data[i, 1])
    species2 <- as.character(data[i, 2])
    interaction <- as.character(data[i, 3])
    cat('g=addEdge(g,"', species1, '","', species2, '"))\n', sep = "")
  }
}

# Example usage
# Assuming your data frame is named 'df'
print_edges(long_table)

print(getEdgeAttributes(g))



###Plotting networks
plot(g,layout = "scaled")
plot3d(g)
