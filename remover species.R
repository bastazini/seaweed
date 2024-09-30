# Assuming 'graph' is your igraph object and 'species_to_remove' is the name of the species you want to remove
graph1=graph


# Find the index of the species node you want to remove
species_index <- which(V(graph1)$name == "Corallina_officinalis")

# If the species exists in the graph
if (length(species_index) > 0) {
  # Remove the species and its incident edges
  graph1 <- delete_vertices(graph1, species_index)
} else {
  # Species not found in the graph
  print("Species not found in the graph.")
}
graph1


Graph_IVI <- ivi(graph = graph1, mode = "all")

#### Plotting (change shape parameter for each network)
cent_network.vis(graph = graph1, cent.metric = Graph_IVI,
                 legend.title = "Integrated Value of Influence",
                 plot.title = "", layout= "kk", dist.power=2, legend.position="bottom",legend.direction = "horizontal", node.size.min = 3, boxed.legend=FALSE, show.labels=TRUE)


# Compute the generalized indicator matrix
###transform im matrix
adj_matrix <- as_adjacency_matrix(graph, sparse = FALSE)  # set sparse to TRUE if you want a sparse matrix
ind_matrix <- GenInd(adj_matrix)
ind_matrix
# Calculate the sum of each row in the indicator matrix
row_sums <- rowSums(ind_matrix)

# Calculate the degree of each node in the network
node_degrees <- degree(test1)

# Replace 0 degrees with 1 to avoid division by zero
node_degrees[node_degrees == 0] <- 1

# Calculate the fraction of neighbors belonging to the same community
fractions <- row_sums / node_degrees

# Replace infinite values with 0
fractions[is.infinite(fractions)] <- 0

# Calculate the average fraction, excluding NaN values
cbar <- mean(fractions, na.rm = TRUE)