require(igraph)
library(rgl)
require(influential)


df=rbind(unique_df,unique_df1)
# Assuming your data frame is named 'df'
edge_list <- df[, c("Species1", "Species2", "Value")]

# Convert to a graph object using igraph
library(igraph)
graph <- graph_from_data_frame(edge_list, directed = FALSE)

# Plot the graph (optional)
plot(graph)


# Get unique species names
all_species <- unique(c(df$Species1, df$Species2))

# Create an empty adjacency matrix
adj_matrix <- matrix(0, nrow = length(all_species), ncol = length(all_species), dimnames = list(all_species, all_species))

# Fill in the adjacency matrix
for (i in 1:nrow(df)) {
  row <- df[i, ]
  adj_matrix[row$Species1, row$Species2] <- row$Value
  adj_matrix[row$Species2, row$Species1] <- row$Value  # If the graph is undirected
}

plot(graph1,
     layout = layout_with_fr(graph),  # Use Fruchterman-Reingold layout for better visualization
     vertex.size = 10,  # Adjust node size
     vertex_colors= "lightblue",  # Adjust node size
     vertex.label.cex = 0.8,  # Adjust label size
     vertex.label.color = "black",  # Set label color
     vertex.label.family = "sans",  # Set label font family
     edge.width = 2,  # Adjust edge thickness
     edge.color = "gray"  # Set edge color
)

tkid <- tkplot(graph) #tkid is the id of the tkplot that will open
l <- tkplot.getcoords(tkid) # grab the coordinates from tkplot
tk_close(tkid, window.close = T)
plot(graph, layout=l)

###community detection
ceb <- cluster_edge_betweenness(graph) 
plot(ceb, graph) 

clp <- cluster_label_prop(graph)

plot(clp, graph)




#### Calcularing IVI
My_graph <- graph_from_incidence_matrix(graph,weighted = TRUE)
Graph_IVI <- ivi(graph = graph, mode = "all")

#### Plotting (change shape parameter for each network)
cent_network.vis(graph = graph, cent.metric = Graph_IVI,
                 legend.title = "Integrated Value of Influence",
                 plot.title = "", layout= "kk", dist.power=2, legend.position="bottom",legend.direction = "horizontal", node.size.min = 3, boxed.legend=FALSE, show.labels=TRUE)


sil.links <- test_interactions(df, signif.level = 0.95)
