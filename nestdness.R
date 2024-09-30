
#NODF folowing Fortuna, M.A., Barbour, M.A., Zaman, L., Hall, A.R., Buckling, A. and Bascompte, J.: Coevolutionary dynamics shape the structure of bacteria‐phage infection networks. Evolution 1001-1011

library(NetIndices)

compute_nestedness <- function(B){
  
  # Get number of rows and columns
  nrows <- nrow(B)
  ncols <- ncol(B)
  
  # Compute nestedness of rows
  nestedness_rows <- 0
  for(i in 1:(nrows-1)){
    for(j in (i+1): nrows){
      
      c_ij <- sum(B[i,] * B[j,])      # Number of interactions shared by i and j
      k_i <- sum(B[i,])               # Degree of node i
      k_j <- sum(B[j,])               # Degree of node j
      
      if (k_i == 0 || k_j==0) {next}  # Handle case if a node is disconnected
      
      o_ij <- c_ij / min(k_i, k_j)    # Overlap between i and j
      
      nestedness_rows <- nestedness_rows + o_ij
    }
  }
  
  # Compute nestedness of columns
  nestedness_cols <- 0
  for(i in 1: (ncols-1)){
    for(j in (i+1): ncols){
      
      c_ij <- sum(B[,i] * B[,j])      # Number of interactions shared by i and j
      k_i <- sum(B[,i])               # Degree of node i
      k_j <- sum(B[,j])               # Degree of node j
      if (k_i == 0 || k_j==0) {next}  # Handle case if a node is disconnected.
      
      o_ij <- c_ij / min(k_i, k_j)    # Overlap between i and j
      
      nestedness_cols <- nestedness_cols + o_ij         
    }
  }
  
  # Compute nestedness of the network
  nestedness <- (nestedness_rows + nestedness_cols) / ((nrows * (nrows - 1) / 2) + (ncols * (ncols - 1) / 2))
  
  return(nestedness)
}

###transform to adjacency matrix
adjacency_matrix= as.matrix(graph)
compute_nestedness(adjacency_matrix)


####degree distribution
test<-get.adjacency(graph,sparse=F)
degree_data<-degree(test)

# Create a data frame for ggplot
degree_df <- data.frame(
  species = V(graph)$name,
  degree = degree_data
)

# Plot the bar plot using ggplot2
ggplot(degree_df, aes(x = reorder(species, degree), y = degree)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Species Degree Distribution",
       x = "Degree",
       y = "Species") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  coord_flip()

###50 species higher degree
degree_df <- degree_df[order(-degree_df$degree), ][1:50, ]

# Normalize degree values for better visualization
degree_values_normalized <- degree_values / max(degree_values)

# Plot the network, adjusting node size based on betweenness centrality
layout <- layout_with_fr(graph)
plot(graph, layout=layout, vertex.size = degree_values_normalized * 20, vertex.color = "blue")



#Degree distribution is the cumulative frequency of nodes with a given degree
# this, like degree() can be specified as "in", "out", or "all"
deg.distr<-degree.distribution(graph,cumulative=T,mode="all")

# Using the power.law.fit() function I can fit a power law to the degree distribution
fit<-power.law.fit(deg.distr)

# Then I can plot the degree distribution
plot(deg.distr,log="xy",
     ylim=c(.01,10),
     bg="black",pch=21,
     xlab="Degree",
     ylab="Cumulative Frequency")


# And the expected power law distribution
lines(1:200,10*(1:200)^((-power$alpha)+1),col = "red", lwd = 2)

###properties
test.graph.properties<-GenInd(test)
###compartmentalization, [0,1], the degree of connectedness of subsystems within a network.
test.graph.properties$Cbar
###conectance
test.graph.properties$C


###Ploting network index
df <- data.frame(Index = c("Connectance","NODF", "Link Density","Compartmentalization", "Evenness", "Complementarity specialisation"),
                 Value = c(0.02575258,0.5836338,6.850187,0.1429926, 0.2880889,0.85))
df

library(ggplot2)

ggplot(df, aes(x = Index, y = Value)) +
  geom_segment(aes(x = Index, xend = Index, y = 0, yend = Value)) +
  geom_point(size = 4, pch = 21, bg = 4, col = 1) +
  coord_flip()


