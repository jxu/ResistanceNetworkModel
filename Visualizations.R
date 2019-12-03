
# visualizations
library(igraph)

assign_graphs <- function(num){
  assign(paste0("adj_",num), 
         graph_from_adjacency_matrix(adj_mats[,,num]))
}

graphs <- lapply(1:dim(adj_mats)[3], assign_graphs)


# plot the first six graphs
#png("time_visualizations.png", width = 1050, height = 750)
par(mfrow = c(2,3))
for(i in 1:6){
  plot(graphs[[i]],
       edge.arrow.size = 1,vertex.color = "light blue", 
       vertex.label.color="black",vertex.label.cex=2, 
       vertex.size =30, main = paste0("Time ",i), layout = layout_in_circle)
}

#dev.off()

#calculate densities of the graph 
densities <- sapply(graphs, edge_density)

library(ggplot2)
# plot densities 
ggplot(data = data.frame(Time = 1:length(densities),Density = densities), 
       aes(x = Time, y = Density)) +
  geom_point() + 
  theme_classic() + 
  labs(title = "Plot of Density over time")


#number of edges 
edges <- sapply(graphs, gsize)
# hisogram of edges
ggplot(data = data.frame(Time = 1:length(edges),Edges = edges),
       aes(x = Edges)) +
  geom_bar() + 
  theme_classic()+ 
  labs(title = "Edges Histogram")

#scatter plot similar to density for edges 
ggplot(data = data.frame(Time = 1:length(edges),Edges = edges), 
       aes(x = Time, y = Edges)) +
  geom_point(alpha = 0.2) + 
  theme_classic() + 
  labs(title = "Plot of Edges over time")
