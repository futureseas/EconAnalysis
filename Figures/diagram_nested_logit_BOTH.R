gc()
rm(list = ls())
library(data.tree)
library(tidygraph)
library(ggraph)
library(ggplot2)

# Function to convert a data.tree structure into an edge list
tree_to_df <- function(node) {
  edges <- data.frame(from = character(), to = character(), stringsAsFactors = FALSE)
  
  node$Do(function(x) {
    if (!x$isRoot) {
      edges <<- rbind(edges, data.frame(from = x$parent$name, to = x$name, stringsAsFactors = FALSE))
    }
  }, traversal = "pre-order")
  
  return(edges)
}

# First Graph (Species-based)
vessel1 <- Node$new("Vessel\n(Species-based)")
part1 <- vessel1$AddChild("Participate")
msqd1 <- part1$AddChild("Species 1")
msqd1$AddChild("Port 1")
msqd1$AddChild("Port 2")
msqd1$AddChild("Port 4")
cmck1 <- part1$AddChild("Species 2")
cmck1$AddChild("Port 2")
cmck1$AddChild("Port 3")
nanc1 <- part1$AddChild("Species 3")
nanc1$AddChild("Port 1")
nanc1$AddChild("Port 2")
nanc1$AddChild("Port 4")
nopart1 <- vessel1$AddChild("No\nparticipation")
nopart1$AddChild("None")$AddChild("None")

# Second Graph (Port-based)
vessel2 <- Node$new("Vessel\n(Port-based)")
part2 <- vessel2$AddChild("Participate")
msqd2 <- part2$AddChild("Port 1")
msqd2$AddChild("Species 1")
msqd2$AddChild("Species 2")
msqd2$AddChild("Species 4")
cmck2 <- part2$AddChild("Port 2")
cmck2$AddChild("Species 2")
cmck2$AddChild("Species 3")
nanc2 <- part2$AddChild("Port 3")
nanc2$AddChild("Species 1")
nanc2$AddChild("Species 2")
nanc2$AddChild("Species 4")
nopart2 <- vessel2$AddChild("No\nparticipation")
nopart2$AddChild("None")$AddChild("None")

# Convert trees to data frames
edges1 <- tree_to_df(vessel1)
edges2 <- tree_to_df(vessel2)

# Convert data frames to graph objects
graph1 <- as_tbl_graph(edges1)
graph2 <- as_tbl_graph(edges2)

# Plot the first tree (Species-based)
p1 <- ggraph(graph1, layout = "tree") +
  geom_edge_link(aes(), arrow = arrow(length = unit(4, 'mm')), end_cap = circle(4, 'mm')) +
  geom_node_point(size = 5, color = "skyblue") +
  geom_node_text(aes(label = name), repel = TRUE, size = 4) +
  ggtitle("Species-Based Hierarchy") +
  theme_void()

# Plot the second tree (Port-based)
p2 <- ggraph(graph2, layout = "tree") +
  geom_edge_link(aes(), arrow = arrow(length = unit(4, 'mm')), end_cap = circle(4, 'mm')) +
  geom_node_point(size = 5, color = "lightcoral") +
  geom_node_text(aes(label = name), repel = TRUE, size = 4) +
  ggtitle("Port-Based Hierarchy") +
  theme_void()

# Combine both ggplot graphs using patchwork
library(patchwork)
p1 + p2
