library(dplyr)
setwd("~/Code/Birds")
source("code/new_code/bird-level-transitions.R")

# Create edge list: one row = one bird visited one nest (at least once)
edge_list <- logger_spra %>%
  filter(!is.na(Kring), !is.na(nest)) %>%
  distinct(Kring, nest)

edge_list_weighted <- logger_spra %>%
  filter(!is.na(Kring), !is.na(nest)) %>%
  count(Kring, nest, name = "weight")

library(igraph)

# Create vertex types: TRUE for birds, FALSE for nests
birds <- unique(edge_list$Kring)
nests <- unique(edge_list$nest)

vertices <- data.frame(
  name = c(birds, nests),
  type = c(rep(TRUE, length(birds)), rep(FALSE, length(nests)))
)

# Bipartite graph
g_bipartite <- graph_from_data_frame(edge_list, vertices = vertices, directed = FALSE)

# Add weight if using weighted edge list
# g_bipartite <- graph_from_data_frame(edge_list_weighted, vertices = vertices, directed = FALSE)


library(ggraph)
library(tidygraph)

# Convert to tidygraph object
g_tbl <- as_tbl_graph(g_bipartite)

# Bipartite plot
ggraph(g_tbl, layout = "bipartite") +
  geom_edge_link(alpha = 0.4) +
  geom_node_point(aes(color = type), size = 2) +
  geom_node_text(aes(label = name), size = 2, repel = TRUE) +
  theme_minimal()



# Project to bird-bird network based on shared nests
bird_graph <- bipartite_projection(g_bipartite, which = "true")

# Edge weight = number of shared nests
E(bird_graph)$weight



# Community detection on projected bird network
comms <- cluster_louvain(bird_graph)

# Assign communities to vertices
V(bird_graph)$community <- comms$membership

# Summary of community sizes
table(V(bird_graph)$community)

bipartite_graph <- graph_from_data_frame(edge_df, directed = FALSE)
V(bipartite_graph)$type <- V(bipartite_graph)$name %in% birds  # TRUE = bird, FALSE = nest
nest_projection <- bipartite_projection(bipartite_graph)$proj2

