setwd("/Users/pranavsuresh/Code/Birds")

library(readxl)
library(dplyr)
library(stringr)
library(readr)
library(igraph)

#load in mated pair info
nest_pairs <- read_excel("data/bird-nest/pairs_nest.xlsx")

#just the mated pairs (edges)

mate_edges <- nest_pairs %>%
  filter(!is.na(m), !is.na(f)) %>%
  distinct(m, f)

#preview
head(mate_edges)


mate_edges <- mate_edges %>%
  mutate(
    id12 = paste0(m, f),
    id21 = paste0(f, m)
  )

##load relatedness table

relate <- read_csv("data/bird-bird/Relateness_weaver.csv") %>%
  rename(id1 = ind1.id, id2 = ind2.id) %>%
  mutate(
    id1 = str_replace(id1, "^.*_", ""),
    id2 = str_replace(id2, "^.*_", ""),
    id12 = paste0(id1, id2),
    id21 = paste0(id2, id1),
    r = wang
  )

##annotate mated pairs with kinship

mate_edges <- mate_edges %>%
  left_join(relate %>% select(id12, r), by = "id12") %>%
  mutate(
    r = ifelse(is.na(r), relate$r[match(id21, relate$id12)], r)
  )

summary(mate_edges$r)

#proportion of mated pairs that are close kin
mean(mate_edges$r > 0.125, na.rm = TRUE)




##visualization

g <- graph_from_data_frame(mate_edges, directed = FALSE)

# Color by relatedness threshold
V(g)$color <- "gray"
V(g)$color[V(g)$name %in% mate_edges$m[mate_edges$r > 0.25]] <- "red"
high_kin_ids <- unique(c(mate_edges$m[mate_edges$r > 0.25], mate_edges$f[mate_edges$r > 0.25]))
V(g)$color[V(g)$name %in% high_kin_ids] <- "red"

# Use Fruchterman-Reingold layout for better spacing
layout <- layout_with_fr(g)

# Plot without vertex labels and spaced out layout
plot(
  g,
  layout = layout,
  vertex.label = NA,         # Remove labels
  vertex.size = 8,           # Adjust size as needed
  edge.width = 1,            # Make edges visible but clean
  main = "Mated Pair Network with High-Kinship Highlighted"
)
