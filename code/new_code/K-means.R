library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(readxl)
library(igraph)
library(scales)
library(ggraph)
library(tidygraph)
library(tidyr)
library(janitor)
library(tibble)
library(uwot)

##Checking ideal K means

wss <- function(k) {
  kmeans(umap_df[, c("V1", "V2")], centers = k, nstart = 10)$tot.withinss
}

k_values <- 1:10
wss_values <- sapply(k_values, wss)

plot(k_values, wss_values, type = "b",
     xlab = "Number of Clusters (k)",
     ylab = "Total Within-Cluster Sum of Squares (WSS)",
     main = "Elbow Method for Choosing k")

#

library(cluster)

silhouette_scores <- function(k) {
  km <- kmeans(umap_df[, c("V1", "V2")], centers = k, nstart = 10)
  ss <- silhouette(km$cluster, dist(umap_df[, c("V1", "V2")]))
  mean(ss[, 3])
}

sil_values <- sapply(2:10, silhouette_scores)

plot(2:10, sil_values, type = "b",
     xlab = "Number of Clusters (k)",
     ylab = "Average Silhouette Width",
     main = "Silhouette Method for Choosing k")


####################

n_days <- length(unique(logger_llod$date))
n_birds <- length(unique(logger_llod$Kring))
n_nests <- length(unique(logger_llod$nest))

set.seed(123)

n_moves_per_bird <- 5  # or draw from real data distribution

# Create a random dataset
simulated_data <- expand.grid(
  Kring = paste0("Bird", 1:n_birds),
  date = sort(unique(logger_llod$date))
) %>%
  group_by(Kring, date) %>%
  do({
    n_moves <- sample(1:n_moves_per_bird, 1)
    nests <- sample(unique(logger_llod$nest), n_moves + 1, replace = TRUE)
    tibble(
      from_nest = nests[1:n_moves],
      to_nest = nests[2:(n_moves + 1)]
    )
  }) %>%
  ungroup()


networks_per_day_sim <- split(simulated_data, simulated_data$date)

network_stats_sim <- lapply(networks_per_day_sim, function(df) {
  g <- graph_from_data_frame(df %>% select(from_nest, to_nest), directed = TRUE)
  tibble(
    density = edge_density(g),
    clustering = transitivity(g, type = "global"),
    n_nodes = gorder(g),
    n_edges = gsize(g)
  )
}) %>% bind_rows(.id = "date")


daily_summary_sim <- simulated_data %>%
  group_by(date) %>%
  summarise(
    n_transitions = n(),
    n_unique_edges = n_distinct(paste(from_nest, to_nest, sep = "_")),
    n_nests_involved = n_distinct(c(from_nest, to_nest))
  )


day_features_sim <- daily_summary_sim %>%
  left_join(network_stats_sim, by = "date") %>%
  mutate(date = as.Date(date)) %>%
  column_to_rownames("date")

day_features_sim[is.na(day_features_sim)] <- 0
day_features_sim <- day_features_sim[, apply(day_features_sim, 2, function(x) sd(x) > 0)]
day_features_sim <- as.data.frame(scale(day_features_sim))

umap_coords_sim <- umap(day_features_sim)
umap_df_sim <- as.data.frame(umap_coords_sim) %>%
  mutate(date = rownames(day_features_sim))


ggplot(umap_df_sim, aes(V1, V2)) +
  geom_point() +
  labs(title = "UMAP on Simulated Null Movement Data")

