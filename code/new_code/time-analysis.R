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


# 1. Load the logger data
logger_data <- read_excel("~/Code/Birds/data/logger/1min/2016_weaver_all_colony_1min_20190515_final.xlsx")

# isolate to just LLOD
logger_llod <- logger_data %>%
  filter(plot == "LLOD") %>%
  mutate(date = as_date(Date)) %>%
  arrange(Kring, timestamp.1m) #switch Color

logger_moves <- logger_llod %>%
  group_by(Kring) %>%
  arrange(timestamp.1m) %>%
  mutate(
    from_nest = nest,
    to_nest = lead(nest),
    from_date = date,
    to_date = lead(date)
  ) %>%
  filter(to_date == from_date + days(1))

#aggregate movement per day
daily_summary <- logger_moves %>%
  group_by(from_date) %>%
  summarise(
    n_transitions = n(),
    n_birds_moved = n_distinct(Color),
    n_unique_edges = n_distinct(paste(from_nest, to_nest, sep = "_")),
    n_nests_involved = n_distinct(c(from_nest, to_nest))
  )


# Nest-level movement graphs
networks_per_day <- split(logger_moves, logger_moves$from_date)

network_stats <- lapply(networks_per_day, function(df) {
  g <- graph_from_data_frame(df %>% select(from_nest, to_nest), directed = TRUE)
  tibble(
    density = edge_density(g),
    clustering = transitivity(g, type = "global"),
    n_nodes = gorder(g),
    n_edges = gsize(g)
  )
}) %>% bind_rows(.id = "date")

## UMAP stuff

network_stats <- network_stats %>%
  mutate(date = as.Date(date))
daily_summary <- daily_summary %>%
  mutate(from_date = as.Date(from_date))

day_features <- daily_summary %>%
  left_join(network_stats, by = c("from_date" = "date")) %>%
  column_to_rownames("from_date") 
  
day_features[is.na(day_features)] <- 0
day_features <- day_features[, apply(day_features, 2, function(x) sd(x) > 0)]
day_features <- as.data.frame(scale(day_features))
day_features[!is.finite(as.matrix(day_features))] <- 0

anyNA(day_features)                           # Should be FALSE
any(is.nan(as.matrix(day_features)))          # Should be FALSE
any(is.infinite(as.matrix(day_features)))     # Should be FALSE


umap_coords <- umap(day_features)

umap_df <- as.data.frame(umap_coords) %>%
  mutate(date = rownames(day_features))

#Kmeans
set.seed(42)
kfit <- kmeans(umap_df[, c("V1", "V2")], centers = 3)
umap_df$cluster <- factor(kfit$cluster)

umap_df$date <- as.Date(umap_df$date)

umap_df <- umap_df %>%
  left_join(daily_summary, by = c("date" = "from_date")) %>%
  left_join(network_stats, by = c("date" = "date"))


ggplot(umap_df, aes(x = cluster, y = n_transitions, fill = cluster)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Transitions per Day by Cluster", x = "Cluster", y = "Number of Transitions")


###Cluster analysis

# Transitions
ggplot(umap_df, aes(x = cluster, y = n_transitions, fill = cluster)) +
  geom_boxplot() + theme_minimal() +
  labs(title = "Transitions per Day by Cluster")

# Number of birds
ggplot(umap_df, aes(x = cluster, y = n_birds_moved, fill = cluster)) +
  geom_boxplot() + theme_minimal() +
  labs(title = "Birds Moving per Day by Cluster")

# Clustering coefficient
ggplot(umap_df, aes(x = cluster, y = clustering, fill = cluster)) +
  geom_boxplot() + theme_minimal() +
  labs(title = "Network Clustering by Cluster")

# Network density
ggplot(umap_df, aes(x = cluster, y = density, fill = cluster)) +
  geom_boxplot() + theme_minimal() +
  labs(title = "Network Density by Cluster")


cluster_summary <- umap_df %>%
  group_by(cluster) %>%
  summarise(
    mean_transitions = mean(n_transitions, na.rm = TRUE),
    mean_birds = mean(n_birds_moved, na.rm = TRUE),
    mean_edges = mean(n_unique_edges, na.rm = TRUE),
    mean_density = mean(density, na.rm = TRUE),
    mean_clustering = mean(clustering, na.rm = TRUE),
    n_days = n()
  )
print(cluster_summary)


ggplot(umap_df, aes(x = as.Date(date), y = cluster, group = cluster)) +
  geom_point(aes(color = cluster), size = 3) +
  theme_minimal() +
  labs(title = "Temporal Sequence of Clusters", x = "Date", y = "Cluster")



##Plot transitions vs. density, colored by cluster
ggplot(umap_df, aes(x = n_transitions, y = density, color = cluster)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "Transitions vs. Network Density by Cluster")


#line fitting


df$pred_density <- predict(exp_fit)

ggplot(df, aes(x = n_transitions, y = density, color = cluster)) +
  geom_point(size = 3) +
  geom_line(aes(y = pred_density), color = "black", size = 1.2) +
  theme_minimal() +
  labs(title = "Exponential Fit: Transitions vs. Network Density")


# Finding line fitting parameters
df <- umap_df %>%
  filter(!is.na(density), density > 0, !is.na(n_transitions))

# Nonlinear fit: density = a * exp(-b * n_transitions)
exp_fit <- nls(density ~ a * exp(-b * n_transitions),
               data = df,
               start = list(a = 0.1, b = 0.05))  # starting guessee
              # a = 0.39199 , b = 0.18960

summary(exp_fit)



##Temporal order of Clusters

ggplot(umap_df, aes(x = as.Date(date), y = cluster, color = cluster)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "Temporal Sequence of Movement Clusters")



##PLotting

ggplot(umap_df, aes(V1, V2)) +
  geom_point() +
  geom_text(aes(label = date), hjust = -0.1, size = 2.5) +
  theme_minimal() +
  labs(title = "Daily Movement Patterns Across Nests (LLOD)")








