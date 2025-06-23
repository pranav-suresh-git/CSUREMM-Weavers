library(dplyr)
library(readr)
library(ggplot2)
library(readxl)
library(igraph)
library(scales)
library(tidygraph)
library(tibble)
library(depmixS4)
library(purrr)

setwd("~/Code/Birds")
logger_data <- read_excel("data/logger/combined_weaver_log.xlsx")


logger_spra <- logger_data %>%
  filter(plot == "SPRA")

bird_paths <- logger_spra %>%
  arrange(Kring, timestamp.1m) %>%
  group_by(Kring) %>%
  mutate(
    from_nest = nest,
    to_nest = lead(nest),
    same_nest = as.integer(from_nest == lead(nest)),
    time_lag = as.numeric(difftime(lead(timestamp.1m), timestamp.1m, units = "mins"))
  ) %>%
  filter(!is.na(to_nest), time_lag < 60)  # optional: only short gaps

transitions <- logger_spra %>%
  filter(!is.na(nest), !is.na(Kring)) %>%
  arrange(Kring, timestamp.1m) %>%
  group_by(Kring) %>%
  mutate(
    from_nest = nest,
    to_nest = lead(nest),
    transition_time = as.numeric(difftime(lead(timestamp.1m), timestamp.1m, units = "mins"))
  ) %>%
  ungroup() %>%
  filter(!is.na(to_nest), transition_time < 60)  # Optional: filter short lag transitions

transition_counts <- transitions %>%
  count(from_nest, to_nest)

# Create transition matrix
transition_matrix <- transition_counts %>%
  group_by(from_nest) %>%
  mutate(prob = n / sum(n)) %>%
  ungroup()

# (Optional) Build igraph Markov chain visualization
g <- graph_from_data_frame(transition_matrix, directed = TRUE)
plot(g, edge.width = transition_matrix$prob * 10)


transitions_hmm <- transitions %>%
  mutate(same_nest = as.integer(from_nest == to_nest)) %>%
  filter(!is.na(same_nest))

# Optional: filter one bird for simplicity or aggregate all
bird_seq <- transitions_hmm %>%
  arrange(Kring, timestamp.1m) %>%
  dplyr::select(Kring, same_nest)



mod <- depmix(
  same_nest ~ 1,
  family = binomial(),
  nstates = 2,
  data = bird_seq
)

fit_mod <- fit(mod, verbose = FALSE)
post_probs <- posterior(fit_mod)
bird_seq$state <- post_probs$state





#ggplot(bird_seq, aes(x = 1:nrow(bird_seq), y = same_nest, color = factor(state))) +
 # geom_line(size = 1) +
  #labs(title = "Nest Stay/Switch Pattern with Hidden States", x = "Transition Index", y = "Same Nest (1=Yes)",
   #    color = "HMM State") +
  #theme_minimal()



library(dplyr)
library(lubridate)

####Lagged variable
bird_features <- logger_spra %>%
  arrange(Kring, timestamp.1m) %>%
  group_by(Kring) %>%
  mutate(
    prev_nest = lag(nest),
    nest_same_as_prev = if_else(nest == prev_nest, 1, 0, missing = 0),
    time_diff = as.numeric(difftime(timestamp.1m, lag(timestamp.1m), units = "mins")),
    lagged_visit_count = lag(cumsum(!is.na(nest))),  # How many visits the bird made before this
    lagged_unique_nests = lag(n_distinct(nest[1:row_number()])), # Rolling count of unique nests
  ) %>%
  ungroup()

###Bird ID level features
bird_summaries <- logger_active %>%
  group_by(Kring) %>%
  summarise(
    total_visits = n(),
    unique_nests = n_distinct(nest),
    avg_time_between_visits = {
      sorted_times <- sort(timestamp.1m)
      diffs <- diff(sorted_times)
      diffs <- diffs[diffs >= 0]
      mean(as.numeric(diffs), na.rm = TRUE)
    },
    prop_same_nest_visits = mean(nest == lag(nest), na.rm = TRUE),
    first_seen = min(date),
    last_seen = max(date)
  )


library(entropy)

entropy_per_bird <- logger_spra %>%
  group_by(Kring) %>%
  summarise(
    nest_entropy = entropy::entropy(table(nest))  # Higher = more random
  )

bird_features <- bird_features %>%
  left_join(entropy_per_bird, by = "Kring")



nest_entropy_df <- logger_active %>%
  group_by(Kring, nest) %>%
  summarise(visits = n(), .groups = "drop") %>%
  group_by(Kring) %>%
  mutate(p = visits / sum(visits)) %>%
  summarise(
    nest_entropy = -sum(p * log(p)),
    .groups = "drop"
  )

bird_summaries <- bird_summaries %>%
  left_join(nest_entropy_df, by = "Kring")




############Clustering and results


library(cluster)

bird_type_features <- bird_summaries %>%
  dplyr::select(
    Kring,
    total_visits,
    unique_nests,
    avg_time_between_visits,
    prop_same_nest_visits,
    nest_entropy
  )

colnames(bird_summaries)

bird_summaries %>%
  dplyr:: select(
    total_visits, 
    unique_nests, 
    avg_time_between_visits, 
    prop_same_nest_visits, 
    nest_entropy
    )

bird_type_features_numeric <- bird_type_features %>%
  mutate(
    avg_time_between_visits = as.numeric(avg_time_between_visits)) %>%
  mutate(across(c(total_visits, unique_nests, avg_time_between_visits, 
                  prop_same_nest_visits, nest_entropy), as.numeric)) %>% 
  filter(
    is.finite(total_visits),
    is.finite(unique_nests),
    is.finite(avg_time_between_visits),
    is.finite(prop_same_nest_visits),
    is.finite(nest_entropy)
  )

##K-means

kmeans_input <- bird_type_features_numeric %>%
  dplyr::select(total_visits, unique_nests, avg_time_between_visits, 
                prop_same_nest_visits, nest_entropy)
bird_clusters <- kmeans(scale(kmeans_input), centers = 4)

bird_type_features_numeric$cluster <- bird_clusters$cluster

bird_summaries <- left_join(
  bird_summaries, 
  dplyr::select(bird_type_features_numeric, Kring, cluster), 
  by = "Kring"
  )





library(ggplot2)

ggplot(bird_type_features_numeric, aes(x = prop_same_nest_visits, y = nest_entropy, color = factor(cluster))) +
  geom_point(size = 2, alpha = 0.7) +
  theme_minimal() +
  labs(title = "Behavioral Clusters of Birds", color = "Cluster",
       x = "Proportion of Same Nest Visits", y = "Nest Entropy")


############### Kmeans clustering summary

bird_type_features_numeric %>%
  group_by(cluster) %>%
  summarise(
    count = n(),
    mean_fidelity = mean(prop_same_nest_visits),
    mean_entropy = mean(nest_entropy),
    mean_visits = mean(total_visits),
    mean_nests = mean(unique_nests),
    .groups = "drop"
  )

bird_type_features_numeric <- bird_type_features_numeric %>%
  mutate(cluster_label = case_when(
    cluster == 1 ~ "Resident",
    cluster == 2 ~ "Roamer",
    cluster == 3 ~ "Explorer (Outlier)",
    cluster == 4 ~ "Loyal Roamer"
  ))


bird_summaries <- left_join(bird_summaries, 
                            bird_type_features_numeric %>% select(Kring, cluster_label), 
                            by = "Kring")


###UMAP

library(umap)
library(ggplot2)

# Reuse already scaled kmeans_input
umap_result <- umap(scale(kmeans_input))
umap_coords <- as.data.frame(umap_result$layout)
colnames(umap_coords) <- c("UMAP1", "UMAP2")

# Add cluster and Kring ID for plotting
umap_coords$cluster <- factor(bird_type_features_numeric$cluster)
umap_coords$Kring <- bird_type_features_numeric$Kring

ggplot(umap_coords, aes(x = UMAP1, y = UMAP2, color = cluster)) +
  geom_point(size = 2, alpha = 0.7) +
  theme_minimal() +
  labs(title = "UMAP of Bird Behavioral Clusters", color = "Cluster")

##specific bird behavior
ggplot(umap_coords, aes(x = UMAP1, y = UMAP2, color = cluster)) +
  geom_point(size = 2) +
  geom_text(aes(label = Kring), size = 2, hjust = 0.5, vjust = -0.8, check_overlap = TRUE) +
  theme_minimal()

##UMAP cluster summary means
bird_type_features_numeric %>%
  group_by(cluster) %>%
  summarise(
    n = n(),
    avg_visits = mean(total_visits),
    avg_fidelity = mean(prop_same_nest_visits),
    avg_entropy = mean(nest_entropy),
    avg_unique_nests = mean(unique_nests),
    avg_time_between = mean(avg_time_between_visits)
  )

### Reflection: clustering finds a pattern, but it is weak; methods of continuum
# and other things that are more gradual might be better for this test;

#######################################################
#DBSCAN / also not great

library(dbscan)

# eps and minPts will need tuning
scaled_input <- scale(kmeans_input)
db <- dbscan::dbscan(scaled_input, eps = 1.0, minPts = 5)

# Add cluster labels
bird_type_features_numeric$cluster_dbscan <- db$cluster  # Note: 0 = noise


library(umap)
umap_result <- umap(scaled_input)
umap_df <- as.data.frame(umap_result$layout)
colnames(umap_df) <- c("UMAP1", "UMAP2")
umap_df$cluster <- factor(bird_type_features_numeric$cluster_dbscan)

ggplot(umap_df, aes(x = UMAP1, y = UMAP2, color = cluster)) +
  geom_point(size = 2, alpha = 0.8) +
  theme_minimal() +
  labs(title = "UMAP of Bird Behaviors (DBSCAN Clusters)")


