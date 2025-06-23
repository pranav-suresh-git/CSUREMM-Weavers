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

# Step 1: Clean and filter
logger_spra <- logger_data %>%
  filter(plot == "SPRA") %>%
  mutate(
    date = as.Date(Date),
    Kring = as.character(Kring),
    nest = as.character(nest)
  )

# Step 2: Daily activity stats
daily_activity <- logger_spra %>%
  group_by(date, plot) %>%
  summarise(
    n_birds = n_distinct(Kring),
    n_visits = n(),
    n_nests = n_distinct(nest),
    .groups = "drop"
  )

# Only dates with any activity
valid_dates <- daily_activity %>%
  filter(n_birds >= 1, n_visits >= 1) %>%
  pull(date)

logger_active <- logger_spra %>%
  filter(date %in% valid_dates)

# Step 3: Create edge list per bird
logger_moves <- logger_active %>%
  group_by(Kring) %>%
  arrange(timestamp.1m) %>%
  mutate(
    from_nest = nest,
    to_nest = lead(nest),
    from_date = date,
    to_date = lead(date)
  ) %>%
  filter(from_date == to_date, from_nest != to_nest) %>%
  ungroup()

# Step 4: Network stats per day
networks_by_day <- logger_moves %>%
  filter(!is.na(from_nest) & !is.na(to_nest)) %>%
  group_by(from_date) %>%
  group_split()

network_stats <- map_dfr(networks_by_day, function(df) {
  g <- tryCatch({
    graph_from_data_frame(df[, c("from_nest", "to_nest")], directed = TRUE)
  }, error = function(e) return(NULL))
  
  if (is.null(g)) return(NULL)
  
  tibble(
    date = unique(df$from_date),
    density = edge_density(g),
    clustering = transitivity(g, type = "global")
  )
})

# Step 5: Join + prepare for HMM
day_features <- daily_activity %>%
  filter(date %in% valid_dates) %>%
  left_join(network_stats, by = "date") %>%
  dplyr::select(date, n_visits, density, clustering) %>%
  filter(!is.na(density)) %>%
  mutate(across(where(is.numeric), ~replace_na(., 0))) %>%
  column_to_rownames("date")

# Remove constant columns
numeric_cols <- sapply(day_features, is.numeric)
nonzero_var_cols <- apply(day_features[, numeric_cols], 2, function(x) sd(x, na.rm = TRUE) > 0)
day_features <- day_features[, names(nonzero_var_cols[nonzero_var_cols])]

# Final scale + cleanup
day_features <- as.data.frame(scale(day_features))
day_features[!is.finite(as.matrix(day_features))] <- 0

###############################################################

##### Define and fit HMM

set.seed(42)
day_features_hmm <- daily_activity %>%
  left_join(network_stats, by = "date") %>%
  filter(!is.na(density)) %>%
  mutate(
    n_transitions = n_visits,  # or however you define transitions
    density = replace_na(density, 0),
    clustering = replace_na(clustering, 0)
  ) %>%
  dplyr::select(date, n_transitions, density, clustering) %>%
  filter(!is.na(date))


day_features_hmm$date <- as.Date(rownames(day_features))

# Fit 3-state HMM with key features
hmm_model <- depmix(
  response = list(
    n_transitions ~ 1,
    density ~ 1,
    clustering ~ 1
  ),
  data = day_features_hmm,
  nstates = 2,
  family = list(poisson(), gaussian(), gaussian())
)

hmm_fit <- fit(hmm_model, verbose = FALSE)


day_features_hmm$state <- posterior(hmm_fit)$state


#Regime visualization:


day_features_hmm <- day_features_hmm %>%
  arrange(date) %>%
  mutate(day_number = row_number()) 
ggplot(day_features_hmm, aes(x = day_number, y = n_transitions, color = factor(state))) +
  geom_line(size = 1.2) +
  theme_minimal() +
  labs(title = "Hidden Markov States Over Time (Compressed Time)",
       x = "Day Index (no gaps)",
       y = "Number of Transitions",
       color = "Regime")



day_features_hmm$state <- posterior(hmm_fit)$state
summary_by_state <- day_features_hmm %>%
  group_by(state) %>%
  summarise(across(c(n_transitions, density, clustering), mean, na.rm = TRUE))

print(summary_by_state)

transition_matrix <- hmm_fit@trDens
print(transition_matrix)

day_features_hmm$day_index <- seq_len(nrow(day_features_hmm))library(ggplot2)

ggplot(day_features_hmm, aes(x = day_index, y = n_transitions, color = factor(state))) +
  geom_line(size = 1.1) +
  labs(
    title = "Hidden Markov States Over Time",
    x = "Sequential Day Index",
    y = "Number of Transitions",
    color = "State"
  ) +
  theme_minimal()


