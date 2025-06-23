library(dplyr)
library(readr)
library(ggplot2)
library(readxl)
library(igraph)
library(scales)
library(ggraph)
library(tidygraph)
library(tidyr)
library(janitor)
library(tibble)
library(depmixS4)
library(purrr)

setwd("~/Code/Birds")
logger_data <- read_excel("data/logger/combined_weaver_log.xlsx")
stopifnot("date" %in% names(daily_activity))
stopifnot("date" %in% names(network_stats))
###################################################################
#Clean to daily data, ignoring days with no recordings

# isolate to just SPRA
logger_spra <- logger_data %>%
  filter(plot == "SPRA") %>%
  mutate(
    date = as_date(Date),
    Kring = as.character(Kring),
    nest = as.character(nest)
    )

daily_activity <- logger_spra %>%
  group_by(date, plot) %>%
  summarise(
    n_birds = n_distinct(Kring),
    n_visits = n(),
    n_nests = n_distinct(nest),
    .groups = "drop"
  )

valid_dates <- daily_activity %>%
  filter(n_birds >= 1, n_visits >= 1) %>%
  pull(date)

logger_active <- logger_spra %>%
  filter(date %in% valid_dates)



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

#more stats

networks_by_day <- logger_moves %>%
  filter(!is.na(from_nest) & !is.na(to_nest)) %>%
  group_by(from_date) %>%
  group_split()

# Compute network stats per day
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


##########################################################################

#data preparation

# Step 1: Join and filter by valid dates

day_features <- daily_activity %>%
  filter(date %in% valid_dates) %>%
  left_join(network_stats, by = "date") %>%
  select(date, n_visits, density, clustering) %>%
  filter(!is.na(density)) %>% 
  mutate(across(where(is.numeric), ~replace_na(., 0))) %>%
  column_to_rownames("date")
  
numeric_cols <- sapply(day_features, is.numeric)
nonzero_var_cols <- apply(day_features[, numeric_cols], 2, function(x) sd(x, na.rm = TRUE) > 0)
day_features <- day_features[, names(nonzero_var_cols[nonzero_var_cols])]
day_features <- as.data.frame(scale(day_features))
day_features[!is.finite(as.matrix(day_features))] <- 0


# Step 2: Scale & clean
day_features_scaled <- day_features %>%
  mutate(across(c(n_visits, density, clustering), ~scale(.)[,1]))  # scale() returns matrix


day_features <- day_features[, apply(day_features, 2, sd) > 0]
day_features <- as.data.frame(scale(day_features))


##### Define and fit HMM

set.seed(42)
day_features_hmm <- day_features
day_features_hmm$date <- as.Date(rownames(day_features))

# Fit 3-state HMM with key features
hmm_model <- depmix(
  response = list(
    n_transitions ~ 1,
    density ~ 1,
    clustering ~ 1
  ),
  data = day_features_hmm,
  nstates = 3,
  family = list(gaussian(), gaussian(), gaussian())
)

hmm_fit <- fit(hmm_model, verbose = FALSE)


day_features_hmm$state <- posterior(hmm_fit)$state


#Regime visualization:

ggplot(day_features_hmm, aes(x = date, y = n_transitions, color = factor(state))) +
  geom_line(size = 1.2) +
  theme_minimal() +
  labs(title = "Hidden Markov States over Time", color = "Regime")




#############################
#basic date-based-plot
ggplot(daily_activity, aes(x = date, y = n_visits)) +
  geom_line() +
  facet_wrap(~plot, scales = "free_y") +
  theme_minimal() +
  labs(title = "Daily Nest Visit Volume", y = "Number of Visits")


