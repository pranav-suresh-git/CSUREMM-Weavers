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

logger_data <- read_excel("data/logger/combined_weaver_log.xlsx") %>%
  filter(plot == "SPRA")
  arrange(Kring, timestamp.1m) %>%
  group_by(Kring) %>%
  mutate(
    next_nest = lead(nest),
    current_nest = nest,
    time_diff = as.numeric(difftime(lead(timestamp.1m), timestamp.1m, units = "mins"))
  ) %>%
  ungroup() %>%
  filter(!is.na(next_nest)) %>%
  mutate(reward = -time_diff)  # Inverse time as reward

# Define helper functions
`%||%` <- function(a, b) if (!is.null(a)) a else b

softmax <- function(x, temp = 1) {
  x <- x[!is.na(x)]  # Remove NAs
  exp_x <- exp(x / temp)
  
  probs <- exp_x / sum(exp_x)
  return(probs)
}

# Learning parameters
alpha <- 0.1
temp <- 0.2

# Rebuild bird_models if not already defined
bird_models <- logger_data %>%
  group_by(Kring) %>%
  group_split() %>%
  set_names(unique(logger_data$Kring))

# Reinforcement learning per bird
rl_results <- map_dfr(bird_models, function(bird_df) {
  bird_nests <- unique(c(bird_df$current_nest, bird_df$next_nest))
  Q <- setNames(rep(0, length(bird_nests)), bird_nests)
  
  results <- list()  # Collect rows manually
  
  for (i in seq_len(nrow(bird_df))) {
    current <- as.character(bird_df$current_nest[i])
    next_n <- as.character(bird_df$next_nest[i])
    r <- bird_df$reward[i]
    
    # Skip if invalid
    if (is.na(next_n) || !(next_n %in% bird_nests)) {
      results[[i]] <- cbind(bird_df[i, ], chosen_prob = NA)
      next
    }
    
    # Update Q
    Q[next_n] <- Q[next_n] + alpha * (r - Q[next_n])
    
    # Compute softmax
    probs <- softmax(Q, temp)
    
    # Safe assignment of probs
    prob_vec <- setNames(rep(NA, length(bird_nests)), paste0("prob_", bird_nests))
    names(probs) <- paste0("prob_", names(probs))
    prob_vec[names(probs)] <- probs
    
    # Combine result
    result_row <- cbind(bird_df[i, ], as.data.frame(as.list(prob_vec)))
    result_row$chosen_prob <- probs[paste0("prob_", next_n)] %||% NA
    
    results[[i]] <- result_row
  }
  
  bind_rows(results)
  
  
  
  library(ggplot2)
  
  # Ensure `chosen_prob` exists and is numeric
  if (!"chosen_prob" %in% names(rl_results)) {
    stop("`chosen_prob` column not found in rl_results.")
  }
  
  if (!is.numeric(rl_results$chosen_prob)) {
    rl_results$chosen_prob <- as.numeric(rl_results$chosen_prob)
  }
  
  # Remove NA values before plotting (common in RL models)
  rl_plot_data <- rl_results %>% filter(!is.na(chosen_prob))
  
  # Plot
  ggplot(rl_plot_data, aes(x = chosen_prob)) +
    geom_histogram(bins = 30, fill = "steelblue", color = "white") +
    labs(
      title = "Distribution of Chosen Probabilities",
      x = "Probability of Chosen Nest",
      y = "Count"
    ) +
    theme_minimal()
  

library(ggplot2)
ggplot(rl_results, aes(x = chosen_prob)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Chosen Probabilities",
       x = "Probability of Chosen Nest",
       y = "Count") +
  theme_minimal()
})