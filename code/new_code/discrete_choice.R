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
  arrange(Kring, timestamp.1m)

logger_data <- logger_data %>%
  group_by(Kring) %>%
  mutate(
    next_nest = lead(nest),
    current_nest = nest,
    time_diff = as.numeric(difftime(lead(timestamp.1m), timestamp.1m, units = "mins"))
  ) %>%
  ungroup() %>%
  filter(!is.na(next_nest))

##Define reward system

logger_data <- logger_data %>%
  mutate(reward = -time_diff)  # Inverse time as simple reward proxy

##implement softmax decision rule
`%||%` <- function(a, b) if (!is.null(a)) a else b

softmax <- function(x, temp = 1) {
  x <- x[!is.na(x)]  # remove NAs
  exp_x <- exp(x / temp)
  probs <- exp_x / sum(exp_x)
  names(probs) <- names(x)
  return(probs)
}

alpha <- 0.1
temp <- 0.2

rl_results <- map_dfr(bird_models, function(bird_df) {
  bird_nests <- unique(c(bird_df$current_nest, bird_df$next_nest))
  Q <- setNames(rep(0, length(bird_nests)), bird_nests)
  sim_df <- bird_df
  
  for (i in 1:nrow(bird_df)) {
    current <- as.character(bird_df$current_nest[i])
    next_n <- as.character(bird_df$next_nest[i])
    r <- bird_df$reward[i]
    
    if (is.na(next_n) || !(next_n %in% bird_nests)) {
      sim_df[i, paste0("prob_", bird_nests)] <- NA
      sim_df[i, "chosen_prob"] <- NA
      next
    }
    Q[next_n] <- Q[next_n] + alpha * (r - Q[next_n])
    probs <- softmac(Q, temp)
    
    full_probs <- setNames(rep(NA, length(bird_nests)), bird_nests)
    full_probs[names(probs)] <- probs
    
    sim_df[i, paste0("prob_", bird_nests)] <- full_probs
    sim_df[i, "chosen_prob"] <- probs[next_n] %||% NA
  }
  sim_df
})







# Learning parameters
alpha <- 0.1   # learning rate
temp <- 0.2    # temperature

# Get all nests
nest_ids <- unique(logger_data$nest)

# Create Q-table for each bird
bird_models <- logger_data %>%
  group_by(Kring) %>%
  group_split() %>%
  set_names(unique(logger_data$Kring))

# Run reinforcement learning per bird
rl_results <- map_dfr(bird_models, function(bird_df) {
  bird_nests <- unique(c(bird_df$current_nest, bird_df$next_nest))
  Q <- setNames(rep(0, length(bird_nests)), bird_nests)
  sim_df <- bird_df
  
  for (i in 1:nrow(bird_df)) {
    current <- as.character(bird_df$current_nest[i])
    next_n <- as.character(bird_df$next_nest[i])
    r <- bird_df$reward[i]
    if (is.na(next_n) || !(next_n %in% nest_ids)) {
      next
    }
    
    # Update Q
    Q[next_n] <- Q[next_n] + alpha * (r - Q[next_n])
    
    # Store softmax choice probabilities
    probs <- softmax(Q, temp)
    
    sim_df[i, paste0("prob_", bird_nests)] <- probs[bird_nests]
    sim_df[i, "chosen_prob"] <- probs[next_n]
  }
  sim_df
})

bird_models[["K00000"]] %>% head()
