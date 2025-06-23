# Part 1A: Kinship & Nesting Proximity Correlation
# ------------------------------------------------
# Goal: Do kin nest closer than non-kin?
# Approach: Linear/logistic regression; network assortativity by relatedness

# --- Step-by-step Plan ---
# 1. Load required libraries
# 2. Import relevant data files:
#    - relateness_weaver.csv: for pairwise relatedness (combo1, combo2, r)
#    - combined_log: for mapping individuals (combo) to nests and colonies
#    - colony_measurement.xlsx: for nest spatial coordinates (x, y)
#    - intercolony_distance.xlsx: for Euclidean distances between nests
# 3. Clean and merge data to:
#    a. Identify all pairs (combo1, combo2) and their relatedness (r)
#    b. Map each combo to nest, colony, and spatial coordinates
#    c. Compute distance between each pair's nests (dist_m)
#    d. Determine if both individuals are in the same colony (TRUE/FALSE)
# 4. Construct final dataframe:
#    - combo1, combo2, r, nest1, nest2, dist_m, same_colony
# 5. Statistical analysis:
#    - Linear regression: dist_m ~ r
#    - Logistic regression: same_colony ~ r
#    (Optionally add sex as a control later)
# 6. Visualize results

# --- Begin Implementation ---

# 1. Load libraries
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
.libPaths()

# 2. Import data

setwd("~/Code/Birds")

# output_2013-2017-weaver-r-and-association_SS_all_yearplot.csv: columns id1, id2, r, v1sex, v2sex, v12sex
pairwise_data <- read_csv("data/behavior_social/output_2013-2017-weaver-r-and-association_SS_all_yearplot.csv")

# Select relevant columns
pairwise_data <- pairwise_data %>%
  select(id1, id2, r, V1sex, V2sex, V12sex)

# Print first few rows for verification
print(head(pairwise_data))

# relateness_weaver.csv: columns combo1, combo2, r
relateness <- read_csv("data/behavior_social/Relateness_weaver.csv")
relateness <- relateness %>%
  mutate(ind1.id = sub("^AA_", "", ind1.id),
         ind2.id = sub("^AA_", "", ind2.id))
# combined_weaver_log.xlsx: columns combo, nest, colony
weaver_log <- read_excel("data/observation_logs/combined_weaver_log.xlsx")

# colony_measurement.xlsx: nest spatial coordinates (x, y)
colony_measurement <- read_excel("data/Environment/c_2013-2017_weaver_colony _measurement.xlsx")

# intercolony_distance.xlsx: nest-to-nest distances
intercolony_distance <- read_excel("data/Environment/intercolony_distance.xlsx")

combo_map <- read_excel("data/observation_logs/c_2012-2017_weaver_banding.xlsx") %>%
  select(Combo, Metal) %>%
  rename(combo = Combo)


# 3. Data wrangling steps
# a. Merge relatedness with nest/colony assignments
# For each combo1 and combo2, get their nest and colony from weaver_log

# Map id1 and id2 to Kring using combo_map
pairwise_data <- pairwise_data %>%
  left_join(combo_map, by = c("id1" = "combo")) %>%
  rename(Kring1 = Metal) %>%
  left_join(combo_map, by = c("id2" = "combo")) %>%
  rename(Kring2 = Metal)

# Get latest nest and colony assignment for each Kring
combo_nest_colony <- weaver_log %>%
  arrange(Kring, desc(timestamp.1m)) %>%
  group_by(Kring) %>%
  slice(1) %>%
  ungroup() %>%
  select(Kring, nest, colony)

# Join nest/colony info for Kring1 and Kring2
pairwise_data <- pairwise_data %>%
  left_join(combo_nest_colony, by = c("Kring1" = "Kring")) %>%
  rename(nest1 = nest, colony1 = colony) %>%
  left_join(combo_nest_colony, by = c("Kring2" = "Kring")) %>%
  rename(nest2 = nest, colony2 = colony)

# Prepare final dataframe
final_df <- pairwise_data %>%
  select(id1, id2, r, V1sex, V2sex, V12sex, nest1, nest2, colony1, colony2)



# Standardize colony names (remove underscores, uppercase)
final_df <- final_df %>%
  mutate(colony1 = toupper(gsub("_", "", colony1)),
         colony2 = toupper(gsub("_", "", colony2)))
intercolony_distance <- intercolony_distance %>%
  mutate(colony1 = toupper(gsub("_", "", colony1)),
         colony2 = toupper(gsub("_", "", colony2)))

# Join with intercolony_distance to get colony-to-colony distances
final_df <- final_df %>%
  left_join(intercolony_distance, by = c("colony1" = "colony1", "colony2" = "colony2"))

# If dist_m is still NA, try the reverse (colony2, colony1)
final_df <- final_df %>%
  mutate(dist_m = ifelse(is.na(dist_m),
                        intercolony_distance$dist_m[match(
                          paste(colony2, colony1),
                          paste(intercolony_distance$colony1, intercolony_distance$colony2)
                        )],
                        dist_m))

# Remove duplicates
final_df <- final_df %>%
  distinct(id1, id2, colony1, colony2, .keep_all = TRUE)

# Clean: keep only rows with both colonies and distance known
final_df_clean <- final_df %>%
  filter(!is.na(colony1) & !is.na(colony2) & !is.na(dist_m)) %>%
  distinct(id1, id2, colony1, colony2, .keep_all = TRUE)

# Create same_colony variable if not already present
final_df_clean <- final_df_clean %>%
  mutate(same_colony = colony1 == colony2)

# Print first few rows for verification
print(head(final_df_clean))

# Standardize colony names (remove underscores, uppercase) before extracting plot
final_df_clean <- final_df_clean %>%
  mutate(
    colony1 = toupper(gsub("_", "", colony1)),
    colony2 = toupper(gsub("_", "", colony2))
  )

# Extract plot from colony names and create regime variable
final_df_clean <- final_df_clean %>%
  mutate(
    plot1 = substr(colony1, 1, 4),
    plot2 = substr(colony2, 1, 4),
    regime = ifelse(plot1 == plot2, "Within Plot", "Between Plot")
  )

# Print regime distribution for verification
table(final_df_clean$regime)

# Filter out rows where dist_m <= 0 before log-transform
final_df_clean <- final_df_clean %>%
  filter(dist_m > 0)

# Add log-transformed distance
final_df_clean <- final_df_clean %>%
  mutate(log_dist_m = log(dist_m))

# Linear regression: log_dist_m ~ r (Within Plot)
cat("\nLinear regression: log_dist_m ~ r (Within Plot)\n")
print(summary(lm(log_dist_m ~ r, data = filter(final_df_clean, regime == "Within Plot"))))

# Linear regression: log_dist_m ~ r (Between Plot)
cat("\nLinear regression: log_dist_m ~ r (Between Plot)\n")
print(summary(lm(log_dist_m ~ r, data = filter(final_df_clean, regime == "Between Plot"))))

# Visualization: log distance by regime
ggplot(final_df_clean, aes(x = r, y = log_dist_m, color = regime)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm") +
  labs(title = "Log Colony Distance vs. Relatedness by Regime",
       x = "Relatedness (r)", y = "Log Distance (m)")

# 5. Statistical analysis
# Linear regression: dist_m ~ r
# Logistic regression: same_colony ~ r
# (to be filled)

# Linear regression: Does relatedness predict distance?
lm_dist <- lm(dist_m ~ r, data = final_df_clean)
cat("\nLinear regression: dist_m ~ r\n")
print(summary(lm_dist))

# Logistic regression: Does relatedness predict being in the same colony?
glm_colony <- glm(same_colony ~ r, data = final_df_clean, family = binomial)
cat("\nLogistic regression: same_colony ~ r\n")
print(summary(glm_colony))

# Visualization: Relatedness vs. Distance
ggplot(final_df_clean, aes(x = r, y = dist_m)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm") +
  labs(title = "Colony Distance vs. Relatedness", x = "Relatedness (r)", y = "Distance (m)") 