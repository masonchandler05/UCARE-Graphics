library(DBI)
library(RSQLite)
library(dplyr)
library(tidyverse)
library(lme4)
library(lmerTest)

#open connection
con <- dbConnect(RSQLite::SQLite(), "visualization_study_final.db")

#reading in data 
users_data <- dbReadTable(con, "users")
sessions_data <- dbReadTable(con, "sessions")
judgments_data <- dbReadTable(con, "judgments")
images_data <- dbReadTable(con, "selected_images")

# close connection
dbDisconnect(con)

# Filter out sessions before March 15
sessions_after_cutoff <- sessions_data %>%
  filter(as.Date(session_start) >= as.Date("2026-03-15")) %>%
  pull(session_id)

# Join judgments with image data and filter
judgments_with_images <- judgments_data %>%
  filter(session_id %in% sessions_after_cutoff) %>%
  left_join(images_data, by = c("session_id", "selection_id"))

judgements_per_session <- judgments_with_images %>%
  group_by(session_id) %>%
  summarise(num_judgements = n())

completed_tests <- judgements_per_session %>%
  filter(num_judgements == 27)

data_complete <- judgments_with_images %>%
  filter(session_id %in% completed_tests$session_id) %>%
  mutate(
    true_val = true_value, 
    user_val = user_ratio, 
    error = true_value - user_ratio,
    error_ratio = error / true_value,
    pair = factor(pair_number.x),
    order = factor(judgment_within_image),
    type = vis_type
  ) %>%
  select(session_id, true_val, user_val, error, error_ratio, pair, order, type) %>%
  arrange(session_id)


# Create dataset
data_all <- data_complete

# Model with all data
model_all <- lmer(error ~ type + factor(pair) + factor(order) + (1 | session_id), data = data_all)
summary(model_all)

# Model with different notation 
model_all2 <- lmer(error ~ type + factor(pair) + factor(order) + (1 | session_id), data = data_all)
print(summary(model_all2)$coefficients)

