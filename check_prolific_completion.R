library(tidyverse)
library(DBI)
library(RSQLite)
library(dplyr)
library(digest)
library(dplyr)

con <- dbConnect(RSQLite::SQLite(), "visualization_study_final.db")

users <- dbReadTable(con, "users")
sessions <- dbReadTable(con, "sessions")
judgments <- dbReadTable(con, "judgments")

sessions |>
  filter(completed == 1) |>
  left_join(users) |>
  filter(!is.na(prolific_id))
