library(readr)
library(dplyr)
library(plotly)
library(tidyr)
library(stringr)

# !!!! only train on events that occured prior to the shot being predicted
# https://www.kaggle.com/wiki/Leakage

# Data load ----------------------------------
setwd("workspace/facultate/DM/kobe-bryant-stats/")
df <- read_csv("data.csv")

# Data repair ---------------------------------

# Some teams changed their names, so the abbreviations changed as well;
# In the data file the same team appeared under different abbreviations
# Brooklyn Nets(BKN) was New Jersey Nets(NJN)
# https://en.wikipedia.org/wiki/Brooklyn_Nets
# New Orleans Pelicans(NOP) was New Orleans Hornets(NOH)
# https://en.wikipedia.org/wiki/New_Orleans_Pelicans

df <- df %>%
  mutate(opponent = replace(opponent, opponent == "BKN", "NJN")) %>%
  mutate(opponent = replace(opponent, opponent == "NOP", "NOH")) %>%
  mutate(matchup = paste("LAL @ ", opponent, sep = ""))

# Injuries that kept him on he bench:
# https://pbs.twimg.com/media/BioEZffCAAAIJwr.png
# Worth noting: 99-00, 03-04, 04-05,13-14

# Data enrichment ------------------------------

# Supplimetary CSV with team names abbreviations
# https://sportsdelve.wordpress.com/abbreviations/

full_names_df <- read_csv("team_names.csv")
df <- df %>%
  left_join(full_names_df) %>%
  mutate(shot_value = as.numeric(substring(shot_type, 1, 1)))


# Insights -----------------------------------

games_per_season <- df %>%
  group_by(season) %>%
  distinct(game_id) %>%
  summarise(games = n())

seasonal_stats <- df %>%
  filter(!is.na(shot_made_flag)) %>%
  group_by(season, shot_made_flag) %>%
  summarise(count = n()) %>%
  spread(shot_made_flag, count) %>%
  mutate(shots_total = `1` + `0`) %>%
  mutate(shot_accuracy = 100 * (`1` / (`1` + `0`))) %>%
  ungroup() %>%
  inner_join(games_per_season) %>%
  mutate(success_per_game = `1` / games) %>%
  select(
    season,
    games,
    shots_success = `1`,
    shots_fail = `0`,
    shots_total,
    shot_accuracy,
    success_per_game
  )

seasonal_stats

# Visualizations -----------------------------------

seasonal_stats_graph <- plot_ly(seasonal_stats,
  x = ~ `season`,
  y = ~ shots_success,
  type = "bar",
  color = ~ `shot_accuracy`,
  hoverinfo = "text",
  text = ~ paste(
    "Full stats, season ", season, "</br>:",
    "</br> Total shots: ", shots_total,
    "</br> Failed shots: ", shots_fail,
    "</br> Success shots: ", shots_success,
    "</br> Percentage: ", round(shot_accuracy, 2), "%"
  )
) %>%
  layout(
    title = "Shots statistics by season",
    yaxis = list(
      title = "# of succesful shots"
    )
  )

seasonal_stats_graph


#-----------------
