library(readr)
library(dplyr)
library(plotly)
library(tidyr)
library(stringr)

# !!!! only train on events that occured prior to the shot being predicted
# https://www.kaggle.com/wiki/Leakage


# set wd, read given data
setwd("workspace/facultate/DM")
df <- read_csv("Datasets/KobeBryant/data.csv")


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

# supplimetary dataset with team names abbreviations
# https://sportsdelve.wordpress.com/abbreviations/

full_names_df <- read_csv("Datasets/KobeBryant/team_names.csv")
df <- df %>%
  left_join(full_names_df) %>%
  mutate(shot_value = as.numeric(substring(shot_type, 1, 1)))


seasonal_shots <- df %>%
  filter(!is.na(shot_made_flag)) %>%
  group_by(season, shot_made_flag) %>%
  summarise(count = n()) %>%
  spread(shot_made_flag, count) %>%
  mutate(shots_total = `1` + `0`) %>%
  mutate(percentage = 100 * (`1` / (`1` + `0`))) %>%
  select(
    shots_success = `1`,
    shots_fail = `0`,
    shots_total,
    percentage
  )
seasonal_shots

p <- plot_ly(seasonal_shots,
  x = ~ `season`,
  y = ~ shots_success,
  type = "bar",
  color = ~ `percentage`,
  hoverinfo = "text",
  text = ~ paste(
    "Full stats, season ", season, "</br>:",
    "</br> Total shots: ", shots_total,
    "</br> Failed shots: ", shots_fail,
    "</br> Success shots: ", shots_success,
    "</br> Percentage: ", round(percentage, 2), "%"
  )
) %>%
  layout(
    title = "Shots statistics by season",
    yaxis = list(
      title = "# of succesful shots"
    )
  )
p
