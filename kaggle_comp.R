library(readr)
library(dplyr)
library(plotly)
library(tidyr)
library(stringr)
library(corrr)
library(polycor)
library(arules)
library(dbscan)
library(ggplot2)

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
  rowwise() %>%
  mutate(home_match = grepl('vs', matchup))

# Injuries that kept him on he bench:
# https://pbs.twimg.com/media/BioEZffCAAAIJwr.png
# Worth noting: 99-00, 03-04, 04-05,13-14

# Data enrichment ------------------------------

# Supplimetary CSV with team names abbreviations
# https://sportsdelve.wordpress.com/abbreviations/

full_names_df <- read_csv("team_names.csv")
df <- df %>%
  filter(!is.na(shot_made_flag)) %>%
  left_join(full_names_df) %>%
  mutate(shot_value = as.numeric(substring(shot_type, 1, 1)))


# Insights -----------------------------------

games_per_season <- df %>%
  group_by(season) %>%
  distinct(game_id) %>%
  summarise(games = n())

seasonal_stats <- df %>%
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



action_type_stats <- df %>% 
  mutate(shot_made_flag = as.numeric(shot_made_flag)) %>% 
  group_by(action_type, shot_made_flag) %>%
  summarise(count = n()) %>%
  spread(shot_made_flag, count)  %>%
  mutate_all(funs(replace(., is.na(.), 0)))


#Correlations ------------------------

## !!! t test or mean 
#!! Box plots

cor(df$shot_distance, df$shot_made_flag)
cor(df$shot_value, df$shot_made_flag)
cor(df$loc_x, df$shot_made_flag)
cor(df$loc_y, df$shot_made_flag)

df %>%
  group_by(shot_zone_area, shot_made_flag) %>%
  summarise( count = n())

colnames(df)


#Locations heatmap

df %>%
   group_by(loc_x, loc_y) %>%
   summarize(count = n())

# Visualizations -----------------------------------

# Action type bar chart
df %>% 
  group_by(action_type) %>% 
  summarise( count =n()) %>% 
  plot_ly(y=~count, type="bar",hoverinfo="text", text=(~action_type))

#Shots stats by season
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


#Heatmap ---------------------

loc_df <- df %>% select(loc_x,loc_y,shot_made_flag)
loc_df %>% 
  group_by(loc_x, loc_y,shot_made_flag) %>% 
  summarize(count = n()) %>%
  spread(shot_made_flag, count) %>%
  arrange(desc(`1`)) %>%
  mutate_all(funs(replace(.,is.na(.),0))) %>%
  rowwise()%>%
  mutate(percentage = `1`/(`1`+`0`)) %>%
  plot_ly(x=~loc_x, y=~loc_y, z=~percentage, type="heatmap")




# Arules transactions --------------------------


trans_df <- data_frame(
  action_type = as.factor(df$action_type),
  combined_shot = as.factor(df$combined_shot_type),
  shot_distance = as.factor(df$shot_distance),
  period = as.factor(df$period),
  shot_made_flag = as.factor(df$shot_made_flag)
)


trans_df <- as(trans_df, "transactions")

inspect(trans_df[1:5])
apriori(trans_df, parameter=list(support = 0.5))

support(trans_df$action_type)


