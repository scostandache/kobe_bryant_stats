library(readr)
library(dplyr)
library(plotly)
library(tidyr)
library(stringr)
library(polycor)
library(arules)
library(ggplot2)
library(shiny)
library(magrittr)
library(viridisLite)

# !!!! only train on events that occured prior to the shot being predicted
# https://www.kaggle.com/wiki/Leakage

# Data load ----------------------------------
# setwd("workspace/facultate/DM/kobe-bryant-stats/")
df <- read_csv(url("https://raw.githubusercontent.com/serbanc94/kobe_bryant_stats/master/data.csv"))

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
  mutate(home_match = grepl("vs", matchup))

# Injuries that kept him on he bench:
# https://pbs.twimg.com/media/BioEZffCAAAIJwr.png
# Worth noting: 99-00, 03-04, 04-05,13-14

# Data enrichment ------------------------------

# Supplimetary CSV with team names abbreviations
# https://sportsdelve.wordpress.com/abbreviations/

full_names_df <- read_csv(url("https://raw.githubusercontent.com/serbanc94/kobe_bryant_stats/master/team_names.csv"))
shot_dist_mean <- mean(df$shot_distance)
df <- df %>%
  filter(!is.na(shot_made_flag)) %>%
  left_join(full_names_df) %>%
  mutate(shot_value = as.numeric(substring(shot_type, 1, 1))) %>%
  mutate(short_distance = (shot_distance <= shot_dist_mean))


# Insights -----------------------------------

games_per_season <- df %>%
  group_by(season) %>%
  distinct(game_id) %>%
  summarise(games = n())

seasonal_stats <- df %>%
  filter(home_match == TRUE) %>%
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
  spread(shot_made_flag, count) %>%
  mutate_all(funs(replace(., is.na(.), 0)))


# Correlations ------------------------

## !!! t test or mean
# !! Box plots

cor(df$shot_distance, df$shot_made_flag)
cor(df$shot_value, df$shot_made_flag)
cor(df$loc_x, df$shot_made_flag)
cor(df$loc_y, df$shot_made_flag)
cor(df$home_match, df$shot_made_flag)
cor(df$period, df$shot_made_flag)
cor(df$short_distance, df$shot_made_flag)

df %>%
  group_by(shot_zone_area, shot_made_flag) %>%
  summarise(count = n())

colnames(df)

# Visualizations -----------------------------------

# Action type bar chart
df %>%
  group_by(action_type) %>%
  summarise(count = n()) %>%
  plot_ly(y = ~ count, type = "bar", hoverinfo = "text", text = (~ action_type))

# Shots stats by season
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

# Scatterplot
loc_df <- df %>%
  select(loc_x, loc_y, shot_made_flag) %>%
  group_by(loc_x, loc_y, shot_made_flag) %>%
  summarize(count = n()) %>%
  spread(shot_made_flag, count) %>%
  arrange(desc(`1`)) %>%
  mutate_all(funs(replace(., is.na(.), 0))) %>%
  rowwise() %>%
  mutate(percentage = `1` / (`1` + `0`)) %>%
  plot_ly(
    x = ~ loc_x,
    y = ~ loc_y,
    color = ~ percentage,
    type = "scatter"
  )
loc_df

# Shot frequency heatmap
inferno_colors <- inferno(100)

df %>%
  filter(abs(loc_x) > 1, abs(loc_y) > 1) %>%
  ggplot() +
  stat_density_2d(
    aes(
      x = loc_x, y = loc_y,
      fill = ..density..
    ),
    geom = "raster", contour = FALSE, interpolate = TRUE, n = 200
  ) +
  scale_fill_gradientn(colors = inferno_colors, guide = FALSE)


# Hexplot

ggplot(data = df) +
  geom_hex(aes(x = loc_x, y = loc_y), binwidth = c(15, 15)) +
  scale_fill_gradient(trans = "log", low = "blue", high = "red") +
  facet_wrap(~ shot_made_flag) +
  coord_fixed() +
  ggtitle("Misses vs makes")
ggsave("misses_vs_makes.png")


# Bubble chart of accuracy percentages

loc_matrix <- as.matrix(cbind(df$loc_x, df$loc_y))
loc_cluster <- kmeans(loc_matrix, centers = 500)
df$loc_cluster <- loc_cluster$cluster


cluster_percentages <- df %>%
  group_by(loc_cluster, shot_made_flag) %>%
  summarize(count = n()) %>%
  spread(shot_made_flag, count) %>%
  mutate_all(funs(replace(., is.na(.), 0))) %>%
  mutate(total = as.integer(`1` + `0`)) %>%
  mutate(percentage = `1` / total)
cluster_percentages$center_x <- loc_cluster$centers[, 1]
cluster_percentages$center_y <- loc_cluster$centers[, 2]

cluster_percentages %>%
  mutate(reg_total = log(total)) %>%
  plot_ly(
    x = ~ center_x,
    y = ~ center_y,
    type = "scatter",
    mode = "markers",
    color = ~ percentage,
    colors = "Reds",
    marker = list(
      size = ~ reg_total * 1.5,
      opacity = ~ percentage * 3
    )
  ) %>%
  layout(
    plot_bgcolor = "rgb(120,120,120)"
  )



# Arules transactions --------------------------


trans_df <- data_frame(
  action_type = as.factor(df$action_type),
  combined_shot = as.factor(df$combined_shot_type),
  short_distance = as.factor(df$short_distance),
  period = as.factor(df$period),
  home_match = as.factor(df$home_match),
  shot_made_flag = as.factor(df$shot_made_flag)
)


trans_df <- as(trans_df, "transactions")

inspect(trans_df[1:20])
rules <- apriori(trans_df, 
                 parameter = list(support = 0.01, target="rules", conf = 0.5), 
                 appearance = list(rhs='shot_made_flag=0'))
inspect(sort(rules, decreasing = TRUE, by="confidence"))


# Sandbox -----------
