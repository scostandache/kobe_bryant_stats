library(readr)
library(dplyr)
library(plotly)
library(tidyr)
library(stringr)
library(polycor)
library(arules)
library(arulesViz)
library(ggplot2)
library(shiny)
library(magrittr)
library(viridisLite)
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(Matrix)
library(xgboost)
library(caret)
library(tidyverse)
library(DataExplorer)
library(gmodels)
library(dbscan)
library(dendextend)
library(EMCluster)

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
  # action_type = as.factor(df$action_type),
  # combined_shot = as.factor(df$combined_shot_type),
  # short_distance = as.factor(df$short_distance),
  period = as.factor(df$period),
  # home_match = as.factor(df$home_match),
  shot_made_flag = as.factor(df$shot_made_flag),
  shot_zone_range = as.factor(df$shot_zone_range)
)


trans_df <- as(trans_df, "transactions")

inspect(trans_df[1:20])
rules <- apriori(trans_df,
  parameter = list(support = 0.05, target = "rules", conf = 0.4, minlen = 2),
  appearance = list(rhs = "shot_made_flag=1")
)
inspect(sort(rules, decreasing = TRUE, by = "confidence"))
plot(rules, method = "graph")
plotly_arules(rules)

# Decision trees --------------------

dtree_df <- df %>%
  mutate(shot_distance = ifelse(shot_distance > 45, 45, shot_distance))
fit <- rpart(shot_made_flag ~ minutes_remaining + home_match + shot_value + playoffs + shot_zone_basic, method = "class", data = df)
fancyRpartPlot(fit)


# XGBoost ---------------------------

raw_df <- read_csv(url("https://raw.githubusercontent.com/serbanc94/kobe_bryant_stats/master/data.csv"))
raw_df <- raw_df %>%
  mutate(shot_distance = ifelse(shot_distance > 45, 45, shot_distance)) %>%
  mutate(time_remaining = minutes_remaining * 60 + seconds_remaining) %>%
  mutate(
    seconds_remaining = NULL,
    team_name = NULL,
    team_id = NULL,
    game_event_id = NULL,
    game_id = NULL,
    lat = NULL,
    lon = NULL
  ) %>%
  select(-c(opponent, matchup, season))

train_df <- raw_df %>%
  filter(!is.na(shot_made_flag)) %>%
  mutate(
    id = shot_id,
    shot_id = NULL
  ) %>%
  mutate(
    y = shot_made_flag,
    shot_made_flag = NULL
  )

train_dmy <- dummyVars(" ~ .", data = train_df)
train_sf <- predict(train_dmy, newdata = train_df)

test_df <- raw_df %>%
  filter(is.na(shot_made_flag)) %>%
  mutate(
    id = shot_id,
    shot_id = NULL,
    shot_made_flag = NULL
  )

test_dmy <- dummyVars(" ~ .", data = test_df)
test_sf <- predict(test_dmy, newdata = test_df)

trainM <- data.matrix(train_sf, rownames.force = NA)
xgb_dtrain <- xgb.DMatrix(data = trainM, label = train_df$y, missing = NaN)

watchlist <- list(trainM = xgb_dtrain)

set.seed(1984)

param <- list(
  objective = "binary:logistic",
  booster = "gbtree",
  eval_metric = "logloss",
  eta = 0.0005,
  max_depth = 100,
  subsample = 0.7,
  colsample_bytree = 0.5,
  nthread = 8
)
system.time(
  clf <- xgb.cv(
    params = param,
    data = xgb_dtrain,
    nrounds = 5000,
    verbose = 1,
    watchlist = watchlist,
    maximize = FALSE,
    nfold = 5,
    early_stopping_rounds = 10,
    print.every.n = 1,
    prediction = TRUE
  )
)


clf <- xgb.train(
  params = param,
  data = xgb_dtrain,
  nrounds = 1500,
  verbose = 1,
  watchlist = watchlist,
  maximize = FALSE,
  early_stopping_rounds = 300
)



testM <- data.matrix(test_sf, rownames.force = NA)
preds <- predict(clf, testM)

submission <- data.frame(shot_id = test_df$id, shot_made_flag = preds)
submission %>%
  write_csv("submission.csv", col_names = F)

write.csv(submission, "submission.csv", row.names = F)


# KNN ------------------------------
file_csv <- read_csv(url("https://raw.githubusercontent.com/serbanc94/kobe_bryant_stats/master/data.csv"))
raw_df <- file_csv
shot_made_flag_class <- raw_df$shot_made_flag

raw_df <- raw_df %>%
  mutate(shot_distance = ifelse(shot_distance > 45, 45, shot_distance)) %>%
  mutate(time_remaining = minutes_remaining * 60 + seconds_remaining) %>%
  mutate(
    seconds_remaining = NULL,
    team_name = NULL,
    team_id = NULL,
    game_event_id = NULL,
    game_id = NULL,
    lat = NULL,
    lon = NULL
  ) %>%
  select(-c(opponent, matchup, period, season, game_date, playoffs, shot_id, shot_made_flag)) %>%
  mutate_if(is.numeric, scale)

raw_df$shot_made_flag <- shot_made_flag_class


raw_dmy <- dummyVars(" ~ .", data = raw_df)
raw_sf <- data.frame(predict(raw_dmy, newdata = raw_df))

train_sf <- raw_sf %>%
  filter(!is.na(shot_made_flag))
test_sf <- raw_sf %>%
  filter(is.na(shot_made_flag))

train_class <- train_sf$shot_made_flag
train_sf <- train_sf %>%
  select(-c(shot_made_flag))
test_sf <- test_sf %>%
  select(-c(shot_made_flag))

raw_df <- file_csv
submission <- FNN::knn(train_sf, test_sf, cl = train_class, k = 2500, prob = TRUE, algorithm = c("kd_tree", "cover_tree", "brute"))
shot_ids <- raw_df %>%
  filter(is.na(shot_made_flag)) %>%
  select(shot_id)
namex <- "shot_id"
namey <- "shot_made_flag"
df <- data.frame(shot_ids, attr(submission, "prob"))
names(df) <- c(namex, namey)

write.csv(df, "submission.csv", row.names = F)


# Task4 ---------------------------

# DF1
df1 <- read.table("2d-10c.dat", skip = 3, header = FALSE, col.names = c("X", "Y", "Class"))

#hierarchical clustering
plot(hclust(dist(df1), method="complete"))
plot(hclust(dist(df1), method="single"))
plot(hclust(dist(df1), method="average"))
plot(hclust(dist(df1), method="ward.D"))

# DBSCAN

dbs <- dbscan(df1, eps = 1)
df_dbs <- df1
df_dbs$cluster <- dbs$cluster
plot_ly(df_dbs, x=~X, y=~Y, color = ~Class)
plot_ly(df_dbs, x=~X, y=~Y, color = ~cluster)

# K-means

kmns <- kmeans(df1, centers = 10)
df_kmns <- df1
df_kmns$cluster <- kmns$cluster
plot_ly(df_kmns, x=~X, y=~Y, color = ~Class)
plot_ly(df_kmns, x=~X, y=~Y, color = ~cluster)

# EM

set.seed(1500)

emobj <- simple.init(df1, nclass = 10)
emcls <- emcluster(df1, emobj, assign.class = TRUE)

df_em <- df1
df_em$cluster <- emcls$class
plot_ly(df_em, x=~X, y=~Y, color = ~Class)
plot_ly(df_em, x=~X, y=~Y, color = ~cluster)



df2 <- iris %>%
  mutate(Species = case_when(
    Species == "setosa" ~ 0,
    Species == "versicolor" ~ 1,
    Species == "virginica" ~ 2
  ))

df3 <- read.delim("order2-3clust.csv", header = FALSE, sep = ",", col.names = c("X", "Y", "Class"))

df4 <- read.delim("smile.csv", header = FALSE, sep = ",", col.names = c("X", "Y", "Class"))

df5 <- read.table("square.data", header = FALSE, col.names = c("X", "Y", "Class"))

