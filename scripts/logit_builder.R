library(tidyverse)
library(rvest)
library(janitor)


#GET nba data

link <- "https://cleaningtheglass.com/stats/league/fourfactors?season=2019&seasontype=regseason&start=10/1/2019&end=10/15/2020"
page <- read_html(link)
table <- page %>% html_table(fill = T)
df19 <- table[[1]]

link <- "https://cleaningtheglass.com/stats/league/fourfactors"
page <- read_html(link)
table <- page %>% html_table(fill = T)
df20 <- table[[1]]

# reformat headers
#test <- df19 %>%
#  row_to_names(row_number = 1) %>% 
#  clean_names()

test20 <- df20 %>%
  row_to_names(row_number = 1) %>% 
  clean_names()

# select proper columns
data <- test %>% 
  select(team_2, w, l, pts_poss_2, e_fg_percent_2, tov_percent_2, orb_percent_2, 
         ft_rate_2, pts_poss_4, e_fg_percent_4, tov_percent_4, orb_percent_4, ft_rate_4)

data20 <- test20 %>% 
  select(team_2, w, l, pts_poss_2, e_fg_percent_2, tov_percent_2, orb_percent_2, 
         ft_rate_2, pts_poss_4, e_fg_percent_4, tov_percent_4, orb_percent_4, ft_rate_4)

# rename columns
data <- data %>% 
  rename("Team" = team_2,
         "W" = w,
         "L" = l,
         "pts_poss_off" = pts_poss_2,
         "efg_percent_off" = e_fg_percent_2,
         "tov_percent_off" = tov_percent_2,
         "orb_percent_off" = orb_percent_2,
         "ft_rate_off" = ft_rate_2,
         "pts_poss_def" = pts_poss_4,
         "efg_percent_def" = e_fg_percent_4,
         "tov_percent_def" = tov_percent_4,
         "orb_percent_def" = orb_percent_4,
         "ft_rate_def" = ft_rate_4)

data20 <- data20 %>% 
  rename("Team" = team_2,
         "W" = w,
         "L" = l,
         "pts_poss_off" = pts_poss_2,
         "efg_percent_off" = e_fg_percent_2,
         "tov_percent_off" = tov_percent_2,
         "orb_percent_off" = orb_percent_2,
         "ft_rate_off" = ft_rate_2,
         "pts_poss_def" = pts_poss_4,
         "efg_percent_def" = e_fg_percent_4,
         "tov_percent_def" = tov_percent_4,
         "orb_percent_def" = orb_percent_4,
         "ft_rate_def" = ft_rate_4)

# have to change numbers to numeric and also remove characters

# change columns with percentages

data_test <- as.data.frame(lapply(data, function(y) gsub("%", "", y)))

data_test20 <- as.data.frame(lapply(data20, function(y) gsub("%", "", y)))

# change everything to character

data_test <- data_test %>% 
  mutate_all(as.character)

data_test20 <- data_test20 %>% 
  mutate_all(as.character)

# save columns that are numeric
cols.num <- c("pts_poss_off", "efg_percent_off", "tov_percent_off", "orb_percent_off", 
              "ft_rate_off", "pts_poss_def", "efg_percent_def", "tov_percent_def", 
              "orb_percent_def", "ft_rate_def")

# convert to numeric
data_test[cols.num] <- sapply(data_test[cols.num],as.numeric)

data_test20[cols.num] <- sapply(data_test20[cols.num],as.numeric)

data_test <- data_test %>% 
  mutate(Team_name = case_when(
    Team == "Atlanta" ~ "Atlanta Hawks",
    Team == "Brooklyn" ~ "Brooklyn Nets",
    Team == "Boston" ~ "Boston Celtics",
    Team == "New York" ~ "New York Knicks",
    Team == "Philadelphia" ~ "Philadelphia 76ers",
    Team == "Toronto" ~ "Toronto Raptors",
    Team == "Chicago" ~ "Chicago Bulls",
    Team == "Cleveland" ~ "Cleveland Cavaliers",
    Team == "Detroit" ~ "Detroit Pistons",
    Team == "Indiana" ~ "Indiana Pacers",
    Team == "Milwaukee" ~ "Milwaukee Bucks",
    Team == "Charlotte" ~ "Charlotte Hornets",
    Team == "Miami" ~ "Miami Heat",
    Team == "Orlando" ~ "Orlando Magic",
    Team == "Washington" ~ "Washington Wizards",
    Team == "Denver" ~ "Denver Nuggets",
    Team == "Minnesota" ~ "Minnesota Timberwolves",
    Team == "Oklahoma City" ~ "Oklahoma City Thunder",
    Team == "Utah" ~ "Utah Jazz",
    Team == "Golden State" ~ "Golden State Warriors",
    Team == "LA Clippers" ~ "Los Angeles Clippers",
    Team == "LA Lakers" ~ "Los Angeles Lakers",
    Team == "Phoenix" ~ "Phoenix Suns",
    Team == "Sacramento" ~ "Sacramento Kings",
    Team == "Dallas" ~ "Dallas Mavericks",
    Team == "Houston" ~ "Houston Rockets",
    Team == "Memphis" ~ "Memphis Grizzlies",
    Team == "New Orleans" ~ "New Orleans Pelicans",
    Team == "San Antonio" ~ "San Antonio Spurs",
    Team == "Portland" ~ "Portland Trail Blazers"
  ))


data_test20 <- data_test20 %>% 
  mutate(Team_name = case_when(
    Team == "Atlanta" ~ "Atlanta Hawks",
    Team == "Brooklyn" ~ "Brooklyn Nets",
    Team == "Boston" ~ "Boston Celtics",
    Team == "New York" ~ "New York Knicks",
    Team == "Philadelphia" ~ "Philadelphia 76ers",
    Team == "Toronto" ~ "Toronto Raptors",
    Team == "Chicago" ~ "Chicago Bulls",
    Team == "Cleveland" ~ "Cleveland Cavaliers",
    Team == "Detroit" ~ "Detroit Pistons",
    Team == "Indiana" ~ "Indiana Pacers",
    Team == "Milwaukee" ~ "Milwaukee Bucks",
    Team == "Charlotte" ~ "Charlotte Hornets",
    Team == "Miami" ~ "Miami Heat",
    Team == "Orlando" ~ "Orlando Magic",
    Team == "Washington" ~ "Washington Wizards",
    Team == "Denver" ~ "Denver Nuggets",
    Team == "Minnesota" ~ "Minnesota Timberwolves",
    Team == "Oklahoma City" ~ "Oklahoma City Thunder",
    Team == "Utah" ~ "Utah Jazz",
    Team == "Golden State" ~ "Golden State Warriors",
    Team == "LA Clippers" ~ "Los Angeles Clippers",
    Team == "LA Lakers" ~ "Los Angeles Lakers",
    Team == "Phoenix" ~ "Phoenix Suns",
    Team == "Sacramento" ~ "Sacramento Kings",
    Team == "Dallas" ~ "Dallas Mavericks",
    Team == "Houston" ~ "Houston Rockets",
    Team == "Memphis" ~ "Memphis Grizzlies",
    Team == "New Orleans" ~ "New Orleans Pelicans",
    Team == "San Antonio" ~ "San Antonio Spurs",
    Team == "Portland" ~ "Portland Trail Blazers"
  ))

data19_20 <- data_test %>% 
  left_join(data_test20, by = "Team_name")

data19_20 <- data19_20 %>% 
  mutate(pts_poss_off1920 = 0.4*pts_poss_off.x + 0.6*pts_poss_off.y, 
         pts_poss_def1920 = 0.4*pts_poss_def.x + 0.6*pts_poss_def.y,
         efg_percent_off1920 = 0.4*efg_percent_off.x + 0.6*efg_percent_off.y, 
         efg_percent_def1920 = 0.4*efg_percent_def.x + 0.6*efg_percent_def.y, 
         tov_percent_off1920 = 0.4*tov_percent_off.x + 0.6*tov_percent_off.y, 
         tov_percent_def1920 = 0.4*tov_percent_def.x + 0.6*tov_percent_def.y, 
         orb_percent_off1920 = 0.4*orb_percent_off.x + 0.6*orb_percent_off.y, 
         orb_percent_def1920 = 0.4*orb_percent_def.x + 0.6*orb_percent_def.y,
         ft_rate_off1920 = 0.4*ft_rate_off.x + 0.6*ft_rate_off.y, 
         ft_rate_def1920 = 0.4*ft_rate_def.x + 0.6*ft_rate_def.y) %>% 
  select(Team_name, pts_poss_off1920, pts_poss_def1920, efg_percent_off1920, efg_percent_def1920,
         tov_percent_off1920, tov_percent_def1920, orb_percent_off1920, orb_percent_def1920,
         ft_rate_off1920, ft_rate_def1920)


web <- "https://www.basketball-reference.com/teams/TOR/2020_games.html"
webpage <- read_html(web)
table1 <- webpage %>% html_table(fill = T)
df1 <- table1[[1]]

# remove rows with G
df1 <- df1 %>% 
  select(G, Date, Opponent, Tm, Opp, W, L) %>% 
  filter(G != "G")

# rename scores
df1 <- df1 %>% 
  rename("Tm_score" = Tm,
         "Opp_score" = Opp,
         "Team" = Opponent)

# change scores to numeric
df1$Tm_score <- as.numeric(df1$Tm_score)
df1$Opp_score <- as.numeric(df1$Opp_score)

# calculate wins
df1 <- df1 %>% 
  mutate(win = ifelse(Tm_score > Opp_score, 1, 0))

# add in 4 factors for each team
df1 <- df1 %>% 
  left_join(data_test, by = c("Team" = "Team_name"))

# new columnn for raptors
df1 <- df1 %>% 
  mutate(raps = "Toronto Raptors")

# filtering
raps <- data_test %>% 
  filter(Team_name == "Toronto Raptors")

# add raptors 4 factors
df1 <- df1 %>% 
  left_join(raps, by = c("raps" = "Team_name"))

data19_20 <- data19_20 %>% 
  mutate(efg_diff_off = efg_percent_off1920 - efg_percent_def1920,
         efg_diff_def = efg_percent_off1920 - efg_percent_def1920,
         tov_diff_off = tov_percent_off1920 - tov_percent_def1920,
         tov_diff_def = tov_percent_off1920 - tov_percent_def1920,
         orb_diff_off = orb_percent_off1920 - orb_percent_def1920,
         orb_diff_def = orb_percent_off1920 - orb_percent_def1920,
         ft_diff_off = ft_rate_off1920 - ft_rate_def1920,
         ft_diff_def = ft_rate_off1920 - ft_rate_def1920)

model <- glm(win ~ efg_diff_off + efg_diff_def + tov_diff_off + tov_diff_def +
               orb_diff_off + orb_diff_def + ft_diff_off + ft_diff_def,
             data = data19_20, family = binomial())

df1$predict <- predict.glm(model, newdata = df1,
                           type = "response")
df1 <- df1 %>% 
  mutate(pred = ifelse(predict > 0.5, 1, 0)) %>% 
  mutate(accuracy = ifelse(pred == win, 1, 0))

# NEW STUFF





link <- "https://cleaningtheglass.com/stats/league/fourfactors?season=2019&seasontype=regseason&start=10/1/2019&end=10/15/2020"
page <- read_html(link)
table <- page %>% html_table(fill = T)
df19 <- table[[1]]

link <- "https://cleaningtheglass.com/stats/league/fourfactors"
page <- read_html(link)
table <- page %>% html_table(fill = T)
df20 <- table[[1]]

# reformat headers
test <- df19 %>%
  row_to_names(row_number = 1) %>% 
  clean_names()

test20 <- df20 %>%
  row_to_names(row_number = 1) %>% 
  clean_names()

# select proper columns
data <- test %>% 
  select(team_2, w, l, pts_poss_2, e_fg_percent_2, tov_percent_2, orb_percent_2, 
         ft_rate_2, pts_poss_4, e_fg_percent_4, tov_percent_4, orb_percent_4, ft_rate_4)

data20 <- test20 %>% 
  select(team_2, w, l, pts_poss_2, e_fg_percent_2, tov_percent_2, orb_percent_2, 
         ft_rate_2, pts_poss_4, e_fg_percent_4, tov_percent_4, orb_percent_4, ft_rate_4)

# rename columns
data <- data %>% 
  rename("Team" = team_2,
         "W" = w,
         "L" = l,
         "pts_poss_off" = pts_poss_2,
         "efg_percent_off" = e_fg_percent_2,
         "tov_percent_off" = tov_percent_2,
         "orb_percent_off" = orb_percent_2,
         "ft_rate_off" = ft_rate_2,
         "pts_poss_def" = pts_poss_4,
         "efg_percent_def" = e_fg_percent_4,
         "tov_percent_def" = tov_percent_4,
         "orb_percent_def" = orb_percent_4,
         "ft_rate_def" = ft_rate_4)

data20 <- data20 %>% 
  rename("Team" = team_2,
         "W" = w,
         "L" = l,
         "pts_poss_off" = pts_poss_2,
         "efg_percent_off" = e_fg_percent_2,
         "tov_percent_off" = tov_percent_2,
         "orb_percent_off" = orb_percent_2,
         "ft_rate_off" = ft_rate_2,
         "pts_poss_def" = pts_poss_4,
         "efg_percent_def" = e_fg_percent_4,
         "tov_percent_def" = tov_percent_4,
         "orb_percent_def" = orb_percent_4,
         "ft_rate_def" = ft_rate_4)

# have to change numbers to numeric and also remove characters

# change columns with percentages

data_test <- as.data.frame(lapply(data, function(y) gsub("%", "", y)))

data_test20 <- as.data.frame(lapply(data20, function(y) gsub("%", "", y)))

# change everything to character

data_test <- data_test %>% 
  mutate_all(as.character)

data_test20 <- data_test20 %>% 
  mutate_all(as.character)

# save columns that are numeric
cols.num <- c("pts_poss_off", "efg_percent_off", "tov_percent_off", "orb_percent_off", 
              "ft_rate_off", "pts_poss_def", "efg_percent_def", "tov_percent_def", 
              "orb_percent_def", "ft_rate_def")

# convert to numeric
data_test[cols.num] <- sapply(data_test[cols.num],as.numeric)

data_test20[cols.num] <- sapply(data_test20[cols.num],as.numeric)

data_test <- data_test %>% 
  mutate(Team_name = case_when(
    Team == "Atlanta" ~ "Atlanta Hawks",
    Team == "Brooklyn" ~ "Brooklyn Nets",
    Team == "Boston" ~ "Boston Celtics",
    Team == "New York" ~ "New York Knicks",
    Team == "Philadelphia" ~ "Philadelphia 76ers",
    Team == "Toronto" ~ "Toronto Raptors",
    Team == "Chicago" ~ "Chicago Bulls",
    Team == "Cleveland" ~ "Cleveland Cavaliers",
    Team == "Detroit" ~ "Detroit Pistons",
    Team == "Indiana" ~ "Indiana Pacers",
    Team == "Milwaukee" ~ "Milwaukee Bucks",
    Team == "Charlotte" ~ "Charlotte Hornets",
    Team == "Miami" ~ "Miami Heat",
    Team == "Orlando" ~ "Orlando Magic",
    Team == "Washington" ~ "Washington Wizards",
    Team == "Denver" ~ "Denver Nuggets",
    Team == "Minnesota" ~ "Minnesota Timberwolves",
    Team == "Oklahoma City" ~ "Oklahoma City Thunder",
    Team == "Utah" ~ "Utah Jazz",
    Team == "Golden State" ~ "Golden State Warriors",
    Team == "LA Clippers" ~ "Los Angeles Clippers",
    Team == "LA Lakers" ~ "Los Angeles Lakers",
    Team == "Phoenix" ~ "Phoenix Suns",
    Team == "Sacramento" ~ "Sacramento Kings",
    Team == "Dallas" ~ "Dallas Mavericks",
    Team == "Houston" ~ "Houston Rockets",
    Team == "Memphis" ~ "Memphis Grizzlies",
    Team == "New Orleans" ~ "New Orleans Pelicans",
    Team == "San Antonio" ~ "San Antonio Spurs",
    Team == "Portland" ~ "Portland Trail Blazers"
  ))


data_test20 <- data_test20 %>% 
  mutate(Team_name = case_when(
    Team == "Atlanta" ~ "Atlanta Hawks",
    Team == "Brooklyn" ~ "Brooklyn Nets",
    Team == "Boston" ~ "Boston Celtics",
    Team == "New York" ~ "New York Knicks",
    Team == "Philadelphia" ~ "Philadelphia 76ers",
    Team == "Toronto" ~ "Toronto Raptors",
    Team == "Chicago" ~ "Chicago Bulls",
    Team == "Cleveland" ~ "Cleveland Cavaliers",
    Team == "Detroit" ~ "Detroit Pistons",
    Team == "Indiana" ~ "Indiana Pacers",
    Team == "Milwaukee" ~ "Milwaukee Bucks",
    Team == "Charlotte" ~ "Charlotte Hornets",
    Team == "Miami" ~ "Miami Heat",
    Team == "Orlando" ~ "Orlando Magic",
    Team == "Washington" ~ "Washington Wizards",
    Team == "Denver" ~ "Denver Nuggets",
    Team == "Minnesota" ~ "Minnesota Timberwolves",
    Team == "Oklahoma City" ~ "Oklahoma City Thunder",
    Team == "Utah" ~ "Utah Jazz",
    Team == "Golden State" ~ "Golden State Warriors",
    Team == "LA Clippers" ~ "Los Angeles Clippers",
    Team == "LA Lakers" ~ "Los Angeles Lakers",
    Team == "Phoenix" ~ "Phoenix Suns",
    Team == "Sacramento" ~ "Sacramento Kings",
    Team == "Dallas" ~ "Dallas Mavericks",
    Team == "Houston" ~ "Houston Rockets",
    Team == "Memphis" ~ "Memphis Grizzlies",
    Team == "New Orleans" ~ "New Orleans Pelicans",
    Team == "San Antonio" ~ "San Antonio Spurs",
    Team == "Portland" ~ "Portland Trail Blazers"
  ))

data19_20 <- data_test %>% 
  left_join(data_test20, by = "Team_name")

data19_20 <- data19_20 %>% 
  mutate(pts_poss_off1920 = pts_poss_off.x, 
         pts_poss_def1920 = pts_poss_def.x,
         efg_percent_off1920 = efg_percent_off.x, 
         efg_percent_def1920 = efg_percent_def.x, 
         tov_percent_off1920 = tov_percent_off.x, 
         tov_percent_def1920 = tov_percent_def.x, 
         orb_percent_off1920 = orb_percent_off.x, 
         orb_percent_def1920 = orb_percent_def.x ,
         ft_rate_off1920 = ft_rate_off.x, 
         ft_rate_def1920 = ft_rate_def.x) %>% 
  select(Team_name, pts_poss_off1920, pts_poss_def1920, efg_percent_off1920, efg_percent_def1920,
         tov_percent_off1920, tov_percent_def1920, orb_percent_off1920, orb_percent_def1920,
         ft_rate_off1920, ft_rate_def1920)


home_teams <- data_test20$Team_name
away_teams <- data_test20$Team_name

matchups <- expand_grid(home_teams, away_teams)
matchups <- matchups %>% drop_na()

matchups <- matchups %>% 
  left_join(data_test20, by = c("home_teams" = "Team_name"))
matchups <- matchups %>% 
  left_join(data_test20, by = c("away_teams" = "Team_name"))


nba_games <- read_csv("nba_predictor/games.csv")
teams <- read_csv("nba_predictor/teams.csv")

# select variables
teams <- teams %>% 
  select(TEAM_ID, CITY, NICKNAME) %>% 
  mutate(team = paste(CITY, NICKNAME, sep = " ")) %>% 
  select(TEAM_ID, team)

# rename teams
nba_games <- nba_games %>% 
  filter(SEASON >= 2019) %>% 
  left_join(teams, by = c("TEAM_ID_home" = "TEAM_ID")) %>% 
  left_join(teams, by = c("TEAM_ID_away" = "TEAM_ID")) %>% 
  rename("Home" = team.x,
         "Away" = team.y) 
nba_games <- nba_games %>% 
  select(Home, Away, HOME_TEAM_WINS)


matchups <- matchups %>% 
  left_join(nba_games, by = c("home_teams" = "Home", "away_teams" = "Away"))




# create variables for differences
matchups <- matchups %>% 
  mutate(efg_diff_off = efg_percent_off.x - efg_percent_def.y,
         efg_diff_def = efg_percent_def.x - efg_percent_off.y,
         tov_diff_off = tov_percent_off.x - tov_percent_def.y,
         tov_diff_def = tov_percent_def.x - tov_percent_off.y,
         orb_diff_off = orb_percent_off.x - orb_percent_def.y,
         orb_diff_def = orb_percent_def.x - orb_percent_off.y,
         ft_diff_off = ft_rate_off.x - ft_rate_def.y,
         ft_diff_def = ft_rate_def.x - ft_rate_off.y,
         pts_poss_off = pts_poss_off.x - pts_poss_def.y,
         pts_poss_def = pts_poss_def.x - pts_poss_off.y)

# create model
model <- glm(Home_team_wins ~ efg_diff_def + efg_diff_off +
               orb_diff_off + tov_diff_off + ft_diff_def + pts_poss_def +
               pts_poss_off + tov_diff_def + orb_diff_def + ft_diff_off,
             data = schedule, family = binomial())
summary(model)

schedule$estimate <- predict.glm(model, newdata = schedule,
                                 type = "response")

matchups <- matchups %>% 
  mutate(sum_ratings = sum(efg_diff_def, efg_diff_off, orb_diff_off, orb_diff_def, 
                            tov_diff_def, tov_diff_off, ft_diff_def, ft_diff_off,
                            pts_poss_def, pts_poss_off))

matchups$estimate <- predict.glm(model, newdata = matchups,
                                 type = "response")

schedule <- schedule %>% 
  mutate(pred = ifelse(estimate > mean(estimate), 1, 0)) %>% 
  mutate(accuracy = ifelse(pred == Home_team_wins, 1, 0))

sum(schedule$accuracy)/nrow(schedule)

# get predictions
nba_games_ff$estimate <- predict.glm(model, newdata = nba_games_ff,
                           type = "response")


schedule <- read_csv("nba_predictor/nbaschedule2021.csv")
schedule <- schedule %>% 
  left_join(matchups, by = c("Visitor/Neutral" = "home_teams",
                             "Home/Neutral"= "away_teams"))

write.csv(matchups, file = "nba_predictor/matchups.csv")
write.csv(schedule, file = "nba_predictor/2021schedule.csv")

sked <- schedule %>% 
  distinct()

sum(sked$accuracy)
### ACCURACY TEST ###


nba_games_ff <- nba_games_ff %>% 
  mutate(pred = ifelse(estimate > 0.5, 1, 0)) %>% 
  mutate(accuracy = ifelse(pred == Home, 1, 0))

sum(nba_games_ff$accuracy)/nrow(nba_games_ff)

write_csv(nba_games_ff, "nba_predictor/nbafourfactors.csv")


sked <- read_csv("scripts/nba_schedule2020.csv")
sked <- sked %>% 
  mutate(home_win = ifelse(Winner == `Home/Neutral`, 1, 0))

sked <- sked %>% 
  left_join(data19_20, by = c(`Home/Neutral` = "Team_name"))
sked <- sked %>% 
  left_join(data19_20, by = c(`Visitor/Neutral` = "Team_name"))
sked <- sked %>% 
  mutate(efg_diff_off = efg_percent_off1920.x - efg_percent_def1920.y,
         efg_diff_def = efg_percent_def1920.x - efg_percent_off1920.y,
         tov_diff_off = tov_percent_off1920.x - tov_percent_def1920.y,
         tov_diff_def = tov_percent_def1920.x - tov_percent_off1920.y,
         orb_diff_off = orb_percent_off1920.x - orb_percent_def1920.y,
         orb_diff_def = orb_percent_def1920.x - orb_percent_off1920.y,
         ft_diff_off = ft_rate_off1920.x - ft_rate_def1920.y,
         ft_diff_def = ft_rate_def1920.x - ft_rate_off1920.y,
         pts_poss_off = pts_poss_off1920.x - pts_poss_def1920.y,
         pts_poss_def = pts_poss_def1920.x - pts_poss_off1920.y)

model <- glm(home_win ~ efg_diff_off + efg_diff_def + tov_diff_off + tov_diff_def +
               orb_diff_off + orb_diff_def + ft_diff_off + ft_diff_def + pts_poss_off + pts_poss_def,
             data = sked, family = binomial())
summary(model)



# get predictions
sked$estimate <- predict.glm(model, newdata = sked,
                                     type = "response")

# test for accuracy
sked <- sked %>% 
  mutate(pred = ifelse(estimate > 0.5, 1, 0)) %>% 
  mutate(accuracy = ifelse(pred == home_win, 1, 0))

sum(sked$accuracy, na.rm = T)
