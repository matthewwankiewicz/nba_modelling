library(tidyverse)
library(rvest)
library(janitor)


#GET nba data

link <- "https://cleaningtheglass.com/stats/league/fourfactors"
page <- read_html(link)
table <- page %>% html_table(fill = T)
df <- table[[1]]

# reformat headers
test <- df %>%
  row_to_names(row_number = 1) %>% 
  clean_names()

# select proper columns
data <- test %>% 
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

# have to change numbers to numeric and also remove characters

# change columns with percentages

data_test <- as.data.frame(lapply(data, function(y) gsub("%", "", y)))

# change everything to character

data_test <- data_test %>% 
  mutate_all(as.character)

# save columns that are numeric
cols.num <- c("pts_poss_off", "efg_percent_off", "tov_percent_off", "orb_percent_off", 
              "ft_rate_off", "pts_poss_def", "efg_percent_def", "tov_percent_def", 
              "orb_percent_def", "ft_rate_def")

# convert to numeric
data_test[cols.num] <- sapply(data_test[cols.num],as.numeric)

write.csv(data_test, "nba_predictor/fourfactors2020.csv")






