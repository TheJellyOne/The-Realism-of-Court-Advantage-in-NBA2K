install.packages("gghighlight")
library(gghighlight)

install.packages("plotly")
library(plotly)
library(tidyverse)
library(RSQLite)
library(ggthemes)
library(DBI)


con <- dbConnect(drv=SQLite(), dbname="basketball.sqlite")

tables <- dbListTables(con)

## exclude sqlite_sequence (contains table information)
tables <- tables[tables != "sqlite_sequence"]

lDataFrames <- vector("list", length=length(tables))

## create a data.frame for each table
for (i in seq(along=tables)) {
  lDataFrames[[i]] <- dbGetQuery(conn=con, statement=paste("SELECT * FROM '", tables[[i]], "'", sep=""))
}

## isolate specific (Reset)
players <- lDataFrames[[8]]
teams <- lDataFrames[[13]]
games <- lDataFrames[[3]]
games <- games[ , c("TEAM_ABBREVIATION_HOME", "TEAM_ABBREVIATION_AWAY", "SEASON_ID", "FT_PCT_HOME", "FT_PCT_AWAY")]    

games <- games %>%
  mutate(SEASON_ID = as.integer(SEASON_ID)) %>%
  filter(SEASON_ID > 21979)

glimpse(games)

## plots

##### PLOT 1
games_plot <- ggplot(games) +
  geom_point(aes(x = TEAM_ABBREVIATION_HOME, y = FT_PCT_HOME, colour = "Home"), alpha = .1) +
  geom_point(aes(x = TEAM_ABBREVIATION_AWAY, y = FT_PCT_AWAY, colour = "Away"), alpha = .1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(y = "FT Percentage", x = "Court", title = "Court Advantage by Free Throw Percentage", color = "Team")

games_plot

ggsave(
  "pictures/games_plot.png",
  width = 6,
  height = 4,
  )


##### PLOT 2
### Grouping by home court and season
games_by_season <- games %>%
  group_by(TEAM_ABBREVIATION_HOME, SEASON_ID) %>%
  summarise(AVG_FT_PCT_HOME = mean(FT_PCT_HOME),
            AVG_FT_PCT_AWAY = mean(FT_PCT_AWAY),
            ABS_DIFF = abs(AVG_FT_PCT_AWAY - AVG_FT_PCT_HOME))

### plot 2: FT percentage by team/season, showing home and away
games_plot_season <- ggplot(games_by_season) +
  geom_point(aes(x = TEAM_ABBREVIATION_HOME, y = AVG_FT_PCT_AWAY, color = "Away"), alpha = .2) +
  geom_point(aes(x = TEAM_ABBREVIATION_HOME, y = AVG_FT_PCT_HOME, color = "Home"), alpha = .2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(y = "FT Percentage", x = "Court", title = "Court Advantage by Free Throw Percentage, Grouped by Season", color = "Team")

games_plot_season

##### PLOT 3
### filter last decade, group by match ups, calculate average FT%
games_2010 <- filter(games, SEASON_ID>22014)

games_by_away <- games_2010 %>%
  group_by(TEAM_ABBREVIATION_HOME, TEAM_ABBREVIATION_AWAY) %>%
  summarise(AVG_FT_PCT_AWAY = mean(FT_PCT_AWAY))

### filter last decade, group by away teams's average FT%
team_avg_at_home <- games_2010 %>%
  group_by(TEAM_ABBR = TEAM_ABBREVIATION_HOME) %>%
  summarise(TEAM_AVG_HOME = mean(FT_PCT_HOME))

team_avg_away <- games_2010 %>%
  group_by(TEAM_ABBR = TEAM_ABBREVIATION_AWAY) %>%
  summarise(TEAM_AVG_AWAY = mean(FT_PCT_AWAY))

team_avg <- merge(team_avg_at_home, team_avg_away, by="TEAM_ABBR", all.x = TRUE)
team_avg$TEAM_ABBREVIATION_AWAY <- team_avg$TEAM_ABBR
team_avg$TEAM_AVG <- with(team_avg, (TEAM_AVG_HOME + TEAM_AVG_AWAY)/2 )


### calculate difference in average performance.
ft_diff_by_court <- merge(games_by_away, team_avg, by="TEAM_ABBREVIATION_AWAY", all.x = TRUE)
ft_diff_by_court$FT_DIFF <- with(ft_diff_by_court, AVG_FT_PCT_AWAY - TEAM_AVG)
ft_diff_agg <- ft_diff_by_court %>%
  group_by(TEAM_ABBREVIATION_HOME) %>%
  summarise(AVG_DIFF = mean(FT_DIFF))

highlight_df1 <- ft_diff_agg %>% 
  filter(AVG_DIFF<=-.011)
highlight_df2 <- ft_diff_agg %>% 
  filter(AVG_DIFF>=.011)

### plot 3: Away team free throw performance

games_plot_away_perf <- ggplot() +
  geom_point(data = ft_diff_by_court, 
             aes(x = TEAM_ABBREVIATION_HOME, y = FT_DIFF, color = "teams"), 
             alpha = .1,
             size = 2, 
             show.legend = FALSE) +
  geom_point(data = ft_diff_agg, 
             aes(x = TEAM_ABBREVIATION_HOME, y = AVG_DIFF, color = "away"),
             size = 2, 
             show.legend = FALSE) +
  geom_point(data=highlight_df1,
             aes(x=TEAM_ABBREVIATION_HOME,y=AVG_DIFF),
             color = "black",
             fill="orange",
             shape = 23,
             size = 4) +
  geom_point(data=highlight_df2,
             aes(x=TEAM_ABBREVIATION_HOME,y=AVG_DIFF),
             color = "black",
             fill="orange",
             shape = 23,
             size = 4) +
  geom_text(data=subset(ft_diff_by_court, abs(FT_DIFF) > .08),
            aes(x = TEAM_ABBREVIATION_HOME, y = FT_DIFF,label=TEAM_ABBREVIATION_AWAY),
            size = 3) + 
  coord_cartesian(ylim=c(-.11,.11)) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(y = "Away Team FT% +/-", x = "Home Court", title = "Away Team Free Throw Performance in +/- %")

games_plot_away_perf

