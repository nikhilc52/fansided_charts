library(nbastatR)
Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)
library(tidyverse)
library(gt)
library(gtExtras)
library(ggimage)
library(ggthemes)
library(ggrepel)
#install.packages("rsvg")
library(rsvg)
library(dplyr)


gamedata <- game_logs(seasons = 2024)
names(gamedata)

knicks_all <- gamedata |> 
  filter(slugTeam == "NYK") |> 
  filter(numberGameTeamSeason < 40)

knicks_all <- knicks_all %>%
  arrange(idGame, desc(minutes))

knicks_starters <- knicks_all %>%
  group_by(idGame) %>%
  slice_head(n = 5)

knicks_bench <- anti_join(knicks_all, knicks_starters, by = c("idGame", "idPlayer"))

knicks_starters_summary <- knicks_all %>%
  group_by(idGame) %>%
  slice_head(n = 5) |> 
  group_by(numberGameTeamSeason) |> 
  summarize(
    pointsStarters = sum(pts)
    
  )

knicks_bench_final <- knicks_bench |> 
  group_by(numberGameTeamSeason) |> 
  summarize(
    points = sum(pts),
    bench_players = n()
  )

knicks_bench_final <- knicks_bench_final |> 
  left_join(knicks_starters_summary, by = ("numberGameTeamSeason"="numberGameTeamSeason"))
  
knicks_bench_final <- knicks_bench_final |> 
  mutate(percentOfTotal = points/(points+pointsStarters))



knicks_bench_final |> 
  ggplot(aes(x = numberGameTeamSeason, y = points, color = "gold")) +
  geom_line(size = 1.5) +
  labs(color = "")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text()) + 
  labs(
    title="Knicks' Bench Points",
    subtitle="Where bench players are not top 5 in minutes in each game",
    caption = "Twitter/X: @nikhil_chinchal\nSource: nbastatR"
  )+
  ylab("Count")+
  xlab("Game #")+
  theme(plot.title = element_text(size = 22, hjust =0.5, face = "bold"),
        plot.subtitle = element_text(size = 16, hjust =0.5))+
  scale_x_continuous(breaks=scales::pretty_breaks(n=15))+
  scale_y_continuous(breaks=scales::pretty_breaks(n=10))+
  annotate("text", x=10, y=32, label="Approx. Knicks Avg. Bench Scoring", color ="black")+
  geom_hline(yintercept = mean(knicks_bench_final$points), linetype="dashed", color="black")+
  geom_vline(xintercept = 32, linetype="dashed", color="darkred")+
  annotate("text", x=32.75, y=38, label="I. Quickley Traded", angle=90, color ="darkred")+
  scale_color_manual(name = "", values = c("gold" = "gold"), labels = "Points")


knicks_bench_final |> 
  ggplot(aes(x = numberGameTeamSeason, y = percentOfTotal, color = "gold")) +
  geom_line(size = 1.5) +
  labs(color = "")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text()) + 
  labs(
    title="Knicks' Bench Points As A Percent of Total Points",
    subtitle="Where bench players are not top 5 in minutes in each game",
    caption = "Twitter/X: @nikhil_chinchal\nSource: nbastatR"
  )+
  ylab("Percent")+
  xlab("Game #")+
  theme(plot.title = element_text(size = 22, hjust =0.5, face = "bold"),
        plot.subtitle = element_text(size = 16, hjust =0.5))+
  scale_x_continuous(breaks=scales::pretty_breaks(n=15))+
  scale_y_continuous(breaks=scales::pretty_breaks(n=10))+
  annotate("text", x=5, y=.27, label="Avg. Bench %", color ="black")+
  geom_hline(yintercept = mean(knicks_bench_final$percentOfTotal), linetype="dashed", color="black")+
  geom_vline(xintercept = 31.5, linetype="dashed", color="darkred")+
  annotate("text", x=32, y=.38, label="I. Quickley Traded", angle=90, color ="darkred")+
  scale_color_manual(name = "", values = c("gold" = "gold"), labels = "Percent Of Points")

