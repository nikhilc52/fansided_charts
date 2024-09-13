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
#install.packages("stringr")
library(stringr)


gamedata_2 <- game_logs(seasons = 2024)
#names(gamedata)

#df <- read.csv("C:\\Users\\nikhi\\Downloads\\KnicksBench.csv")

knicks_all_2 <- gamedata_2 |> 
  filter(slugTeam == "NYK") |> 
  filter(numberGameTeamSeason < 40) |> 
  left_join(df, by = c("numberGameTeamSeason" = "G")) |> 
  mutate(Player.1 = word(Player.1,-1)) |> 
  mutate(Player.2 = word(Player.2,-1)) |> 
  mutate(Player.3 = word(Player.3,-1)) |> 
  mutate(Player.4 = word(Player.4,-1)) |> 
  mutate(Player.5 = word(Player.5,-1))

knicks_bench_real <- knicks_all_2 |> 
  filter(!(word(namePlayer,-1)==Player.1 | word(namePlayer,-1)==Player.2 
           | word(namePlayer,-1)==Player.3 | word(namePlayer,-1)==Player.4
           | word(namePlayer,-1)==Player.5))

knicks_bench <- knicks_bench_real |> 
  group_by(numberGameTeamSeason) |> 
  summarize(
    benchPoints = (sum(pts)),
    benchPointsPercent = 100*first(benchPoints/Tm),
    players = n(),
    WL = first(X)
  )

average_values <- knicks_bench |> 
  slice(0:31) |> 
  summarize_all(mean, na.rm = TRUE)

knicks_bench |> 
  ggplot(aes(x = numberGameTeamSeason, y = benchPoints, color = "gold")) +
  geom_line(size = 1.5) +
  labs(color = "")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text()) + 
  labs(
    title="Knicks' Bench Points",
    subtitle="Pre/Post Immanuel Quickley Trade",
    caption = "Twitter/X: @nikhil_chinchal\nSource: nbastatR + Basketball Reference"
  )+
  ylab("Count")+
  xlab("Game #")+
  theme(plot.title = element_text(size = 22, hjust =0.5, face = "bold"),
        plot.subtitle = element_text(size = 16, hjust =0.5))+
  scale_x_continuous(breaks=scales::pretty_breaks(n=15))+
  scale_y_continuous(breaks=scales::pretty_breaks(n=10))+
  annotate("text", x=6, y=36, label="Avg. Bench Scoring Pre-Trade", color ="black", size=2.5)+
  annotate("text", x=36.5, y=25, label="Avg. Bench Scoring Post-Trade", color ="black", size=2.5)+
  geom_vline(xintercept = 31.5, linetype="dashed", color="darkred")+
  annotate("text", x=32.25, y=38, label="I. Quickley Traded", angle=90, color ="darkred")+
  scale_color_manual(name = "", values = c("gold" = "gold"), labels = "Points")+
  geom_segment(aes(x=0,xend=31.5,y=34.48387,yend=34.48387), color="black", linetype="dashed")+
  geom_segment(aes(x=31.5,xend=41.23,y=26.375,yend=26.375), color="black", linetype="dashed")

knicks_bench |> 
  ggplot(aes(x = numberGameTeamSeason, y = benchPointsPercent, color = "gold")) +
  geom_line(size = 1.5) +
  labs(color = "")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text()) + 
  labs(
    title="Knicks' Bench Points As A Percent of Team Points",
    subtitle="Pre/Post Immanuel Quickley Trade",
    caption = "Twitter/X: @nikhil_chinchal\nSource: nbastatR + Basketball Reference"
  )+
  ylab("Percent")+
  xlab("Game #")+
  theme(plot.title = element_text(size = 22, hjust =0.5, face = "bold"),
        plot.subtitle = element_text(size = 16, hjust =0.5))+
  scale_x_continuous(breaks=scales::pretty_breaks(n=15))+
  scale_y_continuous(breaks=scales::pretty_breaks(n=10))+
  annotate("text", x=6, y=31, label="Avg. % Bench Scoring Pre-Trade", color ="black", size=2.5)+
  annotate("text", x=36, y=23, label="Avg. % Bench Scoring Post-Trade", color ="black", size=2.5)+
  geom_vline(xintercept = 31.5, linetype="dashed", color="darkred")+
  annotate("text", x=32.25, y=38, label="I. Quickley Traded", angle=90, color ="darkred")+
  scale_color_manual(name = "", values = c("gold" = "gold"), labels = "Percentage of Points")+
  geom_segment(aes(x=0,xend=31.5,y=30.0457,yend=30.0457), color="black", linetype="dashed")+
  geom_segment(aes(x=31.5,xend=40.5,y=22.22192,yend=22.22192), color="black", linetype="dashed")

         