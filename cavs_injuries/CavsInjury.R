library(nbastatR)
Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)
library(tidyverse)
library(gt)
library(gtExtras)
library(ggimage)
library(ggthemes)
library(ggrepel)
library(rsvg)
library(dplyr)
library(gganimate)
library(transformr)
library(av)


gamedata <- game_logs(seasons = 2024)

cavs <- gamedata |> 
  filter(slugTeam == "CLE") |> 
  group_by(numberGameTeamSeason) |> 
  summarize(
    threeptA = sum(fg3a),
    threeptM = sum(fg3m),
    threeptper = sum(fg3m)/sum(fg3a)
  )

cavsA <- read.csv("C:\\Users\\nikhi\\Downloads\\CavsAdvanced2.csv")





average_values <- cavs |> 
  slice(27:99) |> 
  summarize_all(mean, na.rm = TRUE)

average_values <- cavsA |> 
  slice(27:99) |> 
  summarize_all(mean, na.rm = TRUE)






p <- cavs |> 
  filter(numberGameTeamSeason < 45) |> 
  ggplot(aes(x = numberGameTeamSeason, y = threeptA, color = "gold")) +
  geom_line(size = 1.5) +
  labs(color = "")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text()) + 
  labs(
    title="Cavaliers Three Point Attempts",
    subtitle="Pre/Post Garland and Mobley Injuries",
    caption = "Twitter/X: @nikhil_chinchal\nSource: nbastatR"
  )+
  ylab("3PT Attempts")+
  xlab("Game #")+
  theme(plot.title = element_text(size = 22, hjust =0.5, face = "bold"),
        plot.subtitle = element_text(size = 16, hjust =0.5))+
  scale_x_continuous(breaks=scales::pretty_breaks(n=15))+
  scale_y_continuous(breaks=scales::pretty_breaks(n=10))+
  
  annotate("text", x=4, y=32.5, label="Avg. 3PA Pre-Injuries", color ="black", size=2.5)+
  annotate("text", x=39.7, y=43.25, label="Avg. 3PA Post-Injuries", color ="black", size=2.5)+
  geom_vline(xintercept = 26, linetype="dashed", color="darkred")+
  annotate("text", x=25, y=46, label="Garland and Mobely Get Injured", angle=90, color ="darkred")+
  scale_color_manual(name = "", values = c("gold" = "gold"), labels = "Attempts")+
  geom_segment(aes(x=0,xend=26,y=33.5,yend=33.5), color="black", linetype="dashed")+
  geom_segment(aes(x=26,xend=44,y=41.9,yend=41.9), color="black", linetype="dashed")+
  transition_reveal(numberGameTeamSeason)

anim_save(
  "animation.mp4", 
  animate(p, fps=8, renderer = av_renderer())
)

cavsA |> 
  ggplot(aes(x = G, y = ORtg, color = "gold")) +
  geom_line(size = 1.5) +
  labs(color = "")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text()) + 
  labs(
    title="Cavaliers Offensive Rating",
    subtitle="Pre/Post Garland and Mobley Injuries",
    caption = "Twitter/X: @nikhil_chinchal\nSource: Basketball Reference"
  )+
  ylab("ORtg")+
  xlab("Game #")+
  theme(plot.title = element_text(size = 22, hjust =0.5, face = "bold"),
        plot.subtitle = element_text(size = 16, hjust =0.5))+
  scale_x_continuous(breaks=scales::pretty_breaks(n=15))+
  scale_y_continuous(breaks=scales::pretty_breaks(n=10))+
  
  annotate("text", x=4.3, y=112, label="Avg. ORtg Pre-Injuries", color ="black", size=2.5)+
  annotate("text", x=39.7, y=122, label="Avg. ORtg Post-Injuries", color ="black", size=2.5)+
  geom_vline(xintercept = 26, linetype="dashed", color="darkred")+
  annotate("text", x=25, y=130, label="Garland and Mobely Get Injured", angle=90, color ="darkred")+
  scale_color_manual(name = "", values = c("gold" = "gold"), labels = "ORtg")+
  geom_segment(aes(x=0,xend=26,y=113.4,yend=113.4), color="black", linetype="dashed")+
  geom_segment(aes(x=26,xend=44,y=120.7,yend=120.7), color="black", linetype="dashed")




