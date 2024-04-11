library(tidyverse)
library(ggthemes)
library(ggrepel)
library(rsvg)
library(dplyr)
library(gganimate)
library(transformr)
library(av)
library(zoo)


gamedata <- read_csv("C:\\Users\\nikhi\\Downloads\\cavs_ratings.csv")


ratings <- gamedata |> 
  filter(G >= 54)|>  
  mutate(rolling_ORtg = rollmeanr(ratings$ORtg,8,fill=NA)) |>  
  mutate(rolling_DRtg = rollmeanr(ratings$DRtg,8,fill=NA)) |> 
  filter(!is.na(rolling_DRtg))

ratings |> 
  ggplot(aes(x=G))+
  geom_line(aes(y = rolling_ORtg, color = "gold"), size = 1.5) +
  geom_line(aes(y = rolling_DRtg, color = "darkred"), size = 1.5, alpha=0.5) +
  scale_color_identity(guide="legend")+
  scale_color_manual(values = c("darkred", "gold"), 
                     labels = c("Rolling Average DRtg", "Rolling Average ORtg")) +
  labs(color = "")+
  theme_fivethirtyeight()+
  labs(
    title="Cavs Offensive and Defensive Ratings Since 2/22",
    subtitle="8-Game Rolling Average",
    caption = "Twitter/X: @nikhil_chinchal\nSource: Basketball Reference"
  )+
  theme(axis.title = element_text()) + 
  theme(plot.title = element_text(size = 22, hjust =0.5, face = "bold"),
        plot.subtitle = element_text(size = 16, hjust =0.5),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5))+
  scale_x_continuous(breaks=ratings$G, labels = ratings$Date)+
  scale_y_continuous(breaks=scales::pretty_breaks(n=8))+
  ylab("ORtg / DRtg")+
  xlab("Date")





