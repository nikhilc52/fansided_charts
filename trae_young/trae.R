library(tidyverse)
library(ggthemes)
library(ggrepel)
library(dplyr)
library(zoo)
library(showtext)

gamedata <- read_csv('data.csv')

# font_add_google("EB Garamond")
# showtext_auto()

gamedata <- gamedata |> 
  filter(!is.na(Gcar))

gamedata <- gamedata |> 
  mutate(TS = PTS/(2*(FGA + (0.44 * FTA))))

data <- gamedata |>  
  mutate(rolling_fgp = rollmeanr(`FG%`,20,fill=NA)) |>  
  mutate(rolling_tpp = rollmeanr(`3P%`,20,fill=NA)) |>  
  mutate(rolling_tsp = rollmeanr(`TS`,20,fill=NA))

data |> 
  ggplot(aes(x=Gcar))+
  geom_line(aes(y = rolling_fgp, color = "#C8102E"), size = 1.5, alpha=1) +
  geom_line(aes(y = rolling_tpp, color = "#FDB927"), size = 1.5, alpha=1) +
  geom_line(aes(y = rolling_tsp, color = "#9EA2A2"), size = 1.5, alpha=1) +
  scale_color_identity()+
  labs(
    title="Trae Young's shooting efficiency for the last 3 seasons",
    subtitle="20-Game Rolling Averages of FG%, 3P%, and TS% for started games",
    caption = "Nikhil Chinchalkar | Source: Basketball Reference"
  )+
  ylab("")+
  xlab("Career Game #")+
  geom_vline(xintercept = 408, linetype="dashed", color="black")+
  geom_vline(xintercept = 354, linetype="dashed", color="black")+
  geom_vline(xintercept = 281, linetype="dashed", color="black")+
  
  scale_x_continuous(breaks=scales::pretty_breaks(n=8))+
  scale_y_continuous(breaks=scales::pretty_breaks(n=8), labels=scales::label_percent())+
  
  theme_set(theme_gray(base_size = 20, base_family = "EB Garamond" ))+
  theme(plot.title = element_text(size = 30, hjust =0, face = "bold",color="black"), 
        plot.subtitle = element_text(size = 22, hjust =0, color="black"))+
  guides(color = guide_legend(title.position = "top", 
                              title.hjust = 0.5,
                              label.position = "bottom"))+
  theme(legend.title = element_text( size=9), legend.text=element_text(size=9))+
  theme(panel.background = element_rect(fill="white", color="white"))+
  theme(panel.grid.major = element_line(color="#e3e3e3"))+
  theme(legend.background = element_rect(fill="white", color="white"))+
  theme(axis.ticks.x = element_line(colour="#e3e3e3"))+
  theme(axis.ticks.y = element_line(colour="#e3e3e3"))+
  theme(legend.box.background = element_rect(fill="white", color="white"))
