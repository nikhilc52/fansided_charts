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

off <- read.csv("C:\\Users\\nikhi\\Downloads\\CavsOffCombinedData20.csv")
on <- read.csv("C:\\Users\\nikhi\\Downloads\\CavsOnCombinedData20.csv")

colors <- data.frame(team_name=c("Atlanta Hawks","Boston Celtics","Brooklyn Nets","Charlotte Hornets","Chicago Bulls","Cleveland Cavaliers","Dallas Mavericks",
                                 "Denver Nuggets","Detroit Pistons","Golden State Warriors","Houston Rockets","Indiana Pacers",
                                 "LA Clippers","Los Angeles Lakers","Memphis Grizzlies","Miami Heat",
                                 "Milwaukee Bucks","Minnesota Timberwolves","New Orleans Pelicans","New York Knicks",
                                 "Oklahoma City Thunder","Orlando Magic","Philadelphia 76ers","Phoenix Suns",
                                 "Portland Trail Blazers","Sacramento Kings","San Antonio Spurs","Toronto Raptors","Utah Jazz",
                                 "Washington Wizards"), 
                     team_abbr = c("ATL","BOS","BKN","CHA","CHI","CLE","DAL",
                                   "DEN","DET","GSW","HOU","IND",
                                   "LAC","LAL","MEM","MIA",
                                   "MIL","MIN","NOP","NYK",
                                   "OKC","ORL","PHI","PHX",
                                   "POR","SAC","SAS","TOR","UTA",
                                   "WAS"),
                     team_color=c("#E2363D","#041A24","#06864C", "#1C0C65","#CC1244","#B71F38","#C6CFD4",
                                  "#FDB827","#006BB7","#FFCD25","#D31145",
                                  "#FFC422","#006BB7","#FDC82F","#6189B9","#98012E"
                                  ,"#003614","#005084","#B5985A","#F4822C","#007DC3","#C4C9CB",
                                  "#0046AD","#F9A01B","#E2383F","#393996","#C4CED4","#231F20",
                                  "#FCA204","#E41634"),
                     team_logo_png=c("https://cdn.nba.com/logos/nba/1610612737/primary/L/logo.svg",
                                     "https://cdn.nba.com/logos/nba/1610612738/primary/L/logo.svg",
                                     "https://cdn.nba.com/logos/nba/1610612751/primary/L/logo.svg",
                                     "https://cdn.nba.com/logos/nba/1610612766/primary/L/logo.svg",
                                     "https://cdn.nba.com/logos/nba/1610612741/primary/L/logo.svg",
                                     "https://cdn.nba.com/logos/nba/1610612739/primary/L/logo.svg",
                                     "https://cdn.nba.com/logos/nba/1610612742/primary/L/logo.svg",
                                     "https://cdn.nba.com/logos/nba/1610612743/primary/L/logo.svg",
                                     "https://cdn.nba.com/logos/nba/1610612765/primary/L/logo.svg",
                                     "https://cdn.nba.com/logos/nba/1610612744/primary/L/logo.svg",
                                     "https://cdn.nba.com/logos/nba/1610612745/primary/L/logo.svg",
                                     "https://cdn.nba.com/logos/nba/1610612754/primary/L/logo.svg",
                                     "https://cdn.nba.com/logos/nba/1610612746/primary/L/logo.svg",
                                     "https://cdn.nba.com/logos/nba/1610612747/primary/L/logo.svg",
                                     "https://cdn.nba.com/logos/nba/1610612763/primary/L/logo.svg",
                                     "https://cdn.nba.com/logos/nba/1610612748/primary/L/logo.svg",
                                     "https://cdn.nba.com/logos/nba/1610612749/primary/L/logo.svg",
                                     "https://cdn.nba.com/logos/nba/1610612750/primary/L/logo.svg",
                                     "https://cdn.nba.com/logos/nba/1610612740/primary/L/logo.svg",
                                     "https://cdn.nba.com/logos/nba/1610612752/primary/L/logo.svg",
                                     "https://cdn.nba.com/logos/nba/1610612760/primary/L/logo.svg",
                                     "https://cdn.nba.com/logos/nba/1610612753/primary/L/logo.svg",
                                     "https://cdn.nba.com/logos/nba/1610612755/primary/L/logo.svg",
                                     "https://cdn.nba.com/logos/nba/1610612756/primary/L/logo.svg",
                                     "https://cdn.nba.com/logos/nba/1610612757/primary/L/logo.svg",
                                     "https://cdn.nba.com/logos/nba/1610612758/primary/L/logo.svg",
                                     "https://cdn.nba.com/logos/nba/1610612759/primary/L/logo.svg",
                                     "https://cdn.nba.com/logos/nba/1610612761/primary/L/logo.svg",
                                     "https://cdn.nba.com/logos/nba/1610612762/primary/L/logo.svg",
                                     "https://cdn.nba.com/logos/nba/1610612764/primary/L/logo.svg"))


mvp_candidates <- c("Mitchell, Donovan","Jokic, Nikola", "Gilgeous-Alexander, Shai",
                    "Dončić, Luka", "Antetokounmpo, Giannis", "Tatum, Jayson", 
                    "Sabonis, Domantas", "Haliburton, Tyrese", "Booker, Devin", "Davis, Anthony")

plotOffOn1 <- on |> 
  left_join(off, by = c("VS_PLAYER_ID" = "VS_PLAYER_ID")) |> 
  mutate(team_alpha = ifelse(VS_PLAYER_NAME.x %in% mvp_candidates, 0.9, 0.1)) |> 
  left_join(colors, by = c("TEAM_NAME.x" = "team_name")) |> 
  filter(GP.x > 20) |> 
  filter(MIN.x > 700)




plotOffOn1 |> 
  ggplot(aes(x=DEF_RATING.x, y=DEF_RATING.y)) +
  geom_point(aes(fill=team_color, color=team_color, alpha = team_alpha, size = MIN.x),
             shape=20, show.legend = FALSE) +
  scale_color_identity(aesthetics = c("fill","color"))+
  scale_x_continuous(breaks=scales::pretty_breaks(n=8))+
  scale_y_continuous(breaks=scales::pretty_breaks(n=8))+
  theme_fivethirtyeight()+  
  theme(axis.title = element_text()) + 
  ylab('Defensive Rating Off-Court') + 
  xlab('Defensive Rating On-Court') +
  labs(title="Donovan Mitchell Leading the MVP Race Since 12/16/23", 
       subtitle = "Min. 20 GP, 700 Min, Size = # Minutes", 
       caption="Twitter/X: @nikhil_chinchal\nData: NBA_API\nDate: 2/12/24")+
  theme(plot.title = element_text(size = 22, hjust =0.5, face = "bold"), 
        plot.subtitle = element_text(size = 16, hjust =0.5))+
  geom_hline(yintercept = mean(plotOffOn1$DEF_RATING.y), linetype="dashed")+
  geom_vline(xintercept = mean(plotOffOn1$DEF_RATING.x), linetype="dashed")+
  geom_text_repel(aes(label = ifelse(VS_PLAYER_NAME.x %in% mvp_candidates, VS_PLAYER_NAME.x, "")),
                  min.segment.length = unit(0, 'lines'),position = position_nudge_repel(x = 2, y = 2),
                  max.overlaps = Inf)+
  
  scale_y_reverse()+
  scale_x_reverse()+
  geom_point(data=plotOffOn1 |> filter(VS_PLAYER_NAME.x == "Mitchell, Donovan"), pch=0,
             size=11, color="red", stroke=2.5)

#geom_text_repel(aes(label = ifelse(on.1 > 10, Player, "")),
#                position = position_nudge_repel(x = 10, y = 1), 
#                max.overlaps = Inf)
#annotate("text", x=-10, y=-10, label="Low individual impact on team", color ="red", size=2.5)+
#annotate("text", x=10, y=10, label="Low individual impact on team", color ="red", size=2.5)+
#annotate("text", x=10, y=-10, label="High positive individual impact on team", color ="darkgreen", size=2.5)+
#annotate("text", x=-10, y=10, label="Low positive individual impact on team", color ="red", size=2.5)


#geom_image(data = subset(plot, Player == "Donovan Mitchell"),
#aes(image = team_logo_png), asp = 16/9, size = 0.05)
#annotate("text",x=9.35,y=1.05, label="x", angle=270, size=3)
