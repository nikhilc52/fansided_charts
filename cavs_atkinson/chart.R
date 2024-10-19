library(readr)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(ggimage)

team_pace_data <- read_csv("GitHub/fansided_charts/cavs_atkinson/team_pace_data.csv")

colors <- data.frame(team_name=c("Atlanta Hawks","Boston Celtics","Brooklyn Nets","Charlotte Hornets","Chicago Bulls","Cleveland Cavaliers","Dallas Mavericks",
                                 "Denver Nuggets","Detroit Pistons","Golden State Warriors","Houston Rockets","Indiana Pacers",
                                 "Los Angeles Clippers","Los Angeles Lakers","Memphis Grizzlies","Miami Heat",
                                 "Milwaukee Bucks","Minnesota Timberwolves","New Orleans Pelicans","New York Knicks",
                                 "Oklahoma City Thunder","Orlando Magic","Philadelphia 76ers","Phoenix Suns",
                                 "Portland Trail Blazers","Sacramento Kings","San Antonio Spurs","Toronto Raptors","Utah Jazz",
                                 "Washington Wizards"), 
                     team_abbreviation = c("ATL","BOS","BKN","CHA","CHI","CLE","DAL",
                                           "DEN","DET","GSW","HOU","IND",
                                           "LAC","LAL","MEM","MIA",
                                           "MIL","MIN","NOP","NYK",
                                           "OKC","ORL","PHI","PHX",
                                           "POR","SAC","SAS","TOR","UTA",
                                           "WAS"),
                     team_color=c("#E2363D","#06864C","#041A24", "#1C0C65","#CC1244","#B71F38","#C6CFD4",
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


team_pace_data <- team_pace_data |> 
  left_join(colors, by = c("Team" = "team_name")) |>
  mutate(team_alpha = ifelse(Team == "Brooklyn Nets" & SEASON_YEAR != "2022-23" & SEASON_YEAR != "2023-24" 
                             & SEASON_YEAR != "2020-21" & SEASON_YEAR != "2021-22"
                             | Team == "Cleveland Cavaliers" & (SEASON_YEAR == "2022-23" | SEASON_YEAR == "2023-24"), 0.9, 0.2))


team_pace_data |> 
  ggplot(aes(x=SEASON_YEAR, y=PACE)) +
  geom_jitter(shape=21, size=4, aes(fill=team_color, color=team_color, alpha=team_alpha), width=0.15)+
  scale_color_identity(aesthetics = c("fill","color"))+
  theme_fivethirtyeight()+
  theme(axis.title = element_text()) +
  ylab('Pace') + 
  xlab('Season')+
  labs(title="Every NBA Team's Pace Since 2016", 
       subtitle = "Teams coached by Kenny Atkinson had much higher Pace than what the Cavs had under Bickerstaff<br>
       Each dot is an NBA team", 
       caption="Twitter/X: @nikhil_chinchal\nData: NBA_API")+
  theme(plot.title = element_text(size = 22, hjust =0.5, face = "bold"), 
        plot.subtitle = ggtext::element_markdown(size = 16, hjust =0.5))+
  guides(fill=F, alpha=F)+
  geom_line(data = subset(team_pace_data, Team == "Brooklyn Nets" & SEASON_YEAR != "2022-23"
                          & SEASON_YEAR != "2023-24" & SEASON_YEAR != "2020-21" & SEASON_YEAR != "2021-22"),
            aes(group = 1), color = "#041A24", linewidth=1.5)+
  geom_image(data = subset(team_pace_data, Team == "Brooklyn Nets" & SEASON_YEAR != "2022-23"
                           & SEASON_YEAR != "2023-24" & SEASON_YEAR != "2020-21" & SEASON_YEAR != "2021-22"),
             aes(image = team_logo_png), asp = 16/9, size = 0.05)+
  geom_image(data = subset(team_pace_data, Team == "Cleveland Cavaliers" & (SEASON_YEAR == "2022-23" | SEASON_YEAR == "2023-24")),
             aes(image = team_logo_png), asp = 16/9, size = 0.05)+
  geom_line(data = subset(team_pace_data, Team == "Cleveland Cavaliers" & (SEASON_YEAR == "2022-23" | SEASON_YEAR == "2023-24")),
            aes(group = 1), color="#98012E", linewidth=1.5)+
  annotate("text", x="2017-18",y=104, label="The Nets when Atkinson coached them")+
  annotate("text", x="2022-23",y=94, label="The Cavs under Bickerstaff", color="#98012E")
