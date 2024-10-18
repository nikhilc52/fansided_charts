library(readr)
team_pace_data <- read_csv("GitHub/fansided_charts/cavs_atkinson/team_pace_data.csv")

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
       Each dot is a NBA team", 
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
