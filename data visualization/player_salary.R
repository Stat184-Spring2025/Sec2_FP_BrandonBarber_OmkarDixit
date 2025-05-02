library(dplyr)
library(tidyr)
library(readr)
library(rvest)

##Player value
#Load Data
player_value_raw <- read_csv("players_fifa23.csv")

#Filter for LaLiga PLayers
la_liga<- c(
  "FC Barcelona", "Real Madrid CF", "Atlético de Madrid", "Real Sociedad",
  "Villarreal CF", "Real Betis Balompié", "CA Osasuna", "Athletic Club de Bilbao",
  "RCD Mallorca", "Girona FC", "Rayo Vallecano", "Sevilla FC", "RC Celta de Vigo",
  "Cádiz CF", "Getafe CF", "Valencia CF", "Unión Deportiva Almería", "Real Valladolid CF",
  "RCD Espanyol de Barcelona", "Elche CF"
)

player_value <-
  player_value_raw %>%
  
  #select relevant columns
  dplyr::filter(Club %in% la_liga) %>%
  dplyr::select(Club, ValueEUR) %>%
  
  group_by(Club) %>%
  summarize(`Total Player Value (millions)` = sum(ValueEUR, na.rm = TRUE))%>%
  
  #Show number in millions for clarity
  dplyr::mutate(`Total Player Value (millions)` = 
                  round(`Total Player Value (millions)`/ 1000000, 1))

##Standings Data
url <- "https://www.espn.com/soccer/standings/_/league/ESP.1/season/2022"
standings_2023 <- read_html(url) %>%
  html_table() %>%
  as.data.frame()

la_liga_2023<- c(
  "FC Barcelona", "Real Madrid CF", "Atlético de Madrid", "Real Sociedad",
  "Villarreal CF", "Real Betis Balompié", "CA Osasuna", "Athletic Club de Bilbao",
  "RCD Mallorca", "Girona FC", "Rayo Vallecano", "Sevilla FC", "RC Celta de Vigo",
  "Cádiz CF", "Getafe CF", "Valencia CF", "Unión Deportiva Almería", "Real Valladolid CF",
  "RCD Espanyol de Barcelona", "Elche CF"
)

standings_2023 <-
  standings_2023 %>%
  dplyr::select(1) %>%
  rename(Club = 1) %>%
  
  mutate(Club = la_liga_2023)%>%
  
  mutate(Rank = seq(1, nrow(standings_2023)))

## Club value data
#load file
club_value_raw <- read_csv("club_value.csv")

club_value <-
  club_value_raw %>%
  
  #Remove unneccesarry columns
  dplyr::select(Club, Value) %>%
  
  #Make squad names equal to other dataset for join
  dplyr::mutate(Club = la_liga) %>%
  
  #Show value in millions
  dplyr::mutate(Value = 
                  round(Value/ 1000000, 1)) %>%
  dplyr::rename(`Club Value (millions)` = Value)

#join tables

value_insight <-
  standings_2023 %>%
  left_join(club_value, by = "Club") %>%
  
  left_join(player_value, by = "Club" )

##Plot

ggplot(value_insight, aes(x= Rank,
                          y = `Total Player Value (millions)`,
                          size = `Club Value (millions)`)) + 
  geom_point() +
  
  labs(title = "La Liga 22-23: Total Player Values vs Standings") +
  
  scale_x_continuous(n.breaks = 10)+
  
  scale_y_continuous(n.breaks = 10)+
  
  # geom_text(aes(label = `Club`),size = 2) +
  
  theme_minimal()
