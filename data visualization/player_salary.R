library(dplyr)
library(tidyr)
library(readr)

#Load Data
MARKET_VALUE <- read_csv("players_fifa23.csv")

#Filter for LaLiga PLayers
la_liga<- c(
  "FC Barcelona", "Real Madrid CF", "Atlético de Madrid", "Real Sociedad",
  "Villarreal CF", "Real Betis Balompié", "CA Osasuna", "Athletic Club de Bilbao",
  "RCD Mallorca", "Girona FC", "Rayo Vallecano", "Sevilla FC", "RC Celta de Vigo",
  "Cádiz CF", "Getafe CF", "Valencia CF", "Unión Deportiva Almería", "Real Valladolid CF",
  "RCD Espanyol de Barcelona", "Elche CF"
)

MARKET_VALUE <-
  MARKET_VALUE %>%
  dplyr::filter(Club %in% la_liga) %>%
  dplyr::select(Club, ValueEUR)
  
club_avg_market_val <- 
  MARKET_VALUE %>%
  group_by(Club) %>%
  summarize(`Average Market Value (millions)` = sum(ValueEUR, na.rm = TRUE)) %>%
  arrange(desc(`Average Market Value (millions)`))

#Show number in millions for clarity
club_avg_market_val <-
  club_avg_market_val %>%
  dplyr::mutate(`Average Market Value (millions)` = 
                  round(`Average Market Value (millions)`/ 1000000, 1))

##Standings Data
#load file
STANDINGS <- read_csv("laliga_standings.csv")

STANDINGS <-
  STANDINGS[c('Rk', 'Squad')]

#Make squad names equal to other dataset for join

STANDINGS <-
  STANDINGS %>%
  dplyr::mutate(Squad = la_liga)

#Join standings with market value

value_place <-
  club_avg_market_val %>%
  left_join(STANDINGS, by = c("Club" = "Squad")) %>%
  rename(Rank = Rk)
