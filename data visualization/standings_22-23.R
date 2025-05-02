library(knitr)
library(kableExtra)
library(readr)
library(rvest)



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
  
  rename(Club = 1) %>%
  
  #add universal club names from datasets
  mutate(Club = la_liga_2023)%>%
  
  #add season end rank
  mutate(Rank = seq(1, nrow(standings_2023)))

standings_2023 <-
  standings_2023[c(10,1,2,3,4,5,6,7,8,9)]

kable(standings_2023, caption = "Final LaLiga Standings for 2022-23") %>%
  kable_styling() %>%
  footnote(general =
             c("GP - Matches Played",
               "W - Wins",
               "D - Draws",
               "L - Losses",
               "F - Goals For",
               "A - Goals Against",
               "GD - Goal Differential",
               "P - Points"),
           general_title = "Legend:")
