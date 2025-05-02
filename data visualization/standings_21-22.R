library(knitr)
library(kableExtra)
library(readr)
library(rvest)



url <- "https://www.espn.com/soccer/standings/_/league/ESP.1/season/2021"
standings_2022 <- read_html(url) %>%
  html_table() %>%
  as.data.frame()

la_liga_2022 <- c("Real Madrid CF", "FC Barcelona", "Atlético de Madrid", "Sevilla FC",
  "Real Betis Balompié", "Real Sociedad", "Villarreal CF", "Athletic Club de Bilbao",
  "Valencia CF", "CA Osasuna", "RC Celta de Vigo", "Rayo Vallecano",
  "Elche CF", "RCD Espanyol de Barcelona", "Getafe CF", "RCD Mallorca",
  "Cádiz CF", "Granada CF", "Levante UD", "Deportivo Alavés")

standings_2022 <-
  standings_2022 %>%
  
  rename(Club = 1) %>%
  
  #add universal club names from datasets
  mutate(Club = la_liga_2022)%>%
  
  #add season end rank
  mutate(Rank = seq(1, nrow(standings_2022)))

standings_2022 <-
  standings_2022[c(10,1,2,3,4,5,6,7,8,9)]

kable(standings_2022, caption = "Final LaLiga Standings for 2021-22") %>%
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