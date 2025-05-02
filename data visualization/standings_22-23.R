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
  dplyr::select(1) %>%
  rename(Club = 1) %>%

#use club names universal across datasets
  mutate(Club = la_liga_2023)%>%

#add end of season rnak column
  mutate(Rank = seq(1, nrow(standings_2023)))

#Create Table
 standings_2023 <- kable(standings_2023, caption = "Final LaLiga Standings for 2022-23") %>%
  kable_styling() %>%
  footnote(general =
             c("MP - Matches Played",
               "W - Wins",
               "L - Losses",
               "GF - Goals For",
               "GA - Goals Against",
               "GD - Goal Differential",
               "Pts - Points"),
           general_title = "Legend:")
