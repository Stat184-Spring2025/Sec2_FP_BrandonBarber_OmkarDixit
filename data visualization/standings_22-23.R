library(knitr)
library(kableExtra)
library(readr)
library(rvest)

#load file
#STANDINGS <- read_csv("laliga_standings.csv")

url <- "https://www.espn.com/soccer/standings/_/league/ESP.1/season/2022"
standings <- read_html(url) %>%
  html_table() %>%
  as.data.frame()

la_liga_2023<- c(
  "FC Barcelona", "Real Madrid CF", "Atlético de Madrid", "Real Sociedad",
  "Villarreal CF", "Real Betis Balompié", "CA Osasuna", "Athletic Club de Bilbao",
  "RCD Mallorca", "Girona FC", "Rayo Vallecano", "Sevilla FC", "RC Celta de Vigo",
  "Cádiz CF", "Getafe CF", "Valencia CF", "Unión Deportiva Almería", "Real Valladolid CF",
  "RCD Espanyol de Barcelona", "Elche CF"
)

standings <-
  standings %>%
  dplyr::select(1) %>%
  rename(Club = 1) %>%

#import club names that are universal throughout datasets
  mutate(Club = la_liga_2023)%>%

#add a column for rank
  mutate(Rank = seq(1, nrow(standings)))

#Create Table
 standings <- kable(standings, caption = "Final LaLiga Standings for 2022-23") %>%
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
