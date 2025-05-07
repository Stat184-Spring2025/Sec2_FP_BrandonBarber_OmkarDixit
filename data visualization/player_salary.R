
library(dplyr)
library(tidyr)
library(readr)
library(rvest)
library(ggplot2)
library(scales)

##Player value

#Load Data
player_value_raw <- read_csv("players_fifa23.csv")

#La Liga teams list to filter for La Liga players
la_liga<- c(
  "FC Barcelona", "Real Madrid CF", "Atlético de Madrid", "Real Sociedad",
  "Villarreal CF", "Real Betis Balompié", "CA Osasuna", "Athletic Club de Bilbao",
  "RCD Mallorca", "Girona FC", "Rayo Vallecano", "Sevilla FC", "RC Celta de Vigo",
  "Cádiz CF", "Getafe CF", "Valencia CF", "Unión Deportiva Almería", "Real Valladolid CF", "RCD Espanyol de Barcelona", "Elche CF"
)

#Filter for La Liga players
#Group by club
#Sum total player value

player_value <-
  player_value_raw %>%
  
  #select relevant columns
  dplyr::filter(Club %in% la_liga) %>%
  dplyr::select(Club, ValueEUR) %>%
  
  group_by(Club) %>%
  summarize(`Total Player Value (millions)` = sum(ValueEUR, na.rm = TRUE))%>%
  
  #Show number in number of millions for clarity
  dplyr::mutate(`Total Player Value (millions)` = 
                  round(`Total Player Value (millions)`/ 1000000, 1))

## Standings Data

#import data
url <- "https://www.espn.com/soccer/standings/_/league/ESP.1/season/2022"
standings_2023 <- read_html(url) %>%
  html_table() %>%
  as.data.frame()


#isolate each team's final position in the league
standings_2023 <-
  standings_2023 %>%
  dplyr::select(1) %>%
  rename(Club = 1) %>%
  
  mutate(Club = la_liga)%>%
  
  mutate(Rank = seq(1, nrow(standings_2023)))

## Club value data

#LaLiga has promotion and relegation and this
#contains the 3 teams that were promoted at the end of the year in the top division dataset
#and the 3 teams that were demoted in the second division dataset
#we have to join the top division table with the second division to get all the teams
#that played in the 2022-2023 season

#load data for top division
url2 <- "https://www.transfermarkt.com/laliga/marktwerteverein/wettbewerb/ES1/plus/?stichtag=2023-06-15"
page <- read_html(url2) %>%
  html_table() 

#Isolate clubs and values for top division
club_value_l1 <- 
  #select market value table with columns for club and their value
  page[[2]][-1, c(3,5)] %>%
  
  #parse number
  dplyr::mutate(across(2, parse_number)) %>%

  dplyr::rename(`Club Value (millions)` = League)

# load data for second division
url3 <- "https://www.transfermarkt.com/laliga2/marktwerteverein/wettbewerb/ES2/plus/?stichtag=2023-07-15"
page <- read_html(url3) %>%
  html_table() 

#Isolate clubs and value from second division
club_value_l2 <- 
  #select market value table with columns for club and their value
  page[[2]][-1, c(3,5)] %>%
  
  #parse number
  dplyr::mutate(across(2, parse_number)) %>%
  
  dplyr::rename(`Club Value (millions)` = League)

#combine tables to join with standings
club_values <- 
  bind_rows(club_value_l1, club_value_l2)


#Clean data so all team names are equal for joining

standings_names <- standings_2023[[1]]
player_val_names <- player_value[[1]]
club_values_names <- club_values[[1]]

#remove all prefixes and other strings that are unique to each dataset
prefix <- c("Unión Deportiva", "FC", "CF", "RCD", "RC", "de", "Club", "CA", " ", "UD", "CD", "SD")

for (term in prefix){
  standings_names <- str_replace_all(standings_names, term, "")
  player_val_names <- str_replace_all(player_val_names, term, "")
  club_values_names <- str_replace_all(club_values_names, term, "")
}

#replace dataset club names with univiersal cclub names
standings_2023 <- 
  standings_2023 %>%
  dplyr::mutate(Club = standings_names)

player_value <-
  player_value %>%
  dplyr::mutate(Club = player_val_names)

club_values <-
  club_values %>%
  dplyr::mutate(Club = club_values_names)


#join tables standings, club value, palyer value

value_insight <-
  standings_2023 %>%
  left_join(club_values, by = "Club") %>%
  left_join(player_value, by = "Club" ) %>%
  
  dplyr::mutate(Club = la_liga)

##Plot

ggplot(value_insight, aes(y= Rank,
                          x = `Total Player Value (millions)`,
                          size = `Club Value (millions)`)) +  #different club value
                                                              #has different size
  geom_point() +
  
  labs(title = "La Liga 22-23: Total Player Values vs Standings") +
  
  scale_x_continuous(n.breaks = 5)+
  
  scale_y_reverse(n.breaks = 20, minor_breaks = NULL)+  #make rank be top to bottom
  
  
  theme_minimal()
