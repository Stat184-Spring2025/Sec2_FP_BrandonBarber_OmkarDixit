##FIFA OVERALL VISUALIZATION

library(dplyr)
library(tidyr)
library(readr)
library(rvest)

##Team Overall

#Load Data
fifa_rating <- read_csv("players_fifa23.csv")

#Filter for LaLiga PLayers
la_liga<- c(
  "FC Barcelona", "Real Madrid CF", "Atlético de Madrid", "Real Sociedad",
  "Villarreal CF", "Real Betis Balompié", "CA Osasuna", "Athletic Club de Bilbao",
  "RCD Mallorca", "Girona FC", "Rayo Vallecano", "Sevilla FC", "RC Celta de Vigo",
  "Cádiz CF", "Getafe CF", "Valencia CF", "Unión Deportiva Almería", "Real Valladolid CF",
  "RCD Espanyol de Barcelona", "Elche CF"
)

fifa_rating <-
  fifa_rating %>%
  dplyr::filter(Club %in% la_liga)%>%  
  dplyr::filter(!is.na(Overall))
  
#Calculate average player rating per club
club_avg_rating <- 
  fifa_rating %>%
  group_by(Club) %>%
  summarize(Average_Overall = round(mean(Overall, na.rm = TRUE), 2)) %>%
  arrange(desc(Average_Overall))


#2022-2023 standings

#load data
url <- "https://www.espn.com/soccer/standings/_/league/ESP.1/season/2022"
standings_2023 <- read_html(url) %>%
  html_table() %>%
  as.data.frame()


standings_2023 <-
  standings_2023 %>%
  dplyr::select(1) %>%
  rename(Club = 1) %>%
  
  #add universal club names from datasets
  mutate(Club = la_liga) %>%
  
  #add season ending rank
  mutate(`Rank 2023` = seq(1, nrow(standings_2023)))

## Join overall with standings

rating_standings <-
  standings_2023 %>%
  left_join(club_avg_rating, by = "Club")

#Plot bar chart

ggplot(rating_standings, aes(x = Average_Overall, y = reorder(Club, -`Rank 2023`))) +
  
  geom_point(size = 4, color = "steelblue") +
  
  xlim(70,80)+
  
  labs(title = "Average Player Rating in La Liga 22/23", x = "Average Overall", y = "Club") +
  
  geom_text(aes(label = Average_Overall), hjust = -0.4, size = 3) +
  
  theme_minimal()

