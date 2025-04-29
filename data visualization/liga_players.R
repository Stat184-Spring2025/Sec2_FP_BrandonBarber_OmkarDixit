##FIFA OVRALL VISUALIZATION

library(dplyr)
library(tidyr)
library(readr)

#Load Data
FIFA_RATING <- read_csv("players_fifa23.csv")

#Filter for LaLiga PLayers
la_liga<- c(
  "FC Barcelona", "Real Madrid CF", "Atlético de Madrid", "Real Sociedad",
  "Villarreal CF", "Real Betis Balompié", "CA Osasuna", "Athletic Club de Bilbao",
  "RCD Mallorca", "Girona FC", "Rayo Vallecano", "Sevilla FC", "RC Celta de Vigo",
  "Cádiz CF", "Getafe CF", "Valencia CF", "Unión Deportiva Almería", "Real Valladolid CF",
  "RCD Espanyol de Barcelona", "Elche CF"
)

FIFA_RATING <-
  FIFA %>%
  dplyr::filter(Club %in% la_liga)%>%  
  dplyr::filter(!is.na(Overall)) %>%  
  
  dplyr::select(c("Overall", "Club"))
  
#Calculate average player rating per club
club_avg_rating <- 
  FIFA_RATING %>%
  group_by(Club) %>%
  summarize(Average_Overall = round(mean(Overall, na.rm = TRUE), 2)) %>%
  arrange(desc(Average_Overall))

#Plot bar chart
ggplot(club_avg_rating, aes(x = reorder(Club , -Average_Overall), y = Average_Overall)) +
  geom_bar(stat = "identity", fill = "cyan") +
  labs(title = "Average Player Rating in La Liga",
       x = "Club",
       y = "Average Overall") +
  
  geom_text(aes(label = Average_Overall), vjust = 1.2, size = 1.5) +
  
theme_minimal() +
  theme(axis.text.x = element_text(angle = 55, hjust = 1))
