
library(readr)
library(rvest)

library(dplyr)
library(tidyr)
library(stringr)

library(ggplot2)
library(scales)

##STANDINGS TABLES

#2021-2022 standings

#load data
url1 <- "https://www.espn.com/soccer/standings/_/league/ESP.1/season/2021"
standings_2022 <- read_html(url1) %>%
  html_table() %>%
  as.data.frame()

la_liga_2022 <- c("Real Madrid CF", "FC Barcelona", "Atlético de Madrid", "Sevilla FC",
                  "Real Betis Balompié", "Real Sociedad", "Villarreal CF", "Athletic Club de Bilbao",
                  "Valencia CF", "CA Osasuna", "RC Celta de Vigo", "Rayo Vallecano",
                  "Elche CF", "RCD Espanyol de Barcelona", "Getafe CF", "RCD Mallorca",
                  "Cádiz CF", "Granada CF", "Levante UD", "Deportivo Alavés"
)

standings_2022 <-
  standings_2022 %>%
  dplyr::select(1) %>%
  rename(Club = 1) %>%
  
  #add universal club names from datasets
  mutate(Club = la_liga_2022) %>%
  
  #add season ending rank
  mutate(`Rank 2022` = seq(1, nrow(standings_2022)))

##2022-2023 standings

#load data
url2 <- "https://www.espn.com/soccer/standings/_/league/ESP.1/season/2022"
standings_2023 <- read_html(url2) %>%
  html_table() %>%
  as.data.frame()

la_liga_2023 <- c(
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
  
  #add universal club names from datasets
  mutate(Club = la_liga_2023) %>%
  
  #add season ending rank
  mutate(`Rank 2023` = seq(1, nrow(standings_2023)))


##Joint standings

#due to promotion and relegation, the same teams are not in
#the top division of Spanish soccer every year. We want to only see
#the teams that were in LaLiga for both years

joint_standings <- 
  standings_2023 %>%
  inner_join(standings_2022, by = "Club") 


##Academy Transfer Data

#load data
academy_callups_raw <- read_csv("La_Liga_Academy_Promotion_2022_23.csv")

#group by club and count how many players called up
academy_callups <-
  academy_callups_raw %>%
  group_by(Club) %>%
  summarise(Count = n())

#Clean data so expenditures club names are the same as standings

standings_names <- joint_standings[[1]]
callups_names <- academy_callups[[1]]
final_names <- joint_standings[[1]]  #copy of our output club names

#remove all prefixes and other strings that are unique to each dataset
prefix <- c("FC", "CF", "RCD", "RC", "de", "Club", "CA", " ", "LA")

for (term in prefix){
  standings_names <- str_replace_all(standings_names, term, "")
  callups_names <- str_replace_all(callups_names, term, "")
}

#replace dataset club names with univiersal names
joint_standings <- 
  joint_standings %>%
  dplyr::mutate(Club = standings_names)

academy_callups <-
  academy_callups %>%
  dplyr::mutate(Club = callups_names)

#join standings with number of callups
callups_rank <-
  joint_standings %>%
  left_join(academy_callups, by = 'Club') %>%
  dplyr::mutate(Club = final_names)

#split table into teams that stayed the same vs changed
#teams that stayed the same will use a point and not an arrow

rank_up <-
  callups_rank %>%
  dplyr::filter(`Rank 2023` < `Rank 2022`)

rank_down <-
  callups_rank %>%
  dplyr::filter(`Rank 2023` > `Rank 2022`)

rank_same <-
  callups_rank %>%
  dplyr::filter(`Rank 2022` == `Rank 2023`)


#Plot
ggplot()+
  
  geom_segment(data = rank_up, aes(
    x = Count,
    xend = Count + 0.25, #add angle to distinguish between teams with the same number
    y = `Rank 2022`,
    yend = `Rank 2023`),
    
    arrow = arrow(length = unit(0.25, "cm")),
    color = "forestgreen"
  ) +
  
  geom_point(data = rank_up, aes(
    x = Count,
    y = `Rank 2022`),
    color = "forestgreen",
    size = 0.95
  ) +
  
  geom_segment(data = rank_down, aes(
    x = Count,
    xend = Count - 0.25, #add angle to distinguish between teams with the same number
    y = `Rank 2022`,
    yend = `Rank 2023`),
    
    arrow = arrow(length = unit(0.25, "cm")),
    color = "red"
  ) +
  
  
  geom_point(data = rank_down, aes(
    x = Count,
    y = `Rank 2022`),
    color = "red",
    size = 0.95
  ) +
  
  geom_point(data = rank_same, aes(
    x = Count,
    y = `Rank 2023`)) +
  
  labs(title = "Impact of Academy Promotions on Performance",
       x = "Number of Academy Signings",
       y = "Rank") +
  
  scale_y_reverse(breaks = 1:20, minor_breaks= NULL)+
  
  scale_x_continuous(n.breaks = 10) +
  
  theme_minimal()
