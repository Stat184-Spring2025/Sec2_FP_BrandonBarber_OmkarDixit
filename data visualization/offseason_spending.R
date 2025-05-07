### OFF SEASON SPENDING 

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

#standard club names for the 2022 season

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

#2022-2023 standings

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


#Joint standings

#due to promotion and relegation, the same teams are not in
#the top division of Spanish soccer every year. We want to only see
#the teams that were in LaLiga for both years

joint_standings <- 
  standings_2023 %>%
  inner_join(standings_2022, by = "Club") 
  

##OFFSEASON SPENDING

#import data
url3 <- "https://www.transfermarkt.com/laliga/einnahmenausgaben/wettbewerb/ES1/ids/a/sa//saison_id/2021/saison_id_bis/2022/nat/0/pos//w_s//intern/0"
page1 <- read_html(url3) %>%
  html_table() 
liga1_expenditures<- page1[[2]]

#clean column names (n/a columns were included)
variables <- unique(names(liga1_expenditures))
liga1_expenditures <- liga1_expenditures[-c(1,2,9)]
names(liga1_expenditures) <- variables[-1]

#select club and offseason expenditures
liga1_expenditures <- 
  liga1_expenditures %>%
  dplyr::select(1,2)


## Expenditures Comparison

#Clean data so expenditures club names are the same as standings

standings_names <- joint_standings[[1]]
expenditures_names <- liga1_expenditures[[1]]
final_names <- joint_standings[[1]]  #copy of our output club names

#remove all prefixes and other strings that are unique to each dataset
prefix <- c("FC", "CF", "RCD", "RC", "de", "Club", "CA", " ", "LA")

for (term in prefix){
  standings_names <- str_replace_all(standings_names, term, "")
  expenditures_names <- str_replace_all(expenditures_names, term, "")
}

#replace dataset club names with univiersal names
joint_standings <- 
  joint_standings %>%
  dplyr::mutate(Club = standings_names)

liga1_expenditures <-
  liga1_expenditures %>%
  dplyr::mutate(Club = expenditures_names)

#join standings with expenditures, parsing for numbers
expenditure_rank <-
  joint_standings %>%
  left_join(liga1_expenditures, by = 'Club') %>%
  dplyr::mutate(Club = final_names) %>%
  
  dplyr::mutate(Expenditure = str_replace_all(Expenditure, '-', '0')) %>%
  rename(`Expenditure (millions)` = Expenditure) %>%
  dplyr::mutate(across(`Expenditure (millions)`, parse_number))
  
#split table into teams that stayed the same vs changed
#teams that stayed the same will use a point and not an arrow

rank_up <-
  expenditure_rank %>%
  dplyr::filter(`Rank 2023` < `Rank 2022`)

rank_down <-
  expenditure_rank %>%
  dplyr::filter(`Rank 2023` > `Rank 2022`)

rank_same <-
  expenditure_rank %>%
  dplyr::filter(`Rank 2022` == `Rank 2023`)


#Plot
ggplot()+
  
  geom_segment(data = rank_up, aes(
    x = `Expenditure (millions)`,
    xend = `Expenditure (millions)`,
    y = `Rank 2022`,
    yend = `Rank 2023`),
    
    arrow = arrow(length = unit(0.17, "cm")),
    color = "forestgreen"
  ) +
  
  geom_point(data = rank_up, aes(
    x = `Expenditure (millions)`,
    y = `Rank 2022`),
    color = "forestgreen",
    size = 0.8
  ) +
  
  geom_segment(data = rank_down, aes(
    x = `Expenditure (millions)`,
    xend = `Expenditure (millions)`,
    y = `Rank 2022`,
    yend = `Rank 2023`),
    
    arrow = arrow(length = unit(0.17, "cm")),
    color = "red"
  ) +
  
  
  geom_point(data = rank_down, aes(
    x = `Expenditure (millions)`,
    y = `Rank 2022`),
    color = "red",
    size = 0.8
  ) +
  
  geom_point(data = rank_same, aes(
    x = `Expenditure (millions)`,
    y = `Rank 2023`)) +
  
  labs(title = "Impact of Offseason Spending on Performance",
       x = "Offseason Spending (Millions of Euros)",
       y = "Rank") +
  
  scale_y_reverse(breaks = 1:20, minor_breaks= NULL)+  #make rank top to bottom
  
  theme_minimal()
