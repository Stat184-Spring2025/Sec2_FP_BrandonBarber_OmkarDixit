library(knitr)
library(kableExtra)
library(readr)

#load file
STANDINGS <- read_csv("laliga_standings.csv")

#Create Table
TABLE <- kable(STANDINGS, caption = "Final LaLiga Standings for 2022-23") %>%
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