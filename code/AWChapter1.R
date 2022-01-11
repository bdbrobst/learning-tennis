#Brandon Brobst
#8 January 2022
#Built with R version 4.1.2
#This file follows along with AW Chapter 1 material.

#Install and load necessary packages
install.packages("tidyverse")
install.packages("gt")

library(tidyverse)
library(gt)

#The first hypothesis that Klaassen & Magnus explore is that the the person
  #serving first in a set is at an advantage.

#Load in 2021 Wimbledon point-by-point data
wimbledon_2021_pbp <- read.csv(url("https://raw.githubusercontent.com/JeffSackmann/tennis_slam_pointbypoint/master/2021-wimbledon-points.csv"))

#To determine who served first in each set, create a df
  #using the first point of each set's ServiceIndicator attribute
first_server <- wimbledon_2021_pbp |> 
  dplyr::filter(P1GamesWon == 0 & P2GamesWon ==0 &
                  ((P1Score == 15 & P2Score == 0) |
                  (P1Score == 0 & P2Score == 15))) |> 
  dplyr::select(match_id, SetNo, ServeIndicator)


#Filter for rows that show who won the set
set_winners <- wimbledon_2021_pbp |>
  dplyr::filter(SetWinner != 0 & ElapsedTime != '0') |> 
  dplyr::select(match_id, SetNo, SetWinner)

#Join first server and set winner dfs into a new df,
  #then mutate a binary first_server_won_set variable that shows 
  #whether or not the set's first server won that set.
did_set_first_server_win <- first_server |> 
  dplyr::left_join(set_winners, by = c("match_id", "SetNo")) |> 
  mutate(first_server_winner = 
           ifelse(ServeIndicator == SetWinner, 1, 0))

#The dataset is missing set winner information for 9 sets.
  #Filter those, out then create a table vis exploring the hypothesis.
did_set_first_server_win |> 
  dplyr::filter(!is.na(SetWinner)) |> 
  summarise(num_sets = n(),
            first_serve_set_winrate = mean(first_server_winner)) |> 
  gt() |> 
  tab_header(
    title = "Does serving first in a set provide an advantage?",
    subtitle = "Wimbledon 2021"
  ) |> 
  cols_label(num_sets = "# of Sets",
             first_serve_set_winrate = "First Server Set Winrate") |> 
  fmt_percent(columns = first_serve_set_winrate)

#From Wimbledon 2021 data, across 753 sets only 47% of sets were won by
  #the set's first server
#This hypothesis is one of many explored further in later chapters.
