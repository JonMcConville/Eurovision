## Loading Libraries ----

library(dplyr)
library(tidyverse)
library(stringdist)
library(stringr)
library(reshape2)
library(tidyr)
library(wesanderson)
library(pivottabler)

## Reading in Data ----

contestants <- read.csv("contestants.csv")
votes <-read.csv("votes.csv")


Voting_numbers <- votes %>%
  filter(to_country_id == "gb") %>%
  select(year, round, total_points) %>% 
  group_by(year, round) %>%
  arrange(year) %>%
  summarise("Points" = sum(total_points, rm.na = FALSE),
            count = n())


Winner <-votes%>%
  filter(round == "final")%>%
  select(year,to_country,total_points)%>%
  group_by(year, to_country)%>%
  summarise("Total_Points" = sum(total_points, rm.na = FALSE))%>%
  ungroup()%>%
  arrange(desc(Total_Points))%>%
  distinct(year, .keep_all= TRUE)%>%
  arrange(year)





Test_Winner<- contestants%>%
  filter(place_contest ==1)%>%
  select(year, to_country_id)
