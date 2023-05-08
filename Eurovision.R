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
  arrange(desc(Total_Points))%>%
  slice_max(Total_Points)%>%
  arrange(year)


votes%>%
  filter(year==1991)%>%
  select(total_points)%>%
  group_by(total_points)%>%
  distinct(total_points)%>%
  arrange(desc(total_points))


Winner <-votes%>%
  filter(round == "final")%>%
  filter(year == 1991)%>%
  select(to_country,total_points)%>%
  group_by(to_country)%>%
  summarise("12points" =count(total_points))%>%
  arrange(desc(Total_Points))%>%
  slice_max(Total_Points)
  

votes%>%
  select(year, to_country, total_points)%>%
  filter(year==1991)%>%
  filter(to_country == "fr" | to_country == "se")%>%
  group_by(year, to_country, total_points)%>%
  summarise(count=n())%>%
  arrange(desc(total_points))



  



Test_Winner<- contestants%>%
  filter(place_contest ==1)%>%
  select(year, to_country_id)



Winner <-votes%>%
  filter(round == "final")%>%
  select(year,to_country,total_points)%>%
  group_by(year, to_country)%>%
  summarise("Total_Points" = sum(total_points, rm.na = FALSE))%>%
  summarise("Winning_Score" = max(Total_Points, rm.na = FALSE)) %>%
  ungroup()%>%
  select(year,country_to, Winning_Score)%>%
  distinct(year, .keep_all= TRUE)%>%
  arrange(year)

# Question 2----

ggplot(votes %>% 
         filter(year==2018) %>% 
         filter(round=="final")%>%
         filter(tele_points != 0 & jury_points != 0)%>%
         select(tele_points,jury_points)%>%
         group_by(tele_points,jury_points)%>%
         summarise(count=n()), 
       aes(x=tele_points, y=jury_points, size=count)) +
  geom_point() +
  geom_smooth()


# Question 2a----

VotingPattern <-votes%>%
  filter(from_country_id != to_country_id)%>%
  select(from_country_id,to_country_id, total_points)%>%
  group_by(from_country_id, to_country_id)%>%
  summarise("Points"=sum(total_points))%>%
  arrange(from_country_id, to_country_id)

VotingPattern %>%
pivot_wider(names_from = to_country_id, values_from = Points)


  
