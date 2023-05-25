library(readr)
library(tidyverse)
data_network <- read_csv("data/raw/Cleaned_Ressources_Anonymous.csv")
# data_network <- read.table("data/raw/Cleaned_Ressources_Anonymous.csv", sep = ",",
# header = TRUE, row.names = 1)
View(data_network)


test <- data_network %>% 
  rename("test" = "...1")


test <- test %>% 
  filter(!is.na(ressource_id)) 

# data_network[,1]

qweiasd <- test %>% 
  group_by(player_name) %>% 
  nest()

q <- c(1,2,3,4,5)
a <- c(1,2,3,4,5)

num <- as.character(seq(0,9,1))

test %>% 
  mutate(type_interact = starts_with(num))
View()


subset(test, startsWith(as.character(ressource_id), "1"))


test[grepl("[0-9]" test$ressource_id, perl = T),]


mutate(type_interact = )


test[grepl([:digit:], )]


test1 <- test %>%
  mutate(ti = grepl("[0-9]", ressource_id),
         type_interaction = case_when(ti == "TRUE" ~ "foraging",
                                      ti == "FALSE" ~ "predation")) %>% 
  select(-ti)

# %>% 
#   group_by(type_interaction) %>% 
#   count()
         