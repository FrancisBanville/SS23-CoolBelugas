library(readr)
library(tidyverse)
data_network <- read_csv("data/raw/Cleaned_Ressources_Anonymous.csv")
data_network <- read.table("data/raw/Cleaned_Ressources_Anonymous.csv", sep = ",",
                           header = TRUE, row.names = 1)
View(data_network)


data_network %>% 
  rename("test" = "...1")


# data_network[,1]
