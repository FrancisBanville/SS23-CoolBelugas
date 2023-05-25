library(readr)
library(tidyverse)

# Import the dataset
data <- read_csv("data/raw/Cleaned_Ressources_Anonymous.csv")


# Tidy the df so that interaction types are mentionned
data_predation <- data %>% 
  select(-"...1") %>%   # remove 1st column (trash)
  filter(!is.na(ressource_id)) %>%  # remove rows when no patch_ID was entered
  mutate(ti = grepl("[0-9]", ressource_id), # Using regex, sort the type of interactions
         type_interaction = case_when(ti == "TRUE" ~ "foraging", # If resource_id = digit --> foraging activity
                                      ti == "FALSE" ~ "predation")) %>% 
  select(-ti)



scores_per_game_prey <- data_predation %>% 
  mutate(score = case_when(ressource_type == "Type A" ~ 5,
                           ressource_type == "Type B" ~ 1)) %>% 
  group_by(Game_ID, player_name) %>% 
  mutate(score_tot = sum(score))
    

# Plot the overall scores of individuals
scores_per_game_prey %>% 
  ggplot()+
  geom_point(aes())
