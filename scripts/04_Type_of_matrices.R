library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(tidyr)
library(igraph)
library(tidygraph)
library(ggraph)


##### get networks #####

trophic_data <- read_delim("data/raw/Consumption_event_corrected.csv", delim = ";")
metadata <- read_delim("data/raw/Player_Data_corrected.csv", delim = ";") 

# remove row names
trophic_data <- trophic_data %>% select(-"...1")

# check data
head(trophic_data)

# Make subset for different interaction types
types <- as.factor(unique(game$type_interaction))

subset_dataframe <- function(df){
  
  game <- df %>% 
    filter(!is.na(ressource_id), game_id == gameid)  %>% 
    mutate(duration = time - min(time)) %>%
    arrange(duration) %>% 
    mutate(ti = grepl("[0-9]", ressource_id), # Using regex, sort the type of interactions
           type_interaction = case_when(ti == "TRUE" ~ "foraging", # If resource_id = digit --> foraging activity
                                        ti == "FALSE" ~ "predation")) %>% 
    select(-ti) %>% 
    mutate(score = case_when(ressource_type == "Type A" ~ 5,
                             ressource_type == "Type B" ~ 1))
  
}


try <-  subset_dataframe(trophic_data)


nested_df <- game %>% 
  group_by(type_interaction, game_id) %>% 
  nest() %>% 
  mutate(data1 = map(data, ~make_networks(1)))

nested_df$data1


adding_interaction <- function(df){
for (a in 1:nrow(df)) {
  
  i <- which(id == as.character(df[a, "player_id"]))
  j <- which(id == tolower(as.character(df[a, "ressource_id"])))
  
  n1[i,j] <- 1
  
  Ns[,,a] <- n1
  
}
  return(Ns)
  }


q <- adding_interaction(game)


game <- trophic_data %>% 
  filter(!is.na(ressource_id), game_id == gameid)  %>% 
  mutate(duration = time - min(time)) %>%
  arrange(duration) %>% 
  mutate(ti = grepl("[0-9]", ressource_id), # Using regex, sort the type of interactions
         type_interaction = case_when(ti == "TRUE" ~ "foraging", # If resource_id = digit --> foraging activity
                                      ti == "FALSE" ~ "predation")) %>% 
  select(-ti) %>% 
  mutate(score = case_when(ressource_type == "Type A" ~ 5,
                           ressource_type == "Type B" ~ 1))


Ns <- adding_interaction(game)

gameid = 1

# filter games and get temporal networks
make_networks <- function(gameid) {
  
  # select game and compute duration
  game <- nested_df %>% 
    unnest() %>% 
    filter(!is.na(ressource_id), game_id == gameid)  %>% 
    mutate(duration = time - min(time)) %>%
    arrange(duration) %>% 
    mutate(ti = grepl("[0-9]", ressource_id), # Using regex, sort the type of interactions
           type_interaction = case_when(ti == "TRUE" ~ "foraging", # If resource_id = digit --> foraging activity
                                        ti == "FALSE" ~ "predation")) %>% 
    select(-ti) %>% 
    mutate(score = case_when(ressource_type == "Type A" ~ 5,
                             ressource_type == "Type B" ~ 1))
  
  game_foraging <- game %>% 
    filter(type_interaction == "foraging")
  
  game_predation <- game %>% 
    filter(type_interaction == "predation")
  
  
  # get consumer names
  players_game <- game %>% 
    group_by(player_id) %>% 
    summarize() %>% 
    rename(id = player_id)
  
  # get resource name
  patches_game <- game %>% 
    group_by(ressource_id) %>% 
    summarize() %>% 
    rename(id = ressource_id) %>% 
    mutate(id = tolower(id))
  
  # get individuals name
  id <- unique(rbind(players_game, patches_game)) %>% 
    arrange(id) %>%
    pull()
  
  # get number of individuals
  n <- length(id)
  
  # make initial network
  Ns <- array(0, dim = c(n, n, nrow(game)))
  
  # each matrix corresponds to a different time point
  n1 <- matrix(0, nrow = n, ncol = n)
  
  # add interactions one at a time
  for (a in 1:nrow(game)) {

    i <- which(id == as.character(game[a, "player_id"]))
    j <- which(id == tolower(as.character(game[a, "ressource_id"])))

    n1[i,j] <- 1

    Ns[,,a] <- n1

  }
  
  # Ns <- adding_interaction(game)
  
  # find individual roles
  metadata_game <- metadata %>% filter(game_id == gameid)
  roles <- id
  
  for (i in 1:length(id)) {
    if (id[i] %in% metadata_game$player_id) {
      roles[i] <- metadata_game$role[metadata_game$player_id == id[i]]
    } else {
      roles[i] <- "ressource"
    }
  }
  return(list(Ns = Ns, duration = game$duration, id = id, roles = roles))
}


make_networks(1)
