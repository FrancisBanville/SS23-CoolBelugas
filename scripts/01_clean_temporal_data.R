library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(igraph)


##### get networks #####

trophic_data <- read_csv("data/raw/Cleaned_Ressources_Anonymous.csv") 

# remove row names
trophic_data <- trophic_data %>% select(-"...1")

# check data
head(trophic_data)


# gameid = 1 

# filter games and get temporal networks
make_networks <- function(gameid) {
  
  # select game and compute duration
  game <- trophic_data %>% 
    filter(!is.na(ressource_id), Game_ID == gameid)  %>% 
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
    group_by(player_name) %>% 
    summarize() %>% 
    rename(id = player_name)
  
  # get resource name
  patches_game <- game %>% 
    group_by(ressource_id) %>% 
    summarize() %>% 
    rename(id = ressource_id) 
  
  # get individuals name
  id <- unique(rbind(players_game, patches_game)) %>% 
    arrange(id)
  
  # get number of individuals
  n <- nrow(id)
  
  # make initial network
  Ns <- array(0, dim = c(nrow(id), nrow(id), nrow(game)))
  
  # each matrix corresponds to a different time point
  n1 <- matrix(0, nrow = n, ncol = n)
  Ns[,,1] = n1
  
  # add interactions one at a time
  # for (a in 1:nrow(game)) {
  # 
  #   i <- which(id == as.character(game[a, "player_name"]))
  #   j <- which(id == as.character(game[a, "ressource_id"]))
  # 
  #   n1[i,j] <- 1
  # 
  #   Ns[,,a] <- n1
  # }
  
  Adding_interaction(game)
  
  return(list(Ns = Ns, duration = game$duration, type_interaction = game$type_interaction))
}


# Ns_foraging <- Adding_interaction(game_foraging)
# Ns_predation <- Adding_interaction(game_predation)
# Ns_full <- Adding_interaction(game)

return(list(Ns_full = Ns_full, duration = game$duration, type_interaction = game$type_interaction,
            Ns_foraging = Ns_foraging,
            Ns_predation = Ns_predation))

}



x <- make_networks(1)  

# add interactions one at a time 
q <- Adding_interaction(game)

q

# for (a in 1:nrow(game)) {
# 
#   i <- which(id == as.character(game[a, "player_name"]))
#   j <- which(id == as.character(game[a, "ressource_id"]))
#   
#   n1[i,j] <- 1
#   
#   Ns[,,a] <- n1
# }
# 
# return(list(Ns = Ns, duration = game$duration, type_interaction = game$type_interaction))
# }

# Test %>% %>% 
rty <- make_networks(1)


sum %>% rty$Ns_full

# ----------------
# makes a function that build the 3 matrices 
# All interactions - Foraging - Predation

Adding_interaction <- function(df){
  
  for (a in 1:nrow(df)) {
    
    i <- which(id == as.character(df[a, "player_name"]))
    j <- which(id == as.character(df[a, "ressource_id"]))
    
    n1[i,j] <- 1
    
    Ns[,,a] <- n1
  }}


matrix_foraging <- Adding_interaction(game_foraging)
matrix_predation <- Adding_interaction(game_predation)
matrix_full <- Adding_interaction(game)



# filter games and get temporal networks
# 
# make_networks_test <- function(gameid) {
#   
#   # select game and compute duration
#   game <- trophic_data %>% 
#     filter(!is.na(ressource_id), Game_ID == gameid)  %>% 
#     mutate(duration = time - min(time)) %>%
#     arrange(duration) %>% 
#     mutate(ti = grepl("[0-9]", ressource_id), # Using regex, sort the type of interactions
#            type_interaction = case_when(ti == "TRUE" ~ "foraging", # If resource_id = digit --> foraging activity
#                                         ti == "FALSE" ~ "predation")) %>% 
#     select(-ti) %>% 
#     mutate(score = case_when(ressource_type == "Type A" ~ 5,
#                              ressource_type == "Type B" ~ 1))
#   
#   game_foraging <- game %>% 
#     filter(type_interaction == "foraging")
#   
#   game_predation <- game %>% 
#     filter(type_interaction == "predation")
#   
#   
#   # get consumer names
#   players_game <- game %>% 
#     group_by(player_name) %>% 
#     summarize() %>% 
#     rename(id = player_name)
#   
#   # get resource name
#   patches_game <- game %>% 
#     group_by(ressource_id) %>% 
#     summarize() %>% 
#     rename(id = ressource_id) 
#   
#   # get individuals name
#   id <- unique(rbind(players_game, patches_game)) %>% 
#     arrange(id)
#   
#   # get number of individuals
#   n <- nrow(id)
#   
#   # make initial network
#   Ns <- array(0, dim = c(nrow(id), nrow(id), nrow(game)))
#   
#   # each matrix corresponds to a different time point
#   n1 <- matrix(0, nrow = n, ncol = n)
#   Ns[,,1] = n1
#   
#   # add interactions one at a time 
#   
#   res <- test_function(game)
#   
#   # for (a in 1:nrow(game)) {
#   #   
#   #   i <- which(id == as.character(game[a, "player_name"]))
#   #   j <- which(id == as.character(game[a, "ressource_id"]))
#   #   
#   #   n1[i,j] <- 1
#   #   
#   #   Ns[,,a] <- n1
#   # }
#   
#   return(list(Ns = res$Ns, duration = game$duration, type_interaction = game$type_interaction))
# }
# 
# 
# make_networks_test(1)
# 
# q <- test_function(game)
# 
# 
# 
# test_function <- function(df) {
#   
#   for (a in 1:nrow(df)) {
#     
#     i <- which(id == as.character(df[a, "player_name"]))
#     j <- which(id == as.character(df[a, "ressource_id"]))
#     
#     n1[i,j] <- 1
#     
#     Ns[,,a] <- n1
#   }
#   
#   return(list(Ns = Ns))
#   
# }
# 
# game <- trophic_data %>% 
#   filter(!is.na(ressource_id))  %>% 
#   mutate(duration = time - min(time)) %>%
#   arrange(duration) %>% 
#   mutate(ti = grepl("[0-9]", ressource_id), # Using regex, sort the type of interactions
#          type_interaction = case_when(ti == "TRUE" ~ "foraging", # If resource_id = digit --> foraging activity
#                                       ti == "FALSE" ~ "predation")) %>% 
#   select(-ti) %>% 
#   mutate(score = case_when(ressource_type == "Type A" ~ 5,
#                            ressource_type == "Type B" ~ 1))
# 
# 
# res <- test_function(game)
# 
# # r <- apply(X = game, FUN = test_function(.))

##### compute measures #####

# compute number of interactions (herbivory and predatory)
count_links <- function(game) {
  
  links <- rep(0, length(game$duration))
  
  for (a in 1:length(game$duration)) {
    links[a] <- sum(game$Ns[,,a])
  }
  return(links)
}


# compute modularity 
measure_modularity <- function(game) {
  
  mod <- rep(0, length(game$duration))
  
  for (a in 1:length(game$duration)) {
    # make graph (and remove nodes without interactions)
    N <- graph_from_adjacency_matrix(game$Ns[,,a])
    N <- delete.vertices(simplify(N), degree(N)==0)
    # get clusters that maximize modularity and compute modularity
    communities <- cluster_walktrap(N)
    mod[a] <- modularity(N, membership = communities$membership)
  }
  return(mod)
}

df <- data.frame()

for (i in 1:5) {
  
  # generate networks for game i and get durations at which observations were observed
  game <- make_networks(i)
  duration <- game$duration
  
  # make graphs
  
  # compute number of links through time
  links <- count_links(game)
  
  # compute modularity through time
  mod <- measure_modularity(game)
  
  # add rows to dataframe
  df <- rbind(df, data.frame(duration = duration, 
                             links = links, 
                             mod = mod,
                             game = rep(i, length(duration))))
}

df <- df %>% 
  mutate(duration = as.numeric(duration), game = as.factor(game))

##### visualize data ##### 

# number of interactions through time
ggplot(df, aes(x = duration, y = links, col = game)) +
  geom_line(lw = 1) +
  geom_point() + 
  xlab("duration (s)") +
  ylab("number of interactions")

# modularity through time (start after 1 min)
ggplot(df %>% filter(duration > 60), aes(x = duration, y = mod, col = game)) +
  geom_point() + 
  geom_smooth() +
  xlab("duration (s)") +
  ylab("modularity")

# modularity vs number of interactions (start after 1 min)
ggplot(df %>% filter(duration > 60), aes(x = links, y = mod, col = game)) +
  geom_point() + 
  geom_smooth() +
  xlab("number of interactions") +
  ylab("modularity")



## igraph

net_148 <- graph_from_adjacency_matrix(game$Ns[,,148])
plot(net_148)


q <- make_networks(1)

