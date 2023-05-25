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


# gameid = 1 

# filter games and get temporal networks
make_networks <- function(gameid, type = "binary") {
  
  # select game and compute duration
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
  if (type == "binary") {
    for (a in 1:nrow(game)) {
      
      i <- which(id == as.character(game[a, "player_id"]))
      j <- which(id == tolower(as.character(game[a, "ressource_id"])))
      
      n1[i,j] <- 1
      
      Ns[,,a] <- n1
    }
  }

  # count frequency of interactions
  if (type == "quantitative") {
    for (a in 1:nrow(game)) {
      
      i <- which(id == as.character(game[a, "player_id"]))
      j <- which(id == tolower(as.character(game[a, "ressource_id"])))
      
      n1[i,j] <- n1[i,j] + 1
      
      Ns[,,a] <- n1
    }
}
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
(g1 <- ggplot(df, aes(x = duration, y = links, col = game)) +
  geom_line(lw = 1) +
  geom_point() + 
  xlab("duration (s)") +
  ylab("number of interactions") +
  theme_classic())

ggsave("figures/links_time.png")

# modularity through time (start after 1 min)
(g2 <- ggplot(df %>% filter(duration > 60), aes(x = duration, y = mod, col = game)) +
  geom_point() + 
  geom_smooth() +
  xlab("duration (s)") +
  ylab("modularity") +
  theme_classic())

ggsave("figures/mod_time.png")

# modularity vs number of interactions (start after 1 min)
(g3 <- ggplot(df %>% filter(duration > 60), aes(x = links, y = mod, col = game)) +
  geom_point() + 
  geom_smooth() +
  xlab("number of interactions") +
  ylab("modularity") +
  theme_classic())

ggsave("figures/mod_links.png")



# visualize networks

visualize_network <- function(gameid) {
  
  # select game
  game <- make_networks(gameid)
  
  # convert to graph and simplify
  N <- graph_from_adjacency_matrix(game$Ns[,,dim(game$Ns)[3]])
  N <- delete.vertices(simplify(N), degree(N)==0)
  
  N_tbl <- as_tbl_graph(N)
  
  # visualize network
  ggraph(N_tbl, layout = 'kk', maxiter = 100) + 
    geom_node_point(aes(size = degree(N), col = game$roles)) + 
    scale_size(name = "number of interactions", range = c(2,10)) +
    geom_edge_link(alpha = 0.25, arrow = arrow(length=unit(0.08, "inches"))) +
    theme_graph() +
    scale_color_discrete(name = "roles") 
  
}

(gn1 <- visualize_network(1))
ggsave("figures/network_game1.png", scale = 1.1)

(gn2 <- visualize_network(2))
ggsave("figures/network_game2.png", scale = 1.1)

(gn3 <- visualize_network(3))
ggsave("figures/network_game3.png", scale = 1.1)

(gn4 <- visualize_network(4))
ggsave("figures/network_game4.png",scale = 1.1)

(gn5 <- visualize_network(5))
ggsave("figures/network_game5.png", scale = 1.1)



##### Instant networks #####

make_networks_t <- function(gameid, wind = 300, int = 30) {
  
  # make quantitative networks (cumulative)
  game_quant <- make_networks(gameid, "quantitative")
  
  # get networks and duration
  Ns <- game_quant$Ns
  d <- as.numeric(game_quant$duration)

  # count number of networks to compute
  n_networks <- round((max(d) - wind) / int)
  
  # initialize instant networks and duration
  Ns_t <- array(0, dim = c(dim(Ns)[1], dim(Ns)[2], n_networks))
  d_t <- rep(0, n_networks) # duration
  ti <- rep(0, n_networks) # initial time
  tf <- rep(0, n_networks) # final time
  
  # get instant networks
  for (i in 1:n_networks) {
    
    tf[i] <- wind + (i-1)*int
    ti[i] <- (i-1)*int + 1
    
    Ns_t[,,i] <- Ns[,,sum(d < tf[i])] - Ns[,,sum(d < ti[i])]
    Ns_t[,,i] <- as.numeric(Ns_t[,,i] > 0)
    d_t[i] <- tf[i] - (tf[i] - ti[i]) / 2
  }
  return(list(Ns = Ns_t, duration = d_t, ti = ti, tf = tf))
}

# compute instant measures for all game
df_t <- data.frame()

for (i in 1:5) {
  
  # generate networks for game i and get durations at which observations were observed
  game <- make_networks_t(i)
  duration <- game$duration
  ti <- game$ti
  tf <- game$tf
  
  # make graphs
  
  # compute number of links through time
  links <- count_links(game)
  
  # compute modularity through time
  mod <- measure_modularity(game)
  
  # add rows to dataframe
  df_t <- rbind(df_t, data.frame(duration = duration, 
                             ti = ti, 
                             tf = tf,
                             links = links, 
                             mod = mod,
                             game = rep(i, length(duration))))
}

df_t <- df_t %>% 
  mutate(duration = as.numeric(duration), game = as.factor(game))


(g4 <- ggplot(df_t, aes(x = duration, y = links, col = game)) +
  geom_line() +
  geom_point() + 
  geom_errorbarh(aes(xmax = tf, xmin = ti), alpha = 0.2, height = 0) +
  xlim(0, 2000) +
  xlab("time (s)") +
  ylab("number of interactions") +
  theme_classic())
ggsave("figures/links_time_instant.png", scale = 1.1)


(g5 <- ggplot(df_t, aes(x = duration, y = mod, col = game)) +
  geom_line() +
  geom_point() + 
  geom_errorbarh(aes(xmax = tf, xmin = ti), alpha = 0.2, height = 0) +
  xlim(0, 2000) +
  ylim(0.6, 1) +
  xlab("time (s)") +
  ylab("modularity") +
  theme_classic())
ggsave("figures/mod_time_instant.png", scale = 1.1)