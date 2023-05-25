library(dplyr)
library(ggplot2)
library(gifski)
library(gganimate)
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

for (i in 1:max(trophic_data$game_id)) {
  
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

df_nocomp <- df %>% 
  filter(game %in% c(1:6))

df_comp <- df %>% 
  filter(game %in% c(7:9))

##### visualize data ##### 

# number of interactions through time
(g1 <- ggplot(df, aes(x = duration, y = links, col = game)) +
  geom_line(size = 1.5) +
  xlab("duration (s)") +
  ylab("number of interactions") +
  theme_classic())

ggsave("figures/links_time.png")

(g1comp <- ggplot() +
    geom_line(data = df_nocomp, aes(x = duration, y = links, linetype = game), col = "grey", alpha = 0.8, size = 0.3) +
    geom_line(data = df_comp, aes(x = duration, y = links, col = game), size = 1.5) +
    labs(linetype = "low competition", colour="high competition") +
    xlab("duration (s)") +
    ylab("number of interactions") +
    theme_classic())

ggsave("figures/links_time_comp.png")


## animate plot
g1_anim <- ggplot(df, aes(x = duration, y = links, col = game)) +
  geom_point(size = 4) + 
  geom_line(size = 1, alpha = 0.8) +
  labs(col = "game") +
  xlab("duration (s)") +
  ylab("number of interactions") +
  theme_classic() +
  transition_reveal(duration) +
  ease_aes('linear')

animate(g1_anim, duration = 10, fps = 20, width = 600, height = 600, renderer = gifski_renderer())
anim_save("figures/links_time_animate.gif")


# modularity through time (start after 1 min)
(g2 <- ggplot(df %>% filter(duration > 60), aes(x = duration, y = mod, col = game)) +
  geom_line(size = 1.5) +
  xlab("duration (s)") +
  ylab("modularity") +
  theme_classic())

ggsave("figures/mod_time.png")

(g2comp <- ggplot() +
    geom_line(data = df_nocomp %>% filter(duration > 60), aes(x = duration, y = mod, linetype = game), col = "grey", alpha = 0.8, size = 0.3) +
    geom_line(data = df_comp %>% filter(duration > 60), aes(x = duration, y = mod, col = game), size = 1.5) +
    labs(linetype = "low competition", colour="high competition") +
    xlab("duration (s)") +
    ylab("modularity") +
    theme_classic())

ggsave("figures/mod_time_comp.png")


# modularity vs number of interactions (start after 1 min)
(g3 <- ggplot(df %>% filter(duration > 60), aes(x = links, y = mod, col = game)) +
  geom_point(alpha = 0.3) +
  geom_smooth(size = 1.5) +
  xlab("number of interactions") +
  ylab("modularity") +
  theme_classic())

ggsave("figures/mod_links.png")

(g3comp <- ggplot() +
    geom_point(data = df_nocomp %>% filter(duration > 60), aes(x = links, y = mod, shape = game), col = "grey", alpha = 0.1, size = 0.4) +
    geom_smooth(data = df_nocomp %>% filter(duration > 60), aes(x = links, y = mod, linetype = game), col = "grey", alpha = 0.1, size = 0.4) +
    geom_point(data = df_comp %>% filter(duration > 60), aes(x = links, y = mod, col = game), alpha = 0.3) +
    geom_smooth(data = df_comp %>% filter(duration > 60), aes(x = links, y = mod, col = game), size = 1.5) +
    guides(shape = "none") +
    labs(linetype = "low competition", colour="high competition") +
    xlab("number of interactions") +
    ylab("modularity") +
    theme_classic())

ggsave("figures/mod_links_comp.png")



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

for (i in 1:max(trophic_data$game_id)) {
  (gn1 <- visualize_network(i))
  ggsave(filename = paste("figures/networks/network_game",i, ".png", sep = ""), scale = 1.1)
}

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

for (i in 1:max(trophic_data$game_id)) {
  
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

df_t_nocomp <- df_t %>% 
  filter(game %in% c(1:6))

df_t_comp <- df_t %>% 
  filter(game %in% c(7:9))

(g4 <- ggplot(df_t, aes(x = duration, y = links, col = game)) +
  geom_line(size = 1.5) +
  xlim(0, 2000) +
  xlab("time (s)") +
  ylab("number of interactions") +
  theme_classic())

ggsave("figures/links_time_instant.png")


(g4comp <- ggplot() +
    geom_line(data = df_t_nocomp, aes(x = duration, y = links, linetype = game), col = "grey", alpha = 0.8, size = 0.3) +
    geom_line(data = df_t_comp, aes(x = duration, y = links, col = game), size = 1.5) +
    xlim(0, 2000) +
    xlab("time (s)") +
    ylab("number of interactions") +
    labs(linetype = "low competition", colour="high competition") +
    theme_classic())

ggsave("figures/links_time_instant_comp.png")



(g5 <- ggplot(df_t, aes(x = duration, y = mod, col = game)) +
  geom_line(size = 1.5) +
  xlim(0, 2000) +
  ylim(0.6, 1) +
  xlab("time (s)") +
  ylab("modularity") +
  theme_classic())

ggsave("figures/mod_time_instant.png")

(g5comp <- ggplot() +
    geom_line(data = df_t_nocomp, aes(x = duration, y = mod, linetype = game), col = "grey", alpha = 0.8, size = 0.3) +
    geom_line(data = df_t_comp, aes(x = duration, y = mod, col = game), size = 1.5) +
    xlim(0, 2000) +
    ylim(0.6, 1) +
    xlab("time (s)") +
    ylab("modularity") +
    labs(linetype = "low competition", colour="high competition") +
    theme_classic())

ggsave("figures/mod_time_instant_comp.png")

