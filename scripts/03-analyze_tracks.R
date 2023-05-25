# ====================================================================

#     Calculate player distances at each timestep in buffer zone     #

# ====================================================================

# Predators in games :
# - Game 9 : Peaceful Walrus, Bold Owl, Elusive Fox
# - Game 8 : Enduring Beaver, Curious Penguin
# - Game 7 : Silent Squirrel, Loyal Crocodile, Elusive Fox
# - Game 6: Daring Koala, Clever Lion

# Actual problems :
# - Peaceful Walrus game 9 not working
# - Bold Owl on game 9 not working
# - Elusive Fox on game 9 not working
# - All game 7 not working

# Notes on games :
# - Elusive Fox on game 7 did not play (didn't hear the bell)


# ====================================================================
# 1. Import data
# ====================================================================

# Load packages ------------------------------------------------------

library("tidyverse")
library("lubridate")
library("amt")
library("sf")
library("terra")
library("st")



# Loading and cleaning data ------------------------------------------

path <- file.path(getwd(), "data/raw")
tracks_table <- read_csv(
  file.path(path, "Trimmed_Tracks_Anonymous_AllGames.csv")
)

# Arrange timezone
tracks_table_20 <- tracks_table %>% filter(timestamp < "2023-05-21")
tracks_table_23 <- tracks_table %>% filter(timestamp > "2023-05-21")
hour(tracks_table_23$timestamp) <- hour(tracks_table_23$timestamp) - 4
tracks_table <- rbind(tracks_table_20, tracks_table_23)

tracks_sf <- st_as_sf(
  tracks_table, coords = c("longitude", "latitude"),
  crs = 4326, remove = FALSE
)
tracks_sf <- st_transform(tracks_sf, crs = 32619)
tracks_sf$X_new <- st_coordinates(tracks_sf)[, "X"]
tracks_sf$Y_new <- st_coordinates(tracks_sf)[, "Y"]

# ====================================================================
# ====================================================================





# ====================================================================
# 2. Compute procedure
# ====================================================================


# Generalization of the former codes on 1 game for 1 predator --------

# Keeping only game 3 (you can change the value here)
tracks_sf <- tracks_sf %>%
  dplyr::filter(game_id == 6)

# Selecting the predator of interest
Predator <- tracks_sf %>%
  dplyr::filter(player_id == "Clever Lion")

# Selecting all the other players
Players <- tracks_sf %>%
  dplyr::filter(player_id != "Clever Lion")

# Make a list of the other players track, one element for each player
Players_list <- split(x = Players, f = Players$player_id)


# Function that calculates the distance a each timestamp of the predator between the predator one player
distance_predator_player <- function(Player) {
  #Ordering by time
  Predator <- Predator[order(Predator$timestamp), ]
  Player <- Player[order(Player$timestamp), ]

  # Interpolating the X of the player for the predator timestamp
  Player_x_interp <- approx(
    x = Player$timestamp, y = Player$X_new,
    xout = Predator$timestamp, method = "linear",
    ties = "ordered"
  )$y

  # Interpolating the Y of the player for the predator timestamp
  Player_y_interp <- approx(
    x = Player$timestamp, y = Player$Y_new,
    xout = Predator$timestamp, method = "linear",
    ties = "ordered"
  )$y

  # Data frame withe the interpolated player X and Y
  position_predator_player <- data.frame(
    Predator[c("timestamp", "X_new", "Y_new")],
    Player_x_interp, Player_y_interp
  ) %>%

    # Distance along X and Y axis between the predator and the player
    mutate(
      dist_X = abs(X_new - Player_x_interp),
      dist_Y = abs(Y_new - Player_y_interp)
    ) %>%

    # Distance between the predator and the player (with Pythagore)
    mutate(dist = sqrt(dist_X**2 + dist_Y**2))
  position_predator_player$dist
}

# Apply the function on the list of players
Players_dist_list <- lapply(X = Players_list, FUN = distance_predator_player)

# Put everything back into a single matrix
Players_dist <- do.call(rbind, Players_dist_list)

# Make a data.frame and modify the colum names to have the timestamp pf the predator
Players_dist <- data.frame(Players_dist)
names(Players_dist) <- Predator$timestamp
Players_dist

# Function that returns 1 if a value is < 20m (and 0 in it is >)
in_buffer <- function(dist) {
  threshold <- 20
  presence <- ifelse(dist < threshold, 1, 0)
  presence
}

# Apply the function on the distance data frame
Players_in_buffer <- apply(
  Players_dist,
  MARGIN = c(1, 2),
  FUN = in_buffer
)
table(Players_in_buffer)

# ====================================================================
# ====================================================================





# ====================================================================
# Save the matrices
# ====================================================================

# Path
path1 <- file.path(getwd(), "data/raw/matrices")
saveRDS(
  Players_in_buffer,
  file = file.path(path1, "Apex_CleverLion_G6.rds")
)

# ====================================================================
# ====================================================================