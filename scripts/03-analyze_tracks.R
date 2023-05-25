library("tidyverse")
library("lubridate")
library("amt")
library("sf")
library("terra")
library("st")

##### Loading and cleaning data #####

metadata <- read.csv("data/raw/Metadata_Anonymous.csv") 

tracks_table.1 <- read.csv("data/raw/Tracking_Anonymous.csv", 
                   colClasses=c("game_id" = "factor"))
tracks_table <- read_csv("data/raw/Tracking_Anonymous.csv")
tracks_table.1$timestamp <- as_datetime(tracks_table.1$timestamp)
tracks_sf <- st_as_sf(tracks_table, coords=c("longitude", "latitude"), crs=4326, remove = F)
tracks_sf <- st_transform(tracks_sf, crs = 32619)
tracks_sf$X_new = st_coordinates(tracks_sf)[,"X"]
tracks_sf$Y_new = st_coordinates(tracks_sf)[,"Y"]



##### "Généralization" of the former codes on 1 game for 1 predator #####

# Keeping only game 3
tracks_game_3 <- dplyr::filter(tracks_sf, game_id==3)

# Selecting the predator of interest
Predator <- tracks_game_3 %>%
  dplyr::filter(player_id == "Independent Weasel")

# Selecting all the other players
Players <- tracks_game_3 %>%
  dplyr::filter(player_id != "Independent Weasel")

# Make a list of the other players track, one element for each player
Players_list <- split(x = Players, f = Players$player_id)


# Function that calculates the distance a each timestamp of the predator between the predator one player
distance_predator_player <- function(Player) {
  #Ordering by time
  Predator <- Predator[order(Predator$timestamp), ]
  Player <- Player[order(Player$timestamp), ]
  # Interpolating the X of the player for the predator timestamp
  Player_x_interp <- approx(x = Player$timestamp, y = Player$X_new, xout = Predator$timestamp, method = "linear")$y
  # Interpolating the Y of the player for the predator timestamp
  Player_y_interp <- approx(x = Player$timestamp, y = Player$Y_new, xout = Predator$timestamp, method = "linear")$y
  # Data frame withe the interpolated player X and Y
  position_predator_player <- data.frame(Predator[c("timestamp", "X_new", "Y_new")], Player_x_interp, Player_y_interp) %>%
    # Distance along X and Y axis between the predator and the player
    mutate(dist_X = abs(X_new - Player_x_interp),
           dist_Y = abs(Y_new - Player_y_interp)) %>%
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
  presence <- ifelse (dist < threshold, 1, 0)
  presence
}

# Apply the function on the distance data frame
Players_in_buffer <- apply(Players_dist, MARGIN = c(1,2), FUN = in_buffer)
table(Players_in_buffer)


