# ========================================================================

#          Script to prepare a clean table to analyses chases            #

# ========================================================================





# ========================================================================
# 1. Import librairies and data
# ========================================================================


# Load librairies --------------------------------------------------------
library(data.table)
library(lubridate)
library(dplyr)



# Import matrices --------------------------------------------------------

path <- file.path(getwd(), "data/raw/matrices")
mat1 <- readRDS(
    file.path(path, "Apex_CleverLion_G6.rds")
)
mat2 <- readRDS(
    file.path(path, "Apex_CuriousPenguin_G8.rds")
)
mat3 <- readRDS(
    file.path(path, "Apex_DaringKoala_G6.rds")
)



# Import predator chase data ---------------------------------------------

path1 <- file.path(getwd(), "data/raw")
chases_dt <- fread(file.path(path1, "Predator_chases.csv"))

# Rename variables
setnames(chases_dt, "sucess (kill=1)", "success")
setnames(chases_dt, "chase_timestamp", "timestamp")

# Filter only for the predators we need
chases_dt <- chases_dt[
    anonymous_id %in% c(
        "Clever Lion",
        "Curious Penguin",
        "Daring Koala")
]

# Timestamp to date
chases_dt[, timestamp := ymd("2023-05-23") + hms(timestamp)]

# ========================================================================
# ========================================================================





# ========================================================================
# 2. Reshape the matrices to tables
# ========================================================================


# Matrix 1 ---------------------------------------------------------------

tab1 <- melt(
    mat1,
    value.name = "buffer"
)
tab1 <- data.table(tab1)
setnames(tab1, c("Var1", "Var2"), c("player_id_buffer", "timestamp"))
tab1[, ":=" (
    timestamp = ymd_hms(timestamp),
    anonymous_id = "Clever Lion",
    game_id = 6
    )
]



# Matrix 2 ---------------------------------------------------------------

tab2 <- melt(
    mat2,
    value.name = "buffer"
)
tab2 <- data.table(tab2)
setnames(tab2, c("Var1", "Var2"), c("player_id_buffer", "timestamp"))
tab2[, ":=" (
    timestamp = ymd_hms(timestamp),
    anonymous_id = "Curious Penguin",
    game_id = 8
    )
]



# Matrix 3 ---------------------------------------------------------------

tab3 <- melt(
    mat3,
    value.name = "buffer"
)
tab3 <- data.table(tab3)
setnames(tab3, c("Var1", "Var2"), c("player_id_buffer", "timestamp"))
tab3[, ":=" (
    timestamp = ymd_hms(timestamp),
    anonymous_id = "Daring Koala",
    game_id = 6
    )
]



# Combine the tables -----------------------------------------------------

# Bind individual tables
tab <- rbind(tab1, tab2, tab3)

# Only use complete cases for buffers
tab <- tab[complete.cases(buffer)]

# Only use events where individuals were in the buffer zone
tab <- tab[buffer == 1]

# Remove game ID to simplify things
tab[, game_id := NULL]

# ========================================================================
# ========================================================================




# ========================================================================
# 3. Step 1: Calculate chases occuring within buffer
# ========================================================================

# Just keep most important variables
chases_dt <- chases_dt[, .(timestamp, game_id, anonymous_id, role, success)]


# Create a new column in tab to store the event occurrence information
tab$event_occurred <- 0


# Iterate through the rows of tab
for (i in 1:nrow(tab)) {
  # Get the anonymous_id and timestamp from the current row in df1
  id <- tab$anonymous_id[i]
  timestamp <- tab$timestamp[i]

  # Find the corresponding row(s) in chases_dt with the same anonymous_id
  matching_rows <- chases_dt[chases_dt$anonymous_id == id, ]

  # Check if any matching row(s) have a timestamp
  # within 15 seconds of the current timestamp in tab
  if (any(abs(matching_rows$timestamp - timestamp) <= 15)) {
    tab$event_occurred[i] <- 1  # Event occurred
  }
}

# ========================================================================
# ========================================================================





# ========================================================================
# 4. Step 2 : Calcualte N players inside buffer at each timestep
# ========================================================================

# Create 10-second windows to calculate
# number of players in buffer at each timestep
tab$window <- floor_date(tab$timestamp, "10 seconds")

# Calculate the number of unique IDs
# within each window for each anonymous_id
tab <- tab %>%
  group_by(window, anonymous_id) %>%
  mutate(ids_around = n_distinct(player_id_buffer)) %>%
  ungroup()

# Fill missing values with 0
tab$ids_around[is.na(tab$ids_around)] <- 0

# ========================================================================
# ========================================================================