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

# ========================================================================
# ========================================================================




# ========================================================================
# 3. Combine tables
# ========================================================================


# Prepare tables ---------------------------------------------------------

# Bind individual tables
tab <- rbind(tab1, tab2, tab3)

# Only use complete cases for buffers
tab <- tab[complete.cases(buffer)]
tab <- tab[buffer == 1]



# Merge tables -----------------------------------------------------------





# ========================================================================
# ========================================================================




tab[, game_id := NULL]
df1 <- tab
df2 <- chases_dt[, .(timestamp, game_id, anonymous_id, role, success)]



# Assuming your first dataset is stored in 'df1' and the second dataset is in 'df2'

# Create a new column in 'df1' to store the matching values
df1$matching <- 0

# Iterate over each row in 'df1'
for (i in 1:nrow(df1)) {
  # Get the current timestamp from 'df1'
  timestamp1 <- df1$timestamp[i]

  # Find matching rows in 'df2' with the same ID
  matching_rows <- df2[df2$anonymous_id == df1$anonymous_id[i], ]

  # Check if there are any matching rows
  if (nrow(matching_rows) > 0) {
    # Iterate over each matching row
    for (j in 1:nrow(matching_rows)) {
      # Get the timestamp from 'df2'
      timestamp2 <- matching_rows$timestamp[j]

      # Calculate the time difference in seconds
      time_diff <- as.numeric(difftime(timestamp1, timestamp2, units = "secs"))

      # Check if the time difference is within the threshold
      if (time_diff <= 10) { #here, check if absolute diff is the problem
        # Assign 1 to the matching column in 'df1'
        df1$matching[i] <- 1
        # Break the loop as a match is found
        break
      }
    }
  }
}

# Print the combined dataset
combined_dataset <- merge(
    df1, df2, by = c("anonymous_id", "timestamp"),
    all.y = TRUE
)
print(combined_dataset)
dt <- combined_dataset[complete.cases(player_id_buffer)]
dt




# Solution 2 (best to date)

# Create a new column in df1 to store the event occurrence information
df1$event_occurred <- 0

# Iterate through the rows of df1
for (i in 1:nrow(df1)) {
  # Get the anonymous_id and timestamp from the current row in df1
  id <- df1$anonymous_id[i]
  timestamp <- df1$timestamp[i]
  
  # Find the corresponding row(s) in df2 with the same anonymous_id
  matching_rows <- df2[df2$anonymous_id == id, ]
  
  # Check if any matching row(s) have a timestamp within 20 seconds of the current timestamp in df1
  if (any(abs(matching_rows$timestamp - timestamp) <= 15)) {
    df1$event_occurred[i] <- 1  # Event occurred
  }
}

# Create 10-second windows
df1$window <- floor_date(df1$timestamp, "10 seconds")

# Calculate the number of unique IDs within each window for each anonymous_id
df1 <- df1 %>%
  group_by(window, anonymous_id) %>%
  mutate(ids_around = n_distinct(player_id_buffer)) %>%
  ungroup()

# Fill missing values with 0
df1$ids_around[is.na(df1$ids_around)] <- 0


