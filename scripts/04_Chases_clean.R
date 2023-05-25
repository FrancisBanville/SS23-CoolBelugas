# ========================================================================

#          Script to prepare a clean table to analyses chases            #

# ========================================================================


# ========================================================================
# 1. Import librairies and data
# ========================================================================

library(data.table)
library(lubridate)

# import matrices
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

# Import predator chase data
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

# Matrix 1
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
# Matrix 2
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

# Matrix 3
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

# Bind
tab <- rbind(tab1, tab2, tab3)
tab <- tab[complete.cases(buffer)]

test <- merge(
    tab, chases_dt,
    by = c("timestamp", "anonymous_id", "game_id"),
    all = TRUE
)

unique(
    test[, .(anonymous_id, success, game_id, lon_chase_igor, lat_chase_igor)]
)

chases_dt1 <- chases_dt

setnames(chases_dt1, "timestamp", "timestamp_dt")

test1 <- chases_dt1 %>%
    left_join(tab, by = NULL, relationship = "many-to-many") %>%
    filter((timestamp_dt - timestamp) <= 20)

test2 <- test1[buffer == 1]

a <- unique(
    test1[, .(anonymous_id, success, game_id, lon_chase_igor, lat_chase_igor)]
)

b <- unique(
    test2[, .(anonymous_id, success, game_id, lon_chase_igor, lat_chase_igor)]
)

merge(a, b, all = TRUE)


# ========================================================================
# ========================================================================
