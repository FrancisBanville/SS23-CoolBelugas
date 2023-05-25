library("tidyverse")
library("lubridate")
library("amt")
library("sf")
library("terra")
library("st")

tracks <- read.csv("data/raw/Tracking_Anonymous.csv", 
                   colClasses=c("game_id" = "factor"))
metadata <- read.csv("data/raw/Metadata_Anonymous.csv") 

head(tracks)
summary(tracks)
