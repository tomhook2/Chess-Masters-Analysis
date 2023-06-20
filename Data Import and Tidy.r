#--------------------------------------------------------------------------------

# FILE:                   Data Import and Tidy
# DESCRIPTION:            Imports the chess masters downloads found here:
#                         https://www.pgnmentor.com/files.html
#                         Converts data into table with IDs, and adds derived cols.

# AUTHOR:                 Tom Hook
# CONTACT:                thomashook1@outlook.com
# LAST UPDATED            20/06/2023
# VERSION:                v0.2                             



#--------------------------------------------------------------------------------
# 0. Preliminaries

# Clear environment
rm(list = ls())

# Start timer to check how long code takes to run
start <- Sys.time()

# Install/Load the libraries
if(!require("pacman")) install.packages("pacman"); library (pacman)
pacman::p_load(readxl, dplyr, tidyverse, zoo, here, lubridate, rstudioapi, tidyr, 
               data.table, stringr, openxlsx, shiny)

# Get the path of this saved file and set working directly.
strPath = getActiveDocumentContext()$path
setwd(dirname(strPath))

#--------------------------------------------------------------------------------
# 1. Load and tidy data

# Find all PGN files in Downloads folder
vecFile <- list.files("Downloads", pattern = "\\.pgn$", full.names = TRUE)

# Create vector for Player names
vecPlayer <- vecFile %>%
  gsub("\\.pgn", "", .) %>%
  gsub("Downloads/", "", .)

# Define empty data frame
tblData <- data.frame(V1 = character(), FileName = character())

# Import all player data frames at once and combine
for (i in 1:length(vecPlayer)) {
  print(vecPlayer[i])
  tblNewData <- read.table(vecFile[i], quote="", sep="\n", stringsAsFactors=FALSE)
  tblNewData$Player <- vecPlayer[i]
  tblData <- rbind(tblData, tblNewData)
}

# Remove final player data frame
rm(tblNewData)

# Read Opening Reference file (credit to randywolf244 on Lichess)
tblOpeningRef <- read.csv("Chess Opening Reference.csv")

# Remove '[', ']' and '"', and replace "??" in dates with "01"
tblData$V1 <- str_replace_all(tblData$V1, pattern = "\\[|\\]|\"", replacement = "")
tblData$V1 <- str_replace_all(tblData$V1, pattern = "\\?\\?", replacement = "01")

# Add a column name for moves (they start with a digit...)
tblData$V1 <- paste0(ifelse(grepl("^\\d", tblData$V1), "Moves ", ""), tblData$V1)


# Splitting the column into two based on first space in string
tblData <- data.frame(
  Player = tblData$Player,
  ColumnHeader = str_split_fixed(tblData$V1, " ", n = 2)[, 1],
  Value = str_split_fixed(tblData$V1, " ", n = 2)[, 2]
)


# Create empty vectors for storing IDs
vecID <- vector("numeric", length = nrow(tblData))

# Set ID count up to label to games
ID <- 0

# Iterate over each row
for (i in 1:nrow(tblData)) {
  if (tblData$ColumnHeader[i] == "Event") {
    ID <- ID + 1  # Tick up ID when "Event" is found in ColumnHeader column
  }
  vecID[i] <- ID  # Store ID in the vector
}

# Add ID columns to the data
tblData$ID <- vecID


# Combine moves strings into separate table (by default, there's about 8 moves in each row but we want them all together)
tblMoves <- tblData %>%
  filter(ColumnHeader == "Moves") %>%
  group_by(ColumnHeader, ID, Player) %>%
  summarize(Value = paste(Value, collapse = " "))

# Remove old moves from tblData and add new combined string, remove Moves table
tblData <- tblData %>%
  filter(ColumnHeader != "Moves") %>%
  bind_rows(tblMoves)
rm(tblMoves)

# Raw data has some uncleansed rows. Let's just keep the rows we're expecting
tblData <- tblData %>%
  filter(ColumnHeader %in% c("Event", "Site", "Date", "Round", "White", "Black",
                      "Result", "WhiteElo", "BlackElo", "ECO" , "Moves"))

# Find Duplicates
tblDuplicates <- {tblData} %>%
  group_by(Player, ID, ColumnHeader) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 1L)

# Remove a line from the table that arises from poor DQ. Found in the "TorreRepetto" file on 1925.04.17
tblData <- tblData %>%
  filter(Value != "begins to be aggresive and therby he puts himself in the")


# Pivot the data
tblData <- pivot_wider(tblData, names_from = ColumnHeader, values_from = Value)


# Remove games with no elo value for white or black
tblData <- tblData %>% 
  filter(!is.na(WhiteElo)) %>%
  filter(!is.na(BlackElo)) %>%
  filter(WhiteElo != "?") %>%
  filter(BlackElo != "?")


# Set data types of cols
#tblData <- tblData %>%
#  mutate(Date = as.Date(Date, format = "%Y.%m.%d"),
#         WhiteElo = as.integer(WhiteElo),
#         BlackElo = as.integer(BlackElo)
#  )


# Drop unnecessary values
rm(list = setdiff(ls(), c("tblData", "tblOpeningRef", "start")))



#--------------------------------------------------------------------------------
# 2. Derive new columns

# Left join opening reference
tblData <- left_join(tblData, tblOpeningRef, by = c("ECO" = "ECO.Code"))

# Rename new columns
tblData <- tblData %>%
  rename("Opening" = "Name") %>%
  rename("Opening Moves" = "Opening.Moves")


# Derived columns
tblData <- tblData %>%
  mutate(
    # Move count
    MoveCount = str_count(Moves, "\\."),
    
    # No. captures
    CaptureCount = str_count(Moves, "x"),
    
    # Did castle occur? 
    Castle = ifelse(grepl("O-O", Moves), TRUE, FALSE),
    
    # Castle direction
    CastleDirection = ifelse(grepl("O-O-O", Moves), "Long castle",
                             ifelse(grepl("O-O", Moves), "Short castle", NA)),
    
    # Elo for the player and opponent
    MasterElo = ifelse(grepl("Carlsen", tblData$White), WhiteElo, BlackElo),
    OpponentElo = ifelse(grepl("Carlsen", tblData$White), BlackElo, WhiteElo)
  )

print("script complete")

end <- Sys.time()
print(end - start)

# Note to self:
# What time lengths are the games? Mode? e.g. classical vs rapid etc.
# use this site https://ratings.fide.com/profile/1503014/chart
# for elo charts, take unique events! removes the same level dots
