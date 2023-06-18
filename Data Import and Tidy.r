#--------------------------------------------------------------------------------

# FILE:                   Data Import and Tidy
# DESCRIPTION:            Imports the chess masters downloads found here:
#                         https://www.pgnmentor.com/files.html
#                         Converts data into table with IDs, and adds derived cols.

# AUTHOR:                 Tom Hook
# CONTACT:                thomashook1@outlook.com
# LAST UPDATED            18/06/2023
# VERSION:                v0.1                              



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

# Set file name
strFile <- "Carlsen.pgn"

# Read data
tblData <- read.table(strFile, quote="", sep="\n", stringsAsFactors=FALSE)

# Read Opening Reference file (credit to randywolf244 on Lichess)
tblOpeningRef <- read.csv("Chess Opening Reference.csv")



# Remove '[', ']' and '"'
tblData$V1 <- gsub("\\[|\\]|\"", "", tblData$V1)

# Add a column name for moves (they start with a digit...)
tblData$V1 <- paste0(ifelse(grepl("^\\d", tblData$V1), "Moves ", ""), tblData$V1)


# Splitting the column into two based on first space in string
tblData <- data.frame(
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
  group_by(ColumnHeader, ID) %>%
  summarize(Value = paste(Value, collapse = " "))

# Remove old moves from tblData and add new combined string
tblData <- tblData %>%
  filter(ColumnHeader != "Moves") %>%
  bind_rows(tblMoves)


# Pivot the data
tblData <- pivot_wider(tblData, names_from = ColumnHeader, values_from = Value)


# Remove games with no elo value for white or black
tblData <- tblData %>% 
  filter(!is.na(WhiteElo)) %>%
  filter(!is.na(BlackElo)) %>%
  filter(WhiteElo != "?") %>%
  filter(BlackElo != "?")


# Set data types of cols
tblData <- tblData %>%
  mutate(Date = as.Date(Date, format = "%Y.%m.%d"),
         WhiteElo = as.integer(WhiteElo),
         BlackElo = as.integer(BlackElo)
  )


# Drop unnecessary values
rm(list = setdiff(ls(), c("tblData", "tblOpeningRef")))



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
    
    # Elo bucket (nearest 100)
    WhiteEloRounded = round(WhiteElo, -2),
    BlackEloRounded = round(BlackElo, -2),
    
    # Elo for the master and opponent
    MasterElo = ifelse(grepl("Carlsen", tblData$White), WhiteElo, BlackElo),
    OpponentElo = ifelse(grepl("Carlsen", tblData$White), BlackElo, WhiteElo)
  )

print("script complete")

test <- tblData %>%
  filter(Black == "Carlsen,Magnus")
