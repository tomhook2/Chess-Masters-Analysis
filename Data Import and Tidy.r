#--------------------------------------------------------------------------------

# FILE:                   Data Import and Tidy
# DESCRIPTION:            Imports the chess masters downloads found here:
#                         https://www.pgnmentor.com/files.html
#                         Converts data into table with IDs, and adds derived cols.

# AUTHOR:                 Tom Hook
# CONTACT:                thomashook1@outlook.com
# LAST UPDATED            22/06/2023
# VERSION:                v0.3                             



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

# Define empty data frame
tblData <- data.frame(V1 = character(), FileName = character())

# Create vector for Player names
vecPlayer <- vecFile %>%
  gsub("\\.pgn", "", .) %>%
  gsub("Downloads/", "", .)

# Import all player data frames at once and combine
for (i in 1:floor(length(vecFile)/8)) {
  # Read PGN file
  tblNewData <- read.table(vecFile[i], quote="", sep="\n", stringsAsFactors=FALSE)
  
  # Assign Lastname, First Initial to player (this is calculated from the most common names in the dataset)
  tblNewData$Player <- tblNewData$V1 %>%
    str_extract("\\[(White|Black) \"(.*?)\"\\]") %>%
    str_replace_all("\\[(White|Black) \"|\"\\]", "") %>%
    na.omit() %>%
    {
      namesCut <- ifelse(grepl(",", .), paste(sub(",.*", "", .), sub(".*,\\s*(\\w).*", "\\1", .), sep = ", "), .)
      cntNames <- table(namesCut)
      names(cntNames)[which.max(cntNames)]
    }
  
  # Lets user know how far the script has got
  print(tblNewData$Player[1])
  
  # Add to tblData
  tblData <- rbind(tblData, tblNewData)
}

# hold data here
hold <- tblData
tblData <- hold

# Remove final player data frame
rm(tblNewData)

# Player Carlos Torre Repetto has some poor DQ. Removing for now. 
# e.g. no result at end of moves, annotation in moves.
# See v0.2 for some effort towards fixing.
tblData <- tblData %>%
  filter(Player != 'Carlos Torre Repetto')


# Remove '[', ']' and '"', and replace "??" in dates with "01"
tblData$V1 <- str_replace_all(tblData$V1, pattern = "\\[|\\]|\"", replacement = "")
tblData$V1 <- str_replace_all(tblData$V1, pattern = "\\?\\?", replacement = "01")

# Some data imported has random lines in it. We need to keep the rows that are moves and remove everything else
tblData$V1 <- ifelse(!grepl("^(Event|Site|Date|Round|White|Black|Result|WhiteElo|BlackElo|ECO)\\b", 
                            tblData$V1),
                     paste0("Moves ", tblData$V1),
                     tblData$V1)

# Add a column name for moves (they start with a digit...) NOT TRUE
#tblData$V1 <- paste0(ifelse(grepl("^\\d", tblData$V1), "Moves ", ""), tblData$V1)


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
#tblData <- tblData %>%
#  filter(ColumnHeader %in% c("Event", "Site", "Date", "Round", "White", "Black",
#                      "Result", "WhiteElo", "BlackElo", "ECO" , "Moves"))

# Find Duplicates
tblDuplicates <- {tblData} %>%
  group_by(Player, ID, ColumnHeader) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 1L)

# Pivot the data
tblData <- pivot_wider(tblData, names_from = ColumnHeader, values_from = Value)

#--------------------------------------------------------------------------------
# 1.1 Fix/remove poor data quality

# Some moves have game data before the first move. Remove so PGN can be recognised.
tblData <- tblData %>%
  mutate(Moves = sub(".*?(1.*)", "\\1", Moves))


# Remove doubles games (Staunton Mem Consultation game: Short/Vujatovic/Kasparov/Crumiller)
tblData <- tblData %>%
  filter(!grepl(":", WhiteElo))

# Remove games where White pieces player is the same as the Black pieces play
tblData <- tblData %>%
  filter(
    White != Black 
  )

# This leaves 1 DQ problem. Lasker, Edward vs Lasker, Emanuel from 1924, they played twice.
# I'm just going to remove them for now to get rid of the issue. This probably needs revisiting.
tblData <- tblData %>%
  filter(!ID %in% c(219918, 219925))


# No space after surname in lots of the data, add one for col "White" and "Black"
tblData <- tblData %>%
  mutate(White = gsub("(?<! ),(?! )", ", ", White, perl = TRUE),
         Black = gsub("(?<! ),(?! )", ", ", Black, perl = TRUE))

# Nick de Firmian's name is styled as "DeFirmian" sometimes
# And de La Bouradonnais is styled differently... Going with the more common way

tblData <- tblData %>%
  mutate(White = ifelse(White == "DeFirmian, N", "De Firmian, N", White),
         Black = ifelse(Black == "DeFirmian, N", "De Firmian, N", Black),
         White = ifelse(White == "De la Bourdonnais, Louis", "De Labourdonnais, L", White),
         Black = ifelse(Black == "De la Bourdonnais, Louis", "De Labourdonnais, L", Black))

# Remove games that the Player string isn't in either "White" or "Black" columns
tblData <- tblData %>%
  rowwise() %>%
  filter(grepl(Player, White) | grepl(Player, Black)) %>%
  ungroup()


# Convert blanks in Elo, and ? in Round to NAs so the columns can be converted to integer type
tblData <- tblData %>% 
  mutate(WhiteElo = na_if(WhiteElo, ""),
         BlackElo = na_if(BlackElo, ""),
         WhiteElo = na_if(WhiteElo, "?"),
         BlackElo = na_if(BlackElo, "?"),
         Round = na_if(Round, "?"))

# Set data types of cols
tblData <- tblData %>%
  mutate(Date = as.Date(Date, format = "%Y.%m.%d"),
         WhiteElo = as.integer(WhiteElo),
         BlackElo = as.integer(BlackElo)
  )


# Drop unnecessary values
rm(list = setdiff(ls(), c("tblData", "start","hold")))



#--------------------------------------------------------------------------------
# 2. Derive new columns

# Read Opening Reference file (credit to randywolf244 on Lichess)
tblOpeningRef <- read.csv("Chess Opening Reference.csv")

# Left join opening reference
tblData <- left_join(tblData, tblOpeningRef, by = c("ECO" = "ECO.Code"))

# Rename new columns
tblData <- tblData %>%
  rename("Opening" = "Name") %>%
  rename("Opening Moves" = "Opening.Moves")


# Derived columns
tblData <- tblData %>%
  mutate(
    # Result
    Result = sub(".+\\s(.+)$", "\\1", Moves),
    
    # Move count
    MoveCount = str_count(Moves, "\\."),
    
    # No. captures
    CaptureCount = str_count(Moves, "x"),
    
    # Did castle occur? 
    Castle = ifelse(grepl("O-O", Moves), TRUE, FALSE),
    
    # Castle direction
    CastleDirection = ifelse(grepl("O-O-O", Moves), "Long castle",
                             ifelse(grepl("O-O", Moves), "Short castle", NA)),
    
    # White/Black for master
    MasterPieces = ifelse(str_detect(White, Player), "White",
                          ifelse(str_detect(Black, Player), "Black", NA)),
    
    # White/Black for opponent
    OpponentPieces = ifelse(str_detect(White, Player), "Black",
                          ifelse(str_detect(Black, Player), "White", NA)),
    
    # Elo for the master 
    MasterElo = ifelse(str_detect(White, Player), WhiteElo,
                          ifelse(str_detect(Black, Player), BlackElo, NA)),
    
    # Elo for the opponent
    OpponentElo = ifelse(str_detect(White, Player), BlackElo,
                          ifelse(str_detect(Black, Player), WhiteElo, NA))
  ) %>%

  # Remove games where result is "*"
  filter(Result != "*")


print("script complete")


print(Sys.time() - start)



# Note to self:
# File name doesnt have full name

# What time lengths are the games? Mode? e.g. classical vs rapid etc.
# use this site https://ratings.fide.com/profile/1503014/chart
# for elo charts, take unique events! removes the same level dots

