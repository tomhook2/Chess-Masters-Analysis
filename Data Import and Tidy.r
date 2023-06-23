#--------------------------------------------------------------------------------

# FILE:                   Data Import and Tidy
# DESCRIPTION:            Imports the chess masters downloads found here:
#                         https://www.pgnmentor.com/files.html
#                         Converts data into table with IDs, and adds derived cols.

# AUTHOR:                 Tom Hook
# CONTACT:                thomashook1@outlook.com
# LAST UPDATED            23/06/2023
# VERSION:                v0.4                             



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

# Import all player data frames at once and combine
for (i in 1:floor(length(vecFile)/1)) {
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

# Rename WhiteElo -> EloWhite and same for black
tblData <- tblData %>%
  rename(EloWhite = WhiteElo) %>%
  rename(EloBlack = BlackElo)


# Some moves have game data before the first move. Remove so PGN can be recognised.
tblData <- tblData %>%
  mutate(Moves = sub(".*?(1.*)", "\\1", Moves))


# Remove doubles games (Staunton Mem Consultation game: Short/Vujatovic/Kasparov/Crumiller)
tblData <- tblData %>%
  filter(!grepl(":", EloWhite))

# Remove games where White pieces player is the same as the Black pieces play
tblData <- tblData %>%
  filter(White != Black)

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
  mutate(EloWhite = na_if(EloWhite, ""),
         EloBlack = na_if(EloBlack, ""),
         EloWhite = na_if(EloWhite, "?"),
         EloBlack = na_if(EloBlack, "?"),
         Round = na_if(Round, "?"))

# Set data types of cols
tblData <- tblData %>%
  mutate(Date = as.Date(Date, format = "%Y.%m.%d"),
         EloWhite = as.integer(EloWhite),
         EloBlack = as.integer(EloBlack)
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
    
    # White/Black for master/opponent
    PiecesMaster = ifelse(str_detect(White, Player), "White",
                          ifelse(str_detect(Black, Player), "Black", NA)),
    PiecesOpponent = ifelse(str_detect(White, Player), "Black",
                            ifelse(str_detect(Black, Player), "White", NA)),
    
    # Did castle occur; White/Black/Master/Opponent
    CastleWhite = ifelse(grepl(".O-O", Moves), TRUE, FALSE),
    CastleBlack = ifelse(grepl(" O-O", Moves), TRUE, FALSE),
    CastleMaster = ifelse(PiecesMaster == "White", CastleWhite, CastleBlack),
    CastleOpponent = ifelse(PiecesOpponent == "White", CastleWhite, CastleBlack),
    
    
    # Castle direction; White/Black/Master/Opponent
    CastleDirectionWhite = ifelse(grepl(".O-O-O", Moves), "Long castle",
                             ifelse(grepl(".O-O", Moves), "Short castle", NA)),
    CastleDirectionBlack = ifelse(grepl(" O-O-O", Moves), "Long castle",
                                  ifelse(grepl(" O-O", Moves), "Short castle", NA)),
    CastleDirectionMaster = ifelse(PiecesMaster == "White", CastleDirectionWhite, CastleDirectionBlack),
    CastleDirectionOpoonent = ifelse(PiecesOpponent == "White", CastleDirectionWhite, CastleDirectionBlack),
    
    # Move count
    MoveCount = str_count(Moves, "\\."),
    
    # No. captures
    CaptureCount = str_count(Moves, "x"),
    
    # Result; White/Black/Master/Opponent
    ResultWhite = ifelse(Result == "1-0", "Win",
                          ifelse(Result == "0-1", "Loss", "Draw")),
    ResultBlack = ifelse(Result == "1-0", "Loss",
                         ifelse(Result == "0-1", "Win", "Draw")),
    ResultMaster = ifelse(PiecesMaster == "White", ResultWhite, ResultBlack),
    ResultOpponent = ifelse(PiecesOpponent == "White", ResultWhite, ResultBlack),
    
    # Elo for the master/opponent
    EloMaster = ifelse(str_detect(White, Player), EloWhite,
                          ifelse(str_detect(Black, Player), EloBlack, NA)),
    
    EloOpponent = ifelse(str_detect(White, Player), EloBlack,
                          ifelse(str_detect(Black, Player), EloWhite, NA)),
    
    # Is Ben Finegold Happy?
    Isf3PlayedWhite = ifelse(grepl("\\.f3", Moves), TRUE, FALSE),
    Isf3PlayedBlack = ifelse(grepl(" f6", Moves), TRUE, FALSE),
    Isf3PlayedMaster = ifelse(PiecesMaster == "White", Isf3PlayedWhite, Isf3PlayedBlack),
    Isf3PlayedOpponent = ifelse(PiecesOpponent == "White", Isf3PlayedWhite, Isf3PlayedBlack)
    
  ) %>%

  # Remove games where result is "*"
  filter(Result != "*")


print("script complete")


print(Sys.time() - start)



# Note to self:
# Create white moves and black moves.
# Split castling by if white or black castled? And direction
# White capture count, black capture count. Also split by master vs opponent? not sure
# Replace "White" and "Black" values by the master to be consistent. e.g. Adams, Michael -> Adams, M.

# What time lengths are the games? Mode? e.g. classical vs rapid etc.
# use this site https://ratings.fide.com/profile/1503014/chart
# for elo charts, take unique events! removes the same level dots

