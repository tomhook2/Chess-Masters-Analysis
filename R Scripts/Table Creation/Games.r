#--------------------------------------------------------------------------------

# FILE:                   Games
# DESCRIPTION:            Imports the chess masters downloads found here:
#                         https://www.pgnmentor.com/files.html
#                         Converts data into table with IDs, and adds derived cols.

# AUTHOR:                 Tom Hook
# CONTACT:                thomashook1@outlook.com
# LAST UPDATED:           23/06/2023
# VERSION:                v0.4                             



#--------------------------------------------------------------------------------
# 0. Preliminaries

# Clear environment
rm(list = ls())

# Install/Load the libraries
if(!require("pacman")) install.packages("pacman"); library (pacman)
pacman::p_load(readxl, dplyr, tidyverse, zoo, here, lubridate, rstudioapi, tidyr, 
               data.table, stringr, openxlsx, shiny, xlsx)

# Get the path of this saved file and set working directory to 2 folders above.
strPath = getActiveDocumentContext()$path
setwd(dirname(dirname(dirname(strPath))))

#--------------------------------------------------------------------------------
# 1. Load and tidy data

# Find all PGN files in Downloads folder
vecFile <- list.files("Downloads", pattern = "\\.pgn$", full.names = TRUE)

# Define empty data frame
tblGames <- data.frame(V1 = character(), FileName = character())

# Import all master data frames at once and combine
for (i in 1:length(vecFile)) {
  # Read PGN file
  tblNewGames <- read.table(vecFile[i], quote="", sep="\n", stringsAsFactors=FALSE)
  
  # Assign Lastname, First Initial to master (this is calculated from the most common names in the dataset)
  tblNewGames$Master <- tblNewGames$V1 %>%
    str_extract("\\[(White|Black) \"(.*?)\"\\]") %>%
    str_replace_all("\\[(White|Black) \"|\"\\]", "") %>%
    na.omit() %>%
    {
      namesCut <- ifelse(grepl(",", .), paste(sub(",.*", "", .), sub(".*,\\s*(\\w).*", "\\1", .), sep = ", "), .)
      cntNames <- table(namesCut)
      names(cntNames)[which.max(cntNames)]
    }
  
  # Lets user know how far the script has got
  print(tblNewGames$Master[1])
  
  # Add to tblGames
  tblGames <- rbind(tblGames, tblNewGames)
}

# Remove final Master data frame
rm(tblNewGames)

# Master Carlos Torre Repetto has some poor DQ. Removing for now. 
# e.g. no result at end of moves, annotation in moves.
# See v0.2 for some effort towards fixing.
tblGames <- tblGames %>%
  filter(Master != 'Carlos Torre Repetto')


# Remove '[', ']' and '"', and replace "??" in dates with "01"
tblGames$V1 <- str_replace_all(tblGames$V1, pattern = "\\[|\\]|\"", replacement = "")
tblGames$V1 <- str_replace_all(tblGames$V1, pattern = "\\?\\?", replacement = "01")

# Some data imported has random lines in it. We need to keep the rows that are moves and remove everything else
tblGames$V1 <- ifelse(!grepl("^(Event|Site|Date|Round|White|Black|Result|WhiteElo|BlackElo|ECO)\\b", 
                            tblGames$V1),
                     paste0("Moves ", tblGames$V1),
                     tblGames$V1)

# Add a column name for moves (they start with a digit...) NOT TRUE
#tblGames$V1 <- paste0(ifelse(grepl("^\\d", tblGames$V1), "Moves ", ""), tblGames$V1)


# Splitting the column into two based on first space in string
tblGames <- data.frame(
  Master = tblGames$Master,
  ColumnHeader = str_split_fixed(tblGames$V1, " ", n = 2)[, 1],
  Value = str_split_fixed(tblGames$V1, " ", n = 2)[, 2]
)


# Create empty vectors for storing IDs
vecID <- vector("numeric", length = nrow(tblGames))

# Set ID count up to label to games
ID <- 0

# Iterate over each row
for (i in 1:nrow(tblGames)) {
  if (tblGames$ColumnHeader[i] == "Event") {
    ID <- ID + 1  # Tick up ID when "Event" is found in ColumnHeader column
  }
  vecID[i] <- ID  # Store ID in the vector
}

# Add ID columns to the data
tblGames$ID <- vecID


# Combine moves strings into separate table (by default, there's about 8 moves in each row but we want them all together)
tblMoves <- tblGames %>%
  filter(ColumnHeader == "Moves") %>%
  group_by(ColumnHeader, ID, Master) %>%
  summarize(Value = paste(Value, collapse = " "))

# Remove old moves from tblGames and add new combined string, remove Moves table
tblGames <- tblGames %>%
  filter(ColumnHeader != "Moves") %>%
  bind_rows(tblMoves)
rm(tblMoves)

# Raw data has some uncleansed rows. Let's just keep the rows we're expecting
#tblGames <- tblGames %>%
#  filter(ColumnHeader %in% c("Event", "Site", "Date", "Round", "White", "Black",
#                      "Result", "WhiteElo", "BlackElo", "ECO" , "Moves"))

# Find Duplicates
tblDuplicates <- {tblGames} %>%
  group_by(Master, ID, ColumnHeader) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 1L)

# Pivot the data
tblGames <- pivot_wider(tblGames, names_from = ColumnHeader, values_from = Value)

#--------------------------------------------------------------------------------
# 1.1 Fix/remove/tidy poor data quality

# Rename WhiteElo -> EloWhite and same for black
tblGames <- tblGames %>%
  rename(EloWhite = WhiteElo) %>%
  rename(EloBlack = BlackElo)


# Some moves have game data before the first move. Remove so PGN can be recognised.
tblGames <- tblGames %>%
  mutate(Moves = sub(".*?(1.*)", "\\1", Moves))


# Remove doubles games (Staunton Mem Consultation game: Short/Vujatovic/Kasparov/Crumiller)
tblGames <- tblGames %>%
  filter(!grepl(":", EloWhite))

# Remove games where White pieces player is the same as the Black pieces play
tblGames <- tblGames %>%
  filter(White != Black)

# This leaves 1 DQ problem. Lasker, Edward vs Lasker, Emanuel from 1924, they played twice.
# I'm just going to remove them for now to get rid of the issue. This probably needs revisiting.
tblGames <- tblGames %>%
  filter(!ID %in% c(219918, 219925))


# No space after surname in lots of the data, add one for col "White" and "Black"
tblGames <- tblGames %>%
  mutate(White = gsub("(?<! ),(?! )", ", ", White, perl = TRUE),
         Black = gsub("(?<! ),(?! )", ", ", Black, perl = TRUE))

# Standardise names
tblGames <- tblGames %>%
  mutate(
    White = gsub(pattern = "^(.*),\\s*(.).*$", replacement = "\\1, \\2", White),
    Black = gsub(pattern = "^(.*),\\s*(.).*$", replacement = "\\1, \\2", Black)
  )


# Nick de Firmian's name is styled as "DeFirmian" sometimes
# And de La Bouradonnais is styled differently... Going with the more common way

tblGames <- tblGames %>%
  mutate(White = ifelse(White == "DeFirmian, N", "De Firmian, N", White),
         Black = ifelse(Black == "DeFirmian, N", "De Firmian, N", Black),
         White = ifelse(White == "De la Bourdonnais, L", "De Labourdonnais, L", White),
         Black = ifelse(Black == "De la Bourdonnais, L", "De Labourdonnais, L", Black))

# Li Chao's name is "Li Chao2", let's fix that
tblGames <- tblGames %>%
  mutate(Master = ifelse(Master == "Li Chao2", "Li Chao", Master))

# Remove games that the Master string isn't in either "White" or "Black" columns
tblGames <- tblGames %>%
  rowwise() %>%
  filter(grepl(Master, White) | grepl(Master, Black)) %>%
  ungroup()


# Convert blanks in Elo, and ? in Round to NAs so the columns can be converted to integer type
tblGames <- tblGames %>% 
  mutate(EloWhite = na_if(EloWhite, ""),
         EloBlack = na_if(EloBlack, ""),
         EloWhite = na_if(EloWhite, "?"),
         EloBlack = na_if(EloBlack, "?"),
         Round = na_if(Round, "?"))

# Set data types of cols
tblGames <- tblGames %>%
  mutate(Date = as.Date(Date, format = "%Y.%m.%d"),
         EloWhite = as.integer(EloWhite),
         EloBlack = as.integer(EloBlack)
  )


# Drop unnecessary values
rm(list = setdiff(ls(), c("tblGames", "start","hold")))



#--------------------------------------------------------------------------------
# 2. Derive new columns

# Add FIDE IDs
tblMastersID <- read.csv("FIDE IDs.csv")

# Left join FIDE IDs
tblGames <- left_join(tblGames, tblMastersID, by = c("Master" = "Master"))

# Reorder cols to have FIDE ID 1st col
tblGames <- tblGames[, c("FIDE_ID", setdiff(names(tblGames), "FIDE_ID"))]

# Convert ID col to character
tblGames <- tblGames %>% 
  mutate(FIDE_ID = as.character(FIDE_ID))

# Read Opening Reference file (credit to randywolf244 on Lichess)
tblOpeningRef <- read.csv("Downloads/Chess Opening Reference.csv")

# Left join opening reference
tblGames <- left_join(tblGames, tblOpeningRef, by = c("ECO" = "ECO.Code"))

# Rename new columns
tblGames <- tblGames %>%
  rename("Opening" = "Name") %>%
  rename("OpeningMoves" = "Opening.Moves")


# Derived columns
tblGames <- tblGames %>%
  mutate(
    # Result
    Result = sub(".+\\s(.+)$", "\\1", Moves),
    
    # Opponent
    Opponent = ifelse(Master == White, Black, White),
    
    # White/Black for master/opponent
    PiecesMaster = ifelse(Master == White, "White", "Black"),
    PiecesOpponent = ifelse(Master == White, "Black", "White"),
    
    # Did castle occur; White/Black/Master/Opponent
    CastleWhite = ifelse(grepl("\\.O-O", Moves), TRUE, FALSE),
    CastleBlack = ifelse(grepl(" O-O", Moves), TRUE, FALSE),
    CastleMaster = ifelse(PiecesMaster == "White", CastleWhite, CastleBlack),
    CastleOpponent = ifelse(PiecesOpponent == "White", CastleWhite, CastleBlack),
    
    
    # Castle direction; White/Black/Master/Opponent
    CastleDirectionWhite = ifelse(grepl("\\.O-O-O", Moves), "Long",
                             ifelse(grepl("\\.O-O", Moves), "Short", NA)),
    CastleDirectionBlack = ifelse(grepl(" O-O-O", Moves), "Long",
                                  ifelse(grepl(" O-O", Moves), "Short", NA)),
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
    EloMaster = ifelse(str_detect(White, Master), EloWhite,
                          ifelse(str_detect(Black, Master), EloBlack, NA)),
    
    EloOpponent = ifelse(str_detect(White, Master), EloBlack,
                          ifelse(str_detect(Black, Master), EloWhite, NA)),
    
    # Is Ben Finegold Happy?
    Isf3PlayedWhite = ifelse(grepl("\\.f3", Moves), TRUE, FALSE),
    Isf3PlayedBlack = ifelse(grepl(" f6", Moves), TRUE, FALSE),
    Isf3PlayedMaster = ifelse(PiecesMaster == "White", Isf3PlayedWhite, Isf3PlayedBlack),
    Isf3PlayedOpponent = ifelse(PiecesOpponent == "White", Isf3PlayedWhite, Isf3PlayedBlack)
    
  ) %>%

  # Remove games where result is "*"
  filter(Result != "*")



#--------------------------------------------------------------------------------
# 3. Save file at game level to be used in r markdown

saveRDS(tblGames, "Data Outputs/Games.rds")

print("script complete")

# Note to self:

# need a creative solution to add ID to the Masters dataset. joining on name isn't ideal...
# maybe add ID to the masters table manually? 

# What time lengths are the games? Mode? e.g. classical vs rapid etc.
# use this site https://ratings.fide.com/profile/1503014/chart
# for elo charts, take unique events! removes the same level dots
