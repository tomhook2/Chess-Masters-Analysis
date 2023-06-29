#--------------------------------------------------------------------------------

# FILE:                   Players Import and Tidy
# DESCRIPTION:            Imports the LEGACY format TXT format file found on FIDE
#                         site http://ratings.fide.com/download_lists.phtml.

# AUTHOR:                 Tom Hook
# CONTACT:                thomashook1@outlook.com
# LAST UPDATED:           24/06/2023
# VERSION:                v0.1                             



# Install/Load the libraries
if(!require("pacman")) install.packages("pacman"); library (pacman)
pacman::p_load(readxl, dplyr, tidyverse, zoo, here, lubridate, rstudioapi, tidyr, 
               data.table, stringr, openxlsx, shiny, xml2, readr)

# Set the column specifications. Data is in a txt file, awkwardly formatted.
# Numbers below are the character lengths of each column
cols <- fwf_widths(c(15, 61, 4, 4, 5, 5, 15, 6, 4, 3, 6, 4, 3, 6, 4, 3, 6, 4))

# Read the file
tblPlayers <- read_fwf("players_list.txt", col_positions = cols)

# Promote the first row as column headers
colnames(tblPlayers) <- as.character(unlist(tblPlayers[1, ]))

# Remove the first row from the data as it's the col headers
tblPlayers <- tblPlayers[-1, ]

# Convert col types
tblPlayers <- tblPlayers %>% 
  mutate(SRtng = as.numeric(SRtng),
         SGm = as.numeric(SGm),
         SK = as.numeric(SK),
         RRtng = as.numeric(RRtng),
         RGm = as.numeric(RGm),
         Rk = as.numeric(Rk),
         BRtng = as.numeric(BRtng),
         BGm = as.numeric(BGm),
         BK = as.numeric(BK),
         `B-day` = as.numeric(`B-day`))

# Filter just for titled players
tblPlayers <- tblPlayers %>%
  filter(!is.na(Tit) | !is.na(WTit) | !is.na(OTit))

# Tidy names to last name, first initial - IS THIS NEEDED
tblPlayers <- tblPlayers %>%
  mutate(
    Name = gsub(pattern = "^(.*),\\s*(.).*$", replacement = "\\1, \\2", Name)
  )

# Write as RDS file
saveRDS(tblPlayers, "Players.rds")




