#--------------------------------------------------------------------------------

# FILE:                   Players
# DESCRIPTION:            Imports the Combined list STD, BLZ, RPD format TXT format file found on FIDE
#                         site http://ratings.fide.com/download_lists.phtml.

# AUTHOR:                 Tom Hook
# CONTACT:                thomashook1@outlook.com
# LAST UPDATED:           02/07/2023
# VERSION:                v0.2                             



#--------------------------------------------------------------------------------

# Clear environment
rm(list = ls())

# Install/Load the libraries
if(!require("pacman")) install.packages("pacman"); library (pacman)
pacman::p_load(readxl, dplyr, tidyverse, zoo, here, lubridate, rstudioapi, tidyr, 
               data.table, stringr, openxlsx, shiny, xml2, readr)

# Get the path of this saved file and set working directory to 2 folders above.
strPath = getActiveDocumentContext()$path
setwd(dirname(dirname(dirname(strPath))))

# Set the column specifications. Data is in a txt file, awkwardly formatted.
# Numbers below are the character lengths of each column
tblColumns <- fwf_widths(c(15, 61, 4, 4, 5, 5, 15, 4, 6, 4, 3, 6, 4, 3, 6, 4, 3, 6, 4))

# Extract the zip file
zipfile <- "Downloads/players_list.zip"
unzip(zipfile, exdir = "temp")

# Read the file
strFilePath <- file.path("temp", "players_list_foa.txt")
tblPlayers <- read_fwf(strFilePath, col_positions = tblColumns)

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


# Rename name col to full name
tblPlayers <- tblPlayers %>%
  rename(FIDEName = Name)


# Write as RDS file
saveRDS(tblPlayers, "Data Outputs/FIDE Players.rds")

# Clean up by deleting the temporary directory
unlink("temp", recursive = TRUE)



