#--------------------------------------------------------------------------------

# FILE:                   Profiles
# DESCRIPTION:            Creates a FIDE Profiles table for each Master with
#                         games available for download.

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
               data.table, stringr, openxlsx, shiny)

# Get the path of this saved file and set working directory to 2 folders above.
strPath = getActiveDocumentContext()$path
setwd(dirname(dirname(dirname(strPath))))

# Read data

tblPlayers <- readRDS("Data Outputs/FIDE Players.rds")
tblMastersID <- read.csv("FIDE IDs.csv")

# Credit to https://www.chessgames.com/alpha3.html for making the countries download available
tblCountries <- read.csv("Downloads/Countries.csv") 

# Convert ID col to character
tblMastersID <- tblMastersID %>% 
  mutate(FIDE_ID = as.character(FIDE_ID))

# Join players data from FIDE
tblProfiles <- tblMastersID %>%
  left_join(tblPlayers, by = c("FIDE_ID" = "ID Number"))

# Reorder cols to have FIDE ID 1st col
tblProfiles <- tblProfiles[, c("FIDE_ID", setdiff(names(tblProfiles), "FIDE_ID"))]

# Add geography data
tblProfiles <- tblProfiles %>%
  left_join(tblCountries, by = "Fed")

# Remove Carlos Torre Repetto data due to poor DQ
tblProfiles <- tblProfiles %>%
  filter(Master != "Torre Repetto, C")

# Save file at game level to be used in r markdown
write.csv(tblProfiles, "Data Outputs/Profiles.csv", row.names = FALSE)



