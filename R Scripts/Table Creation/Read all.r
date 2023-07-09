# Install/Load the libraries
if(!require("pacman")) install.packages("pacman"); library (pacman)
pacman::p_load(readxl, dplyr, tidyverse, zoo, here, lubridate, rstudioapi, tidyr, 
               data.table, stringr, openxlsx, shiny)

tblPlayers <- readRDS("Data Outputs/Titled Players.rds")
tblMastersID <- read.csv("FIDE IDs.csv")
tblProfiles <- read.csv("Data Outputs/Profiles.csv")
tblGames <- readRDS("Data Outputs/Games.rds")
tblOpeningRef <- read.csv("Downloads/Chess Opening Reference.csv")
