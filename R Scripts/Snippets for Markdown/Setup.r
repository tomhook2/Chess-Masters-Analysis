# Load packages
if(!require("pacman")) install.packages("pacman"); library (pacman)
pacman::p_load(readxl, dplyr, tidyverse, zoo, here, lubridate, rstudioapi, tidyr, 
               data.table, stringr, openxlsx, shiny, knitr, flexdashboard, DT,
               kableExtra, gt)

# Load relevant data
tblGames <- readRDS("Data Outputs/Games.rds")
tblProfiles <- read.csv("Data Outputs/Profiles.csv")

