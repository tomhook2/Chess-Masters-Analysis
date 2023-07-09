# Install/Load the libraries
if(!require("pacman")) install.packages("pacman"); library (pacman)
pacman::p_load(readxl, dplyr, tidyverse, zoo, here, lubridate, rstudioapi, tidyr, 
               data.table, stringr, openxlsx, shiny)

# Get the path of this saved file and set working directly to folder above.
strPath = getActiveDocumentContext()$path
setwd(dirname(strPath))

tblProfiles <- read.csv("Data Outputs/Profiles.csv")

for (Master in tblProfiles$FullName[1]) {
  rmarkdown::render(
    input = "Profile-Pages.Rmd",
    output_file = file.path("Profiles", paste0(Master, ".html")),
    params = list(Master = Master)
  )
}

