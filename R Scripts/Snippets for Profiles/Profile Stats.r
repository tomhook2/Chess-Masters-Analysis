# Filter profile and games table for current master
tblMasterProfile <- tblProfiles %>%
  filter(FullName == params$Master)

tblMasterGames <- tblGames %>%
  filter(FullName == params$Master)

intEloMax <- max(tblMasterGames$EloMaster, na.rm = TRUE)

dateEloMax <- min(tblMasterGames$Date[which(tblMasterGames$EloMaster == intEloMax & !is.na(tblMasterGames$Date))])

intGames <- nrow(tblMasterGames)

strFed <- tblMasterProfile$Fed

