tblMasters <- tblGames %>%
  group_by(Master) %>%
  summarise(MaxElo = max(EloMaster, na.rm = TRUE)) %>%
  left_join(
    tblGames %>% 
      select(Master, FIDE_ID) %>% 
      distinct(),
    by = "Master"
  ) %>%
  left_join(
    tblGames %>% 
      group_by(Master) %>% 
      filter(EloMaster == max(EloMaster, na.rm = TRUE)) %>% 
      summarise(MinDate = min(Date), MaxDate = max(Date)) %>% 
      select(Master, MinDate, MaxDate),
    by = "Master"
  )