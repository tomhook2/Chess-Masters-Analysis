# Filter for current master
tblMasterGames <- tblGames %>%
  filter(FullName == params$Master)

# Table of most common openings by Master 
tblOpenings <- tblMasterGames %>%
  group_by(Opening) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  rename(`Game Count` = count) %>%
  slice(1:10) %>%
  kable()

# Table of most common openings by Master for white pieces
tblOpeningsWhite <- tblMasterGames %>%
  filter(PiecesMaster == "White") %>%
  group_by(Opening) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  slice(1:10) %>%
  rename(`Game Count` = count) %>%
  kable()


# Table of most common openings by Master for white pieces
tblOpeningsBlack <- tblMasterGames %>%
  filter(PiecesMaster == "Black") %>%
  group_by(Opening) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  rename(`Game Count` = count) %>%
  slice(1:10) %>%
  kable()