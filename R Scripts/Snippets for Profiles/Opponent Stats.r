tblOpponentsCount <- tblGames %>%
  filter(FullName == params$Master) %>%
  filter(!is.na(ECO)) %>%
  group_by(Opponent) %>%
  summarise(`Game Count` = n(),
            `Win Rate` = sum(ResultMaster == "Win") / n(),
            `Loss Rate` = sum(ResultMaster == "Loss") / n(),
            `Most Common Opening` = Opening[which.max(table(Opening))]) %>%
  arrange(desc(`Game Count`)) %>%
  filter(Opponent != "NN") %>%
  slice(1:10) %>%
  mutate(`Win Rate` = percent(`Win Rate`, accuracy = 0.1),
         `Loss Rate` = percent(`Loss Rate`, accuracy = 0.1)) %>%
  gt() %>%
  tab_style(
    locations = cells_body(), 
    style = list(
      cell_text(size = px(12))
    )
  )
