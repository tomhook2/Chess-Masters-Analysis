# Filter for current master
tblGamesPerYear <- tblGames %>%
  filter(FullName == params$Master) %>%
  mutate(Date = as.Date(paste0(format(Date, "%Y"), "-01-01"))) %>%
  group_by(Date) %>%
  summarise(Count = n())

plotGamesPerYear <- ggplot(tblGamesPerYear, aes(x = Date, y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black") +
  labs(x = "Year", y = "Number of Games per Year") +
  theme_classic() +
  geom_text(aes(label = Count), color = "white", hjust = 1.2, size = 4, angle = 90)

dateGamesMax <- tblGamesPerYear$Date[tblGamesPerYear$Count == max(tblGamesPerYear$Count)]
