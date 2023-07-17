tblRates <- tblGames %>%
  group_by(Master) %>%
  summarise(CountGames = n(),
            RateF3F6 = sum(Isf3PlayedMaster == TRUE)/n(),
            RateWin = sum(ResultMaster == "Win")/n(),
            RateCastle = sum(CastleMaster == TRUE)/n(),
            MoveMedian = median(MoveCount),
            CaptureMedian = median(CaptureCount)) %>%
  filter(CountGames >= 1000)

ggplot(tblRates, aes(x = RateF3F6, y = RateWin)) +
  geom_point() +
  geom_text_repel(aes(label = Master), force = 10, size = 3) +
  labs(x = "F3F6 Rate", y = "Win Rate") +
  theme_minimal()

ggplot(tblRates, aes(x = RateCastle, y = RateWin)) +
  geom_point() +
  geom_text_repel(aes(label = Master), force = 10, size = 3) +
  labs(x = "Castle Rate", y = "Win Rate") +
  theme_minimal()


ggpairs(tblRates[, -1])

dftest <- tblGames %>%
  head(100) %>%
  select(Moves) %>%
  mutate(MovesWhite = sapply(str_extract_all(Moves, "\\w+\\.\\w+(-\\w+)?"), paste, collapse = " "),
         MovesBlack = sapply(str_extract_all(Moves, "(?<=\\s)\\w+(?=\\s\\d+\\.)|O-O"), paste, collapse = " "),
         MovesBlack = str_replace(MovesBlack, "\\s*(1-0|1/2 - 1/2|0-1)\\s*$", ""),
         MovesBlack = strsplit(MovesBlack, "\\s+"),
         MovesBlack = purrr::map_chr(MovesBlack, function(move) paste0(seq_along(move), ".", move, collapse = " ")))


