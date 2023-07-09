# Define tables for all/white/black
tblWinRate <- tblGames
tblWinRateWhite <- tblGames %>%
 filter(PiecesMaster == "White")
tblWinRateBlack <- tblGames %>%
  filter(PiecesMaster == "Black")

# Create vector for the 3 tables
vecWinRateTables <- c("tblWinRate", "tblWinRateWhite", "tblWinRateBlack")


for (i in seq_along(vecWinRateTables)) {
  tblTemp <- get(vecWinRateTables[i])  # Get the data frame using the name
  tblTemp <- tblTemp %>%
    filter(FullName == "Michael Adams") %>%
    mutate(Result = case_when(
      Result == "1-0" ~ "Win",
      Result == "0-1" ~ "Loss",
      Result == "1/2-1/2" ~ "Draw",
      TRUE ~ Result
    )) %>%
    group_by(Result) %>%
    summarise(Count = n()) %>%
    mutate(Percentage = (Count / sum(Count)) * 100) %>%
    mutate(Result = factor(Result, levels = c("Win", "Draw", "Loss")))
  
  assign(vecWinRateTables[i], tblTemp)  # Assign the modified data frame back to the original name
  
  # Create the plot for each table
  plotName <- paste0("plot", sub("^tbl", "", vecWinRateTables[i], ignore.case = TRUE))
  
  assign(plotName, ggplot(get(vecWinRateTables[i]), aes(x = "", y = Count, fill = Result)) +
           geom_bar(stat = "identity", width = 1, color = "white") +
           coord_polar("y", start = 0) +
           labs(fill = "Result", x = NULL, y = NULL) +
           scale_fill_manual(values = c("Win" = "blue", "Draw" = "grey", "Loss" = "orange")) +
           theme_minimal() +
           theme(legend.position = "right") +
           geom_text(aes(label = paste0(Count, " (", round(Percentage), "%)")),
                     position = position_stack(vjust = 0.5),
                     size = 4, color = "white"))
  
}