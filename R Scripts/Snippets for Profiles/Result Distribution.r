# Define tables for all/white/black
tblResults <- tblGames
tblResultsWhite <- tblGames %>%
  filter(PiecesMaster == "White")
tblResultsBlack <- tblGames %>%
  filter(PiecesMaster == "Black")

# Create vector for the 3 tables
vecResultsTables <- c("tblResults", "tblResultsWhite", "tblResultsBlack")


for (i in seq_along(vecResultsTables)) {
  tblTemp <- get(vecResultsTables[i]) %>%
    filter(FullName == params$Master) %>%
    group_by(ResultMaster) %>%
    summarise(Count = n()) %>%
    mutate(Percentage = (Count / sum(Count)) * 100) %>%
    mutate(ResultMaster = factor(ResultMaster, levels = c("Win", "Draw", "Loss")))
  
  assign(vecResultsTables[i], tblTemp) 
  
  # Create the plot for each table
  plotName <- paste0("plot", sub("^tbl", "", vecResultsTables[i], ignore.case = TRUE))
  
  assign(plotName, ggplot(get(vecResultsTables[i]), aes(x = "", y = Count, fill = ResultMaster)) +
           geom_bar(stat = "identity", width = 1, color = "white") +
           coord_polar("y", start = 0) +
           labs(fill = "Result", x = NULL, y = NULL) +
           scale_fill_manual(values = c("Win" = "blue", "Draw" = "grey", "Loss" = "orange")) +
           theme_minimal() +
           theme(legend.position = "right",
                 axis.text = element_blank(),
                 axis.ticks = element_blank(),
                 panel.grid  = element_blank()) +
           geom_text(aes(label = paste0(Count, " (", round(Percentage), "%)")),
                     position = position_stack(vjust = 0.5),
                     size = 4, color = "white"))
  
}