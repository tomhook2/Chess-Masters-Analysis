# Define tables for all/white/black
tblOpenings <- tblGames
tblOpeningsWhite <- tblGames %>%
  filter(PiecesMaster == "White")
tblOpeningsBlack <- tblGames %>%
  filter(PiecesMaster == "Black")

# Create vector for the 3 tables
vecOpeningsTables <- c("tblOpenings", "tblOpeningsWhite", "tblOpeningsBlack")

for (i in seq_along(vecOpeningsTables)) {
  tblTemp <- get(vecOpeningsTables[i]) %>%
    filter(FullName == params$Master) %>%
    group_by(Opening) %>%
    summarise(count = n()) %>%
    arrange(desc(count)) %>%
    rename(`Game Count` = count) %>%
    slice(1:10) %>%
    gt() %>%
    tab_style(
      locations = cells_body(),  # Specify the locations where styling should be applied
      style = list(
        cell_text(size = px(12))  # Adjust the font size here (e.g., px(12))
      )
    )
  
  assign(vecOpeningsTables[i], tblTemp)  # Assign the modified data frame back to the original name
}