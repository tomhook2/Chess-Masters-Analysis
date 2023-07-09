# Filter for current master and remove NAs to see if there's any Elo data.
tblEloTimeseries <- tblGames %>%
  filter(FullName == params$Master) %>%
  filter(!is.na(EloMaster))

if (nrow(tblEloTimeseries) == 0) {
  
  plotEloTimeseries <- paste("No Elo Data available for", Master)
  
} else {
  
  # Create Elo chart
  tblEloTimeseries <- tblEloTimeseries %>%
    mutate(Date = format(Date, "%Y-%m")) %>%
    group_by(Master, Date) %>%
    summarise(EloMaster = median(EloMaster)) %>%
    mutate(Date = as.Date(paste0(Date,"-01"))) %>%
    arrange(Date) %>%
    mutate(EloChange = abs((EloMaster - lag(EloMaster)) / lag(EloMaster))) %>%
    filter(is.na(EloChange) | EloChange <= 0.1)
  
  plotEloTimeseries <- ggplot(tblEloTimeseries, aes(x = Date, y = EloMaster)) +
    geom_line() +
    labs(title = paste("FIDE Rating over Time for", Master),
         y = "Elo") +
    theme_classic() +
    theme(panel.grid.major.y = element_line(color = "gray", linetype = "longdash", size = 0.5),
          panel.grid.minor = element_blank()) +
    scale_y_continuous(breaks = seq(0, max(tblEloTimeseries$EloMaster), by = 50))
  
}

