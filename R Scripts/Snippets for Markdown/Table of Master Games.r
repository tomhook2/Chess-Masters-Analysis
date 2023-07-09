tblMasterGames <- tblGames %>%
  filter(FullName == params$Master)

tblMasterGames <- tblMasterGames[ , c("Event", "Date", "Master", "Opponent", "PiecesMaster", "EloMaster", "EloOpponent",
                                      "Result", "ECO", "Opening", "Moves")]
                          

tblMasterGames <- datatable(tblMasterGames, 
                            filter = 'top', 
                            extensions = 'Buttons',
                            options = list(
                              dom = 'Blfrtip',
                              #buttons = c('copy', 'csv', 'pdf', 'print'),
                              buttons = c(''),
                              lengthMenu = list(c(10, 25, 50, 100), c(10, 25, 50, 100))
                            )
                  )