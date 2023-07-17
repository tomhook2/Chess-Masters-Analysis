tblMasterGames <- tblGames %>%
  filter(FullName == params$Master)

tblMasterGames <- tblMasterGames[ , c("Master", "Date", "Event", "Opponent", "PiecesMaster", "EloMaster", "EloOpponent",
                                      "ResultMaster", "ECO", "Opening", "Moves")]
                          

dtMasterGames <- datatable(tblMasterGames,
                            colnames = c("Master", "Date", "Event", "Opponent", "Master Pieces", "Master Elo", "Opponent Elo",
                                         "Master Result", "ECO", "Opening", "Moves"),
                            filter = 'top', 
                            extensions = 'Buttons',
                            options = list(
                              dom = 'Blfrtip',
                              buttons = c('copy', 'csv', 'print'),
                              #buttons = c(''),
                              lengthMenu = list(c(10, 25, 50, 100), c(10, 25, 50, 100))
                            )
                  )
