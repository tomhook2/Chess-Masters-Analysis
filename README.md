 # Chess Masters Analysis
 Summary analysis on games played by chess masters.

 # Data Sources

 ### Games Data
 Downloaded from https://www.pgnmentor.com/files.html.   
 Player pgn files downloaded 20/06/2023 with data up to 31/12/2022.

 ### FIDE Players Database
 Downloaded from http://ratings.fide.com/download_lists.phtml.  
 27/06/2023 extract downloaded on 02/07/2023.

 ### Country Codes
 Downloaded from https://www.chessgames.com/alpha3.html.

 ### Opening References
 Downloaded from https://docs.google.com/spreadsheets/d/1CehtdBIt5cOkRy6mbgMJlLjvfK1StGsqiKVCkeh9uqQ/edit.  
 Credit to randywolf244 on Lichess.

 # Data Wrangling
 Most data wrangling was done in RStudio.

 1) Created FIDE ID Excel table with all masters with downloads available. Columm headers included Full Name, Master (Surname, first letter firstname) and FIDE ID (FIDE IDs.csv).
 2) Read FIDE players database and saved as RDS file (Titled Players.RDS).
 3) Created profiles table from FIDE ID file and combining with FIDE players database, and country codes (Profiles.csv).
 4) Combined games data downloads, filtered out poor data, and derived new columns.  
    Example of a single game of data below. This is turned into a single row within the table (Games.RDS).  
    ![image](https://github.com/tomhook2/Chess-Masters-Analysis/assets/119594235/af3c66a8-59c8-4f08-9361-ed0b3edfbf6e)  

# Report Production
R Markdown used to create profiles for each Master that has a download available. Profile pages found in "Profiles Folder".
