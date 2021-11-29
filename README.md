# hand-check

This repo contains the code for a working paper on the inpact of changing the hand-check rule in the NBA prior to the 2004-2005 season. All data used for this project may be found in the compressed file data.zip, with the exception of the shooting data (the file is too large), but code to request the same data from the API used in this paper is included. Decriptions of the python and R scripts are listed below.

* **nba_scraper.py** scrapes injury reports for NBA players from prosportstransactions.com 
* **nba_cleaner.py** cleans and merges data, and creates several new variables
* **nba_textual_analysis.py** text mines the injury reports, finds relevants key words, and creates some cute little word clouds
* **nba_shooting_api.py** requests shooting data
* **nba.R** performs the econometric analyses and generates figures for the paper
