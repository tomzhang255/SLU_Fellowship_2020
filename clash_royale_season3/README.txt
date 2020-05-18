cr_season3_subset.sqlite
- Contains 20000 match results (on ladder) for Season 3. Data is a bit old, but format should be the same. Left vs Right player/deck is simply an internal way to keep track of the player. It has no real meaning other than that.

cr_import_data.r
- A short script showing how to load the SQLite databse into R. For more information on SQL, check out https://db.rstudio.com/ 


clash_royale_card_info.json
- A JSON formatted file containing some basic card information. (Note: This is a bit out of date and contains info from prior to the releae of Fisherman. Also, I can't recall where I found this file, so we might need to find a new source.)

json_examples.rmd
- a file from my STAT 3007 in Fall 2019 showing how to load simple JSON formatted data.

cr_rarity_level_adjustments.xlsx
- An Excel file containing infomation on how to adjust the levels in the match data to what we are used to seeing. Check out https://clashfordummies.com/2018/09/02/new-card-level-system-explained-in-clash-royale/ for a few details on the old vs current system.
- This will be needed when we bring levels into any model/analysis