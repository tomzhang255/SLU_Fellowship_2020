library(DBI)
# Create an ephemeral in-memory RSQLite database
con <- dbConnect(RSQLite::SQLite(), "cr_season3_subset.sqlite")

dbListTables(con)

cr_20k_matches <- dbGetQuery(con, "SELECT * FROM clash_royale_s3")

# Disconnect from the database
dbDisconnect(con)
