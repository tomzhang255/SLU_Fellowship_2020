library(tidyverse)

library(DBI)

# connect to db
con <- dbConnect(drv=RSQLite::SQLite(), dbname="clash_royale_season3/cr_season3_subset.sqlite")

# list all tables
tables <- dbListTables(con)

# exclude sqlite_sequence (contains table information)
tables <- tables[tables != "sqlite_sequence"]

cr_s3_list <- vector("list", length=length(tables))

# create a data.frame for each table
for (i in seq(along=tables)) {
  cr_s3_list[[i]] <- dbGetQuery(conn=con, statement=paste("SELECT * FROM '", tables[[i]], "'", sep=""))
}

# from list to data frame
library(tidyverse)
cr_s3_flat <- flatten(cr_s3_list)
cr_s3_df <- as_tibble(cr_s3_flat)

# disconnect from database
dbDisconnect(con)

# segment for presentation
df1 <- 
cr_s3_df %>%
  slice(1:10) %>%
  select(1, 2, 5:8, 10)



# long model
cr_s3_df_indLvl_left <- read_csv("data/cr_s3_df_indLvl_left.csv")

df2 <-
cr_s3_df_indLvl_left %>%
  slice(1:10) %>%
  select(26:29, 130:133)

df2a <-
  df2[,1:4]

df2b <-
  df2[,5:8]



# short model
cr_s3_df_ind_left <- read_csv("data/cr_s3_df_ind_left.csv")
df_mod1 <-
  cr_s3_df_ind_left %>%
  select(-1:-5) %>%
  select(-2:-20)
mod1 <- lm(leftstartingtrophies ~ ., data = df_mod1)
summary(mod1)
  










