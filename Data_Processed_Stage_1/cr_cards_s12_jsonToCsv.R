library(jsonlite)
library(tidyverse)
cr_cards_list <- fromJSON("data/cr_cards_s12.json")
cr_cards_flat <- flatten(cr_cards_list)
iconUrls <- as_tibble(cr_cards_flat$iconUrls)
cr_cards_df <- bind_cols(as_tibble(cr_cards_flat[-4]), iconUrls)
names(cr_cards_df)[4] <- "iconUrl"


write_csv(cr_cards_df, "data/cr_cards_s12.csv")

cr_cards_df <- read_csv("data/cr_cards_s12.csv")
