library(rvest)
library(tidyverse)


playerTag <- "#9YJUPU9LY"
playerTag <- str_remove(playerTag, "#")
playerURL <- paste("https://royaleapi.com/player/", playerTag, "/", sep = "") # reactiveValue()

cardImg <- html_nodes(read_html(playerURL), ".deck_card.image")

card1 <- html_attrs(cardImg)[[1]][3]
names(card1) <- NULL
card1


cardLvl <- html_nodes(read_html(playerURL), ".cardlevel")
cardLvl <- html_text(cardLvl)
cardLvl <- parse_number(cardLvl)

card1Lvl <- cardLvl[1]
