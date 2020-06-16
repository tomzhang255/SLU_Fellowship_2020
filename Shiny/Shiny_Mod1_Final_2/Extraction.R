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



# build a function
extractCard <- function(tag) {
  playerTag <- str_remove(tag, "#")
  playerURL <- paste("https://royaleapi.com/player/", playerTag, "/", sep = "") # reactiveValue()
  cardImg <- html_nodes(read_html(playerURL), ".deck_card.image")
  
  playerCards <- NA
  for (index in 1:8) {
    playerCards[index] <- html_attrs(cardImg)[[index]][3]
  }
  
  levels <- html_nodes(read_html(playerURL), ".cardlevel")
  levels <- html_text(levels)
  levels <- parse_number(levels)
  
  return(tibble(playerCards, levels))
}



# test
t1 <- Sys.time()
extractCard("#9YJUPU9LY")
t2 <- Sys.time()
t2 - t1







