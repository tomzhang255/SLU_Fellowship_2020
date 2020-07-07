library(shiny)
library(shinyMobile)
library(tidyverse)
library(shinyalert)
library(rhandsontable)
library(rvest)
library(plotly)

# the model
mod3 <- readRDS("mod_4000_s11_lm_2_strip.rds")
df_mod3 <- read_csv("df_mod3.csv")

# create empty data frame for predictions
newxEmpty <-
  df_mod3 %>%
  select(-2) %>%
  slice(1)

for (col in 2:ncol(newxEmpty)) {
  for (row in 1:nrow(newxEmpty)) {
    newxEmpty[row,col] = 0
  }
}

# card selection options
cards <-
  newxEmpty %>%
  select(ends_with("_Level")) %>%
  names(.) %>%
  str_remove(., "_Level") %>%
  str_replace(., "_", " ") %>%
  str_replace(., "X Bow", "X-Bow")

# for upgrade cost calculation
cr_cards_rarity <-
  jsonlite::fromJSON("clash_royale_card_info.json")$cards %>%
  select(name, rarity) %>%
  # season 1 added in Fisherman - nothing more up to season 3
  bind_rows(., tibble(name = "Fisherman", rarity = "Legendary")) %>%
  # season 4: elixir golem
  bind_rows(., tibble(name = "Elixir Golem", rarity = "Rare")) %>%
  # season 6: battle healer
  bind_rows(., tibble(name = "Battle Healer", rarity = "Rare")) %>%
  # season 7: firecracker
  bind_rows(., tibble(name = "Firecracker", rarity = "Common")) %>%
  # season 9: royal delivery
  bind_rows(., tibble(name = "Royal Delivery", rarity = "Common")) %>%
  # season 10: replaces heal with heal spirit
  mutate(name = str_replace(name, "^Heal$", "Heal Spirit"))

cr_upgrade_cost <- read_csv("cr_upgrade_cost.csv")

# calc function
costCalc <- function(cardName, currentLvl, upgradeLvl) {
  # extract rarity info
  cardRarity <-
    cr_cards_rarity %>%
    filter(name == cardName)
  cardRarity <- cardRarity$rarity
  
  # calculate cost
  if (upgradeLvl > currentLvl) {
    cost <-
      cr_upgrade_cost %>%
      select(contains(cardRarity)) %>%
      slice((currentLvl + 1):(upgradeLvl)) %>%
      sum(na.rm = T)
    return(cost)
  } else {
    return(0)
  }
}

# for card icons
cr_cards_s12 <- read_csv("cr_cards_s12.csv")
cr_cards_s12 <- 
  cr_cards_s12 %>%
  arrange(name)

cards_s12 <- cr_cards_s12$name

# web scraping to extract card info from player tag
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

# residual adjustment for different trophy groups
residAdj <- read_csv("residualAdj.csv")



# the mobile app
shiny::shinyApp(
  ui <- f7Page(
    title = "shinyMobile",
    init = f7Init(
      skin = "auto",
      theme = "light",
      #filled = T,
      #color = "red"
    ),
    f7TabLayout(
      navbar = f7Navbar(
        title = "Tabs",
        hairline = F,
        shadow = TRUE
      ),
      f7Tabs(
        animated = TRUE,
        id = "tabs",
        f7Tab(
          tabName = "Tab 1",
          icon = f7Icon("envelope"),
          active = TRUE,
          f7Card(
            f7Col(
              f7Text(inputId = "playerTag", label = "Load with Player Tag", value = ""),
              f7Button(inputId = "updateDeck", label = "Load Deck"),
              align = "center"
            )
          ),
          f7Card(
            f7Col(
              htmlOutput(outputId = "card1LSelectorv5")
            ),
            f7Col(
              f7Stepper(inputId = "step", label = NULL, min = 1, max = 13, value = 9, fill = T)
            ),
            align = "center"
          )
          
        )
      )
    )
  ),
  
  
  
  server <- function(input, output) {
    # reactive values
    values <- reactiveValues()
    
    values$playerDeck <- tibble(
      playerCards = c("Baby Dragon", "Goblin Hut", "Graveyard", "Knight", "The Log", "Musketeer", "Poison", "Guards"),
      levels = c(11, 11, 10, 11, 11, 13, 11, 13)
    )

    # reactive select boxes for left deck (can be pre-loaded from values$playerDeck <- playerTag)
    output$card1LSelectorv5 <- renderUI({
      f7SmartSelect(inputId = "card1Lv5", label = "Card 1", choices = cards, selected = values$playerDeck[[1,1]], openIn = "popup")
    })
  }
)














# f7SmartSelect(inputId = "card1", label = "Card 1", choices = cards, selected = cards[1], openIn = "popup", searchbar = TRUE),
# f7Stepper(inputId = "card1Lvl", label = NULL, min = 1, max = 13, value = 9, step = 1),






