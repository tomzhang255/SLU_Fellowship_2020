library(shiny)
library(shinyMobile)
library(tidyverse)
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
      theme = "light"
    ),
    f7TabLayout(
      # panels = tagList(
      #   f7Panel(
      #     side = "left",
      #     theme = "light",
      #     effect = "cover",
      #     f7Card(
      #       f7Button(inputId = "viewInfo", label = "View Info")
      #     )
      #   )
      # ),
      navbar = f7Navbar(
        title = "CR Card-Upgrade Investigation",
        hairline = TRUE,
        shadow = TRUE#,
        #left_panel = TRUE
      ),
      f7Tabs(
        animated = TRUE,
        id = "tabs",
        f7Tab(
          tabName = "Deck 1",
          icon = f7Icon("layers_alt_fill"),
          f7Card(
            f7Col(
              f7Text(inputId = "playerTag", label = "Load with Player Tag", value = "#9YJUPU9LY", placeholder = "#9YJUPU9LY"),
              f7Button(inputId = "updateDeck", label = "Load Deck"),
              align = "center"
            )
          ),
          f7Card(
            htmlOutput(outputId = "card1LSelectorv5"),
            htmlOutput(outputId = "card1LvlLSelectorv5"),
            align = "center"
          ),
          f7Card(
            htmlOutput(outputId = "card2LSelectorv5"),
            htmlOutput(outputId = "card2LvlLSelectorv5"),
            align = "center"
          ),
          f7Card(
            htmlOutput(outputId = "card3LSelectorv5"),
            htmlOutput(outputId = "card3LvlLSelectorv5"),
            align = "center"
          ),
          f7Card(
            htmlOutput(outputId = "card4LSelectorv5"),
            htmlOutput(outputId = "card4LvlLSelectorv5"),
            align = "center"
          ),
          f7Card(
            htmlOutput(outputId = "card5LSelectorv5"),
            htmlOutput(outputId = "card5LvlLSelectorv5"),
            align = "center"
          ),
          f7Card(
            htmlOutput(outputId = "card6LSelectorv5"),
            htmlOutput(outputId = "card6LvlLSelectorv5"),
            align = "center"
          ),
          f7Card(
            htmlOutput(outputId = "card7LSelectorv5"),
            htmlOutput(outputId = "card7LvlLSelectorv5"),
            align = "center"
          ),
          f7Card(
            htmlOutput(outputId = "card8LSelectorv5"),
            htmlOutput(outputId = "card8LvlLSelectorv5"),
            align = "center"
          )
        ),
        f7Tab(
          tabName = "Deck 2",
          icon = f7Icon("layers_alt_fill"),
          f7Card(
            htmlOutput(outputId = "card1RSelectorv5"),
            htmlOutput(outputId = "card1LvlRSelectorv5"),
            align = "center"
          ),
          f7Card(
            htmlOutput(outputId = "card2RSelectorv5"),
            htmlOutput(outputId = "card2LvlRSelectorv5"),
            align = "center"
          ),
          f7Card(
            htmlOutput(outputId = "card3RSelectorv5"),
            htmlOutput(outputId = "card3LvlRSelectorv5"),
            align = "center"
          ),
          f7Card(
            htmlOutput(outputId = "card4RSelectorv5"),
            htmlOutput(outputId = "card4LvlRSelectorv5"),
            align = "center"
          ),
          f7Card(
            htmlOutput(outputId = "card5RSelectorv5"),
            htmlOutput(outputId = "card5LvlRSelectorv5"),
            align = "center"
          ),
          f7Card(
            htmlOutput(outputId = "card6RSelectorv5"),
            htmlOutput(outputId = "card6LvlRSelectorv5"),
            align = "center"
          ),
          f7Card(
            htmlOutput(outputId = "card7RSelectorv5"),
            htmlOutput(outputId = "card7LvlRSelectorv5"),
            align = "center"
          ),
          f7Card(
            htmlOutput(outputId = "card8RSelectorv5"),
            htmlOutput(outputId = "card8LvlRSelectorv5"),
            align = "center"
          )
        ),
        f7Tab(
          tabName = "Decks",
          icon = f7Icon("tray_2_fill"),
          f7Card(
            title = "Deck 1",
            f7Row(
              f7Col(uiOutput(outputId = "card1Icon")),
              f7Col(uiOutput(outputId = "card2Icon")),
              f7Col(uiOutput(outputId = "card3Icon")),
              f7Col(uiOutput(outputId = "card4Icon"))
            ),
            f7Row(
              f7Col(textOutput(outputId = "card1IconLvl"), align = "center"),
              f7Col(textOutput(outputId = "card2IconLvl"), align = "center"),
              f7Col(textOutput(outputId = "card3IconLvl"), align = "center"),
              f7Col(textOutput(outputId = "card4IconLvl"), align = "center")
            ),
            f7Row(
              f7Col(uiOutput(outputId = "card5Icon")),
              f7Col(uiOutput(outputId = "card6Icon")),
              f7Col(uiOutput(outputId = "card7Icon")),
              f7Col(uiOutput(outputId = "card8Icon"))
            ),
            f7Row(
              f7Col(textOutput(outputId = "card5IconLvl"), align = "center"),
              f7Col(textOutput(outputId = "card6IconLvl"), align = "center"),
              f7Col(textOutput(outputId = "card7IconLvl"), align = "center"),
              f7Col(textOutput(outputId = "card8IconLvl"), align = "center")
            )
          ),
          f7Card(
            title = "Deck 2",
            f7Row(
              f7Col(uiOutput(outputId = "card1IconD2")),
              f7Col(uiOutput(outputId = "card2IconD2")),
              f7Col(uiOutput(outputId = "card3IconD2")),
              f7Col(uiOutput(outputId = "card4IconD2"))
            ),
            f7Row(
              f7Col(textOutput(outputId = "card1IconLvlD2"), align = "center"),
              f7Col(textOutput(outputId = "card2IconLvlD2"), align = "center"),
              f7Col(textOutput(outputId = "card3IconLvlD2"), align = "center"),
              f7Col(textOutput(outputId = "card4IconLvlD2"), align = "center")
            ),
            f7Row(
              f7Col(uiOutput(outputId = "card5IconD2")),
              f7Col(uiOutput(outputId = "card6IconD2")),
              f7Col(uiOutput(outputId = "card7IconD2")),
              f7Col(uiOutput(outputId = "card8IconD2"))
            ),
            f7Row(
              f7Col(textOutput(outputId = "card5IconLvlD2"), align = "center"),
              f7Col(textOutput(outputId = "card6IconLvlD2"), align = "center"),
              f7Col(textOutput(outputId = "card7IconLvlD2"), align = "center"),
              f7Col(textOutput(outputId = "card8IconLvlD2"), align = "center")
            )
          )
        ),
        f7Tab(
          tabName = "Compare",
          icon = f7Icon("arrow_right_arrow_left_square_fill"),
          # chart_bar_alt_fill
          # arrow_right_arrow_left_square_fill
          
          
        ),
        f7Tab(
          tabName = "Route",
          icon = f7Icon("map_fill"),
          
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
    
    
    
    # update deck button
    observeEvent(input$updateDeck, {
      # alert
      f7Notif(
        icon = f7Icon("hourglass_tophalf_fill"),
        title = "Notification",
        subtitle = "Please Wait",
        text = "It may take a few seconds",
        closeTimeout = 3500
      )
      
      values$playerDeck <- extractCard(input$playerTag)
    })
    
    
    
    # reactive select boxes for left deck (can be pre-loaded from values$playerDeck <- playerTag)
    output$card1LSelectorv5 <- renderUI({
      f7SmartSelect(inputId = "card1Lv5", label = "Card 1", choices = cards, selected = values$playerDeck[[1,1]], openIn = "popup")
    })
    
    output$card1LvlLSelectorv5 <- renderUI({
      f7Stepper(inputId = "card1LvlLv5", label = NULL, min = 1, max = 13, value = values$playerDeck[[1,2]], fill = TRUE)
    })
    
    output$card2LSelectorv5 <- renderUI({
      f7SmartSelect(inputId = "card2Lv5", label = "Card 2", choices = cards, selected = values$playerDeck[[2,1]], openIn = "popup")
    })
    
    output$card2LvlLSelectorv5 <- renderUI({
      f7Stepper(inputId = "card2LvlLv5", label = NULL, min = 1, max = 13, value = values$playerDeck[[2,2]], fill = TRUE)
    })
    
    output$card3LSelectorv5 <- renderUI({
      f7SmartSelect(inputId = "card3Lv5", label = "Card 3", choices = cards, selected = values$playerDeck[[3,1]], openIn = "popup")
    })
    
    output$card3LvlLSelectorv5 <- renderUI({
      f7Stepper(inputId = "card3LvlLv5", label = NULL, min = 1, max = 13, value = values$playerDeck[[3,2]], fill = TRUE)
    })
    
    output$card4LSelectorv5 <- renderUI({
      f7SmartSelect(inputId = "card4Lv5", label = "Card 4", choices = cards, selected = values$playerDeck[[4,1]], openIn = "popup")
    })
    
    output$card4LvlLSelectorv5 <- renderUI({
      f7Stepper(inputId = "card4LvlLv5", label = NULL, min = 1, max = 13, value = values$playerDeck[[4,2]], fill = TRUE)
    })
    
    output$card5LSelectorv5 <- renderUI({
      f7SmartSelect(inputId = "card5Lv5", label = "Card 5", choices = cards, selected = values$playerDeck[[5,1]], openIn = "popup")
    })
    
    output$card5LvlLSelectorv5 <- renderUI({
      f7Stepper(inputId = "card5LvlLv5", label = NULL, min = 1, max = 13, value = values$playerDeck[[5,2]], fill = TRUE)
    })
    
    output$card6LSelectorv5 <- renderUI({
      f7SmartSelect(inputId = "card6Lv5", label = "Card 6", choices = cards, selected = values$playerDeck[[6,1]], openIn = "popup")
    })
    
    output$card6LvlLSelectorv5 <- renderUI({
      f7Stepper(inputId = "card6LvlLv5", label = NULL, min = 1, max = 13, value = values$playerDeck[[6,2]], fill = TRUE)
    })
    
    output$card7LSelectorv5 <- renderUI({
      f7SmartSelect(inputId = "card7Lv5", label = "Card 7", choices = cards, selected = values$playerDeck[[7,1]], openIn = "popup")
    })
    
    output$card7LvlLSelectorv5 <- renderUI({
      f7Stepper(inputId = "card7LvlLv5", label = NULL, min = 1, max = 13, value = values$playerDeck[[7,2]], fill = TRUE)
    })
    
    output$card8LSelectorv5 <- renderUI({
      f7SmartSelect(inputId = "card8Lv5", label = "Card 8", choices = cards, selected = values$playerDeck[[8,1]], openIn = "popup")
    })
    
    output$card8LvlLSelectorv5 <- renderUI({
      f7Stepper(inputId = "card8LvlLv5", label = NULL, min = 1, max = 13, value = values$playerDeck[[8,2]], fill = TRUE)
    })
    
    
    
    # reactive select boxes for right deck
    output$card1RSelectorv5 <- renderUI({
      values$playerDeck
      f7SmartSelect(inputId = "card1Rv5", label = "Card 1", choices = cards, selected = input$card1Lv5, openIn = "popup")
    })
    
    output$card1LvlRSelectorv5 <- renderUI({
      values$playerDeck
      f7Stepper(inputId = "card1LvlRv5", label = NULL, min = 1, max = 13, value = input$card1LvlLv5, fill = TRUE)
    })
    
    output$card2RSelectorv5 <- renderUI({
      values$playerDeck
      f7SmartSelect(inputId = "card2Rv5", label = "Card 2", choices = cards, selected = input$card2Lv5, openIn = "popup")
    })
    
    output$card2LvlRSelectorv5 <- renderUI({
      values$playerDeck
      f7Stepper(inputId = "card2LvlRv5", label = NULL, min = 1, max = 13, value = input$card2LvlLv5, fill = TRUE)
    })
    
    output$card3RSelectorv5 <- renderUI({
      values$playerDeck
      f7SmartSelect(inputId = "card3Rv5", label = "Card 3", choices = cards, selected = input$card3Lv5, openIn = "popup")
    })
    
    output$card3LvlRSelectorv5 <- renderUI({
      values$playerDeck
      f7Stepper(inputId = "card3LvlRv5", label = NULL, min = 1, max = 13, value = input$card3LvlLv5, fill = TRUE)
    })
    
    output$card4RSelectorv5 <- renderUI({
      values$playerDeck
      f7SmartSelect(inputId = "card4Rv5", label = "Card 4", choices = cards, selected = input$card4Lv5, openIn = "popup")
    })
    
    output$card4LvlRSelectorv5 <- renderUI({
      values$playerDeck
      f7Stepper(inputId = "card4LvlRv5", label = NULL, min = 1, max = 13, value = input$card4LvlLv5, fill = TRUE)
    })
    
    output$card5RSelectorv5 <- renderUI({
      values$playerDeck
      f7SmartSelect(inputId = "card5Rv5", label = "Card 5", choices = cards, selected = input$card5Lv5, openIn = "popup")
    })
    
    output$card5LvlRSelectorv5 <- renderUI({
      values$playerDeck
      f7Stepper(inputId = "card5LvlRv5", label = NULL, min = 1, max = 13, value = input$card5LvlLv5, fill = TRUE)
    })
    
    output$card6RSelectorv5 <- renderUI({
      values$playerDeck
      f7SmartSelect(inputId = "card6Rv5", label = "Card 6", choices = cards, selected = input$card6Lv5, openIn = "popup")
    })
    
    output$card6LvlRSelectorv5 <- renderUI({
      values$playerDeck
      f7Stepper(inputId = "card6LvlRv5", label = NULL, min = 1, max = 13, value = input$card6LvlLv5, fill = TRUE)
    })
    
    output$card7RSelectorv5 <- renderUI({
      values$playerDeck
      f7SmartSelect(inputId = "card7Rv5", label = "Card 7", choices = cards, selected = input$card7Lv5, openIn = "popup")
    })
    
    output$card7LvlRSelectorv5 <- renderUI({
      values$playerDeck
      f7Stepper(inputId = "card7LvlRv5", label = NULL, min = 1, max = 13, value = input$card7LvlLv5, fill = TRUE)
    })
    
    output$card8RSelectorv5 <- renderUI({
      values$playerDeck
      f7SmartSelect(inputId = "card8Rv5", label = "Card 8", choices = cards, selected = input$card8Lv5, openIn = "popup")
    })
    
    output$card8LvlRSelectorv5 <- renderUI({
      values$playerDeck
      f7Stepper(inputId = "card8LvlRv5", label = NULL, min = 1, max = 13, value = input$card8LvlLv5, fill = TRUE)
    })
    
    
    
    # for card icons of left deck
    output$card1Icon <- renderUI({
      for (icon1 in 1:length(cards_s12)) {
        if (cards_s12[icon1] == input$card1Lv5) {
          icon1URL <-  cr_cards_s12$iconUrl[icon1]
        }
      }
      
      tags$img(src = icon1URL, width = "100%", height = "auto")
    })
    
    output$card2Icon <- renderUI({
      for (icon2 in 1:length(cards_s12)) {
        if (cards_s12[icon2] == input$card2Lv5) {
          icon2URL <-  cr_cards_s12$iconUrl[icon2]
        }
      }
      
      tags$img(src = icon2URL, width = "100%", height = "auto")
    })
    
    output$card3Icon <- renderUI({
      for (icon3 in 1:length(cards_s12)) {
        if (cards_s12[icon3] == input$card3Lv5) {
          icon3URL <-  cr_cards_s12$iconUrl[icon3]
        }
      }
      
      tags$img(src = icon3URL, width = "100%", height = "auto")
    })
    
    output$card4Icon <- renderUI({
      for (icon4 in 1:length(cards_s12)) {
        if (cards_s12[icon4] == input$card4Lv5) {
          icon4URL <-  cr_cards_s12$iconUrl[icon4]
        }
      }
      
      tags$img(src = icon4URL, width = "100%", height = "auto")
    })
    
    output$card5Icon <- renderUI({
      for (icon5 in 1:length(cards_s12)) {
        if (cards_s12[icon5] == input$card5Lv5) {
          icon5URL <-  cr_cards_s12$iconUrl[icon5]
        }
      }
      
      tags$img(src = icon5URL, width = "100%", height = "auto")
    })
    
    output$card6Icon <- renderUI({
      for (icon6 in 1:length(cards_s12)) {
        if (cards_s12[icon6] == input$card6Lv5) {
          icon6URL <-  cr_cards_s12$iconUrl[icon6]
        }
      }
      
      tags$img(src = icon6URL, width = "100%", height = "auto")
    })
    
    output$card7Icon <- renderUI({
      for (icon7 in 1:length(cards_s12)) {
        if (cards_s12[icon7] == input$card7Lv5) {
          icon7URL <-  cr_cards_s12$iconUrl[icon7]
        }
      }
      
      tags$img(src = icon7URL, width = "100%", height = "auto")
    })
    
    output$card8Icon <- renderUI({
      for (icon8 in 1:length(cards_s12)) {
        if (cards_s12[icon8] == input$card8Lv5) {
          icon8URL <-  cr_cards_s12$iconUrl[icon8]
        }
      }
      
      tags$img(src = icon8URL, width = "100%", height = "auto")
    })
    
    
    
    # for card icons of right deck
    output$card1IconD2 <- renderUI({
      for (icon1D2 in 1:length(cards_s12)) {
        if (cards_s12[icon1D2] == input$card1Rv5) {
          icon1URLD2 <-  cr_cards_s12$iconUrl[icon1D2]
        }
      }
      
      tags$img(src = icon1URLD2, width = "100%", height = "auto")
    })
    
    output$card2IconD2 <- renderUI({
      for (icon2D2 in 1:length(cards_s12)) {
        if (cards_s12[icon2D2] == input$card2Rv5) {
          icon2URLD2 <-  cr_cards_s12$iconUrl[icon2D2]
        }
      }
      
      tags$img(src = icon2URLD2, width = "100%", height = "auto")
    })
    
    output$card3IconD2 <- renderUI({
      for (icon3D2 in 1:length(cards_s12)) {
        if (cards_s12[icon3D2] == input$card3Rv5) {
          icon3URLD2 <-  cr_cards_s12$iconUrl[icon3D2]
        }
      }
      
      tags$img(src = icon3URLD2, width = "100%", height = "auto")
    })
    
    output$card4IconD2 <- renderUI({
      for (icon4D2 in 1:length(cards_s12)) {
        if (cards_s12[icon4D2] == input$card4Rv5) {
          icon4URLD2 <-  cr_cards_s12$iconUrl[icon4D2]
        }
      }
      
      tags$img(src = icon4URLD2, width = "100%", height = "auto")
    })
    
    output$card5IconD2 <- renderUI({
      for (icon5D2 in 1:length(cards_s12)) {
        if (cards_s12[icon5D2] == input$card5Rv5) {
          icon5URLD2 <-  cr_cards_s12$iconUrl[icon5D2]
        }
      }
      
      tags$img(src = icon5URLD2, width = "100%", height = "auto")
    })
    
    output$card6IconD2 <- renderUI({
      for (icon6D2 in 1:length(cards_s12)) {
        if (cards_s12[icon6D2] == input$card6Rv5) {
          icon6URLD2 <-  cr_cards_s12$iconUrl[icon6D2]
        }
      }
      
      tags$img(src = icon6URLD2, width = "100%", height = "auto")
    })
    
    output$card7IconD2 <- renderUI({
      for (icon7D2 in 1:length(cards_s12)) {
        if (cards_s12[icon7D2] == input$card7Rv5) {
          icon7URLD2 <-  cr_cards_s12$iconUrl[icon7D2]
        }
      }
      
      tags$img(src = icon7URLD2, width = "100%", height = "auto")
    })
    
    output$card8IconD2 <- renderUI({
      for (icon8D2 in 1:length(cards_s12)) {
        if (cards_s12[icon8D2] == input$card8Rv5) {
          icon8URLD2 <-  cr_cards_s12$iconUrl[icon8D2]
        }
      }
      
      tags$img(src = icon8URLD2, width = "100%", height = "auto")
    })
    
    
    
    # for card icon levels of left deck
    output$card1IconLvl <- renderText({
      paste("lvl", input$card1LvlLv5)
    })
    
    output$card2IconLvl <- renderText({
      paste("lvl", input$card2LvlLv5)
    })
    
    output$card3IconLvl <- renderText({
      paste("lvl", input$card3LvlLv5)
    })
    
    output$card4IconLvl <- renderText({
      paste("lvl", input$card4LvlLv5)
    })
    
    output$card5IconLvl <- renderText({
      paste("lvl", input$card5LvlLv5)
    })
    
    output$card6IconLvl <- renderText({
      paste("lvl", input$card6LvlLv5)
    })
    
    output$card7IconLvl <- renderText({
      paste("lvl", input$card7LvlLv5)
    })
    
    output$card8IconLvl <- renderText({
      paste("lvl", input$card8LvlLv5)
    })
    
    
    
    # for card icon levels of right deck
    output$card1IconLvlD2 <- renderText({
      paste("lvl", input$card1LvlRv5)
    })
    
    output$card2IconLvlD2 <- renderText({
      paste("lvl", input$card2LvlRv5)
    })
    
    output$card3IconLvlD2 <- renderText({
      paste("lvl", input$card3LvlRv5)
    })
    
    output$card4IconLvlD2 <- renderText({
      paste("lvl", input$card4LvlRv5)
    })
    
    output$card5IconLvlD2 <- renderText({
      paste("lvl", input$card5LvlRv5)
    })
    
    output$card6IconLvlD2 <- renderText({
      paste("lvl", input$card6LvlRv5)
    })
    
    output$card7IconLvlD2 <- renderText({
      paste("lvl", input$card7LvlRv5)
    })
    
    output$card8IconLvlD2 <- renderText({
      paste("lvl", input$card8LvlRv5)
    })
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  }
)



