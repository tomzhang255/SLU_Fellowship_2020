library(shiny)
library(shinydashboard)
library(tidyverse)
library(shinyalert)
library(rhandsontable)

# the model
cr_s3_df_ind_left <- read_csv("cr_s3_df_ind_left.csv")

df_mod1 <-
  cr_s3_df_ind_left %>%
  select(-1:-5) %>%
  select(-2:-20)

mod1 <- lm(leftstartingtrophies ~ ., data = df_mod1)

# create empty data frame for predictions
newxEmpty <-
  df_mod1 %>%
  select(-1) %>%
  slice(1)

for (col in 1:ncol(newxEmpty)) {
  for (row in 1:nrow(newxEmpty)) {
    newxEmpty[row,col] = 0
  }
}

# card selection options
cards <-
  newxEmpty %>%
  select(!ends_with(" Lvl")) %>%
  names(.)

# for upgrade cost calculation
cr_cards_rarity <- read_csv("cr_cards_rarity.csv")
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



# the app
ui <- dashboardPage(
  dashboardHeader(title = "Model 1"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Version 5", tabName = "v5", icon = icon("file-code")),
      menuItem("Version 4", tabName = "v4", icon = icon("file-code")),
      menuItem("Version 3", tabName = "v3", icon = icon("file-code")),
      menuItem("Version 2", tabName = "v2", icon = icon("file-code")),
      menuItem("Version 1", tabName = "v1", icon = icon("file-code"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # v1 tab content
      tabItem(
        tabName = "v1",
        fluidRow(
          column(
            12,
            tags$h1("\tPredicting Trophies (Clash Royale Season 3)"),
            tags$h2("\tWith Linear Regression Model Based on Card Indicators and Card Levels"),
          )
        ),
        tags$hr(),
        fluidRow(
          column(3, selectInput(inputId = "card1", label = "Select Card 1", choices = cards, selected = "Baby Dragon")),
          column(3, selectInput(inputId = "card2", label = "Select Card 2", choices = cards, selected = "Goblin Hut")),
          column(3, selectInput(inputId = "card3", label = "Select Card 3", choices = cards, selected = "Graveyard")),
          column(3, selectInput(inputId = "card4", label = "Select Card 4", choices = cards, selected = "Knight"))
        ),
        fluidRow(
          column(3, sliderInput(inputId = "card1Lvl", label = "Card 1 Level", min = 1, max = 13, value = 11)),
          column(3, sliderInput(inputId = "card2Lvl", label = "Card 2 Level", min = 1, max = 13, value = 11)),
          column(3, sliderInput(inputId = "card3Lvl", label = "Card 3 Level", min = 1, max = 13, value = 10)),
          column(3, sliderInput(inputId = "card4Lvl", label = "Card 4 Level", min = 1, max = 13, value = 11))
        ),
        tags$hr(),
        fluidRow(
          column(3, selectInput(inputId = "card5", label = "Select Card 5", choices = cards, selected = "The Log")),
          column(3, selectInput(inputId = "card6", label = "Select Card 6", choices = cards, selected = "Musketeer")),
          column(3, selectInput(inputId = "card7", label = "Select Card 7", choices = cards, selected = "Poison")),
          column(3, selectInput(inputId = "card8", label = "Select Card 8", choices = cards, selected = "Skeletons"))
        ),
        fluidRow(
          column(3, sliderInput(inputId = "card5Lvl", label = "Card 5 Level", min = 1, max = 13, value = 11)),
          column(3, sliderInput(inputId = "card6Lvl", label = "Card 6 Level", min = 1, max = 13, value = 13)),
          column(3, sliderInput(inputId = "card7Lvl", label = "Card 7 Level", min = 1, max = 13, value = 11)),
          column(3, sliderInput(inputId = "card8Lvl", label = "Card 8 Level", min = 1, max = 13, value = 10))
        ),
        tags$hr(),
        fluidRow(
          column(3, tags$h4(textOutput(outputId = "prediction")), offset = 5)
        )
      ),
      
      
      
      # v2 tab tab content
      tabItem(
        tabName = "v2",
        fluidRow(
          column(
            12,
            tags$h1("\tPredicting Trophies (Clash Royale Season 3)"),
            tags$h2("\tWith Linear Regression Model Based on Card Indicators and Card Levels"),
          )
        ),
        tags$hr(),
        fluidRow(
          column(2,
                 fluidRow(column(12, selectInput(inputId = "card1v2", label = "Select Card 1", choices = cards, selected = "Baby Dragon"))),
                 fluidRow(
                   column(6, numericInput(inputId = "card1LvlC", label = "Current Lvl", min = 1, max = 13, value = 11)),
                   column(6, numericInput(inputId = "card1LvlU", label = "Upgraded", min = 1, max = 13, value = 11))
                 ),
                 tags$hr(),
                 fluidRow(column(12, selectInput(inputId = "card5v2", label = "Select Card 5", choices = cards, selected = "The Log"))),
                 fluidRow(
                   column(6, numericInput(inputId = "card5LvlC", label = "Current Lvl", min = 1, max = 13, value = 11)),
                   column(6, numericInput(inputId = "card5LvlU", label = "Upgraded", min = 1, max = 13, value = 11))
                 )
          ),
          column(2,
                 fluidRow(column(12, selectInput(inputId = "card2v2", label = "Select Card 2", choices = cards, selected = "Goblin Hut"))),
                 fluidRow(
                   column(6, numericInput(inputId = "card2LvlC", label = "Current Lvl", min = 1, max = 13, value = 11)),
                   column(6, numericInput(inputId = "card2LvlU", label = "Upgraded", min = 1, max = 13, value = 11))
                 ),
                 tags$hr(),
                 fluidRow(column(12, selectInput(inputId = "card6v2", label = "Select Card 6", choices = cards, selected = "Musketeer"))),
                 fluidRow(
                   column(6, numericInput(inputId = "card6LvlC", label = "Current Lvl", min = 1, max = 13, value = 13)),
                   column(6, numericInput(inputId = "card6LvlU", label = "Upgraded", min = 1, max = 13, value = 13))
                 )
          ),
          column(2,
                 fluidRow(column(12, selectInput(inputId = "card3v2", label = "Select Card 3", choices = cards, selected = "Graveyard"))),
                 fluidRow(
                   column(6, numericInput(inputId = "card3LvlC", label = "Current Lvl", min = 1, max = 13, value = 10)),
                   column(6, numericInput(inputId = "card3LvlU", label = "Upgraded", min = 1, max = 13, value = 10))
                 ),
                 tags$hr(),
                 fluidRow(column(12, selectInput(inputId = "card7v2", label = "Select Card 7", choices = cards, selected = "Poison"))),
                 fluidRow(
                   column(6, numericInput(inputId = "card7LvlC", label = "Current Lvl", min = 1, max = 13, value = 11)),
                   column(6, numericInput(inputId = "card7LvlU", label = "Upgraded", min = 1, max = 13, value = 11))
                 )
          ),
          column(2,
                 fluidRow(column(12, selectInput(inputId = "card4v2", label = "Select Card 4", choices = cards, selected = "Knight"))),
                 fluidRow(
                   column(6, numericInput(inputId = "card4LvlC", label = "Current Lvl", min = 1, max = 13, value = 11)),
                   column(6, numericInput(inputId = "card4LvlU", label = "Upgraded", min = 1, max = 13, value = 11))
                 ),
                 tags$hr(),
                 fluidRow(column(12, selectInput(inputId = "card8v2", label = "Select Card 8", choices = cards, selected = "Skeletons"))),
                 fluidRow(
                   column(6, numericInput(inputId = "card8LvlC", label = "Current Lvl", min = 1, max = 13, value = 10)),
                   column(6, numericInput(inputId = "card8LvlU", label = "Upgraded", min = 1, max = 13, value = 10))
                 )
          ),
          column(4, plotOutput(outputId = "bar"))
        )
      ),
      
      
      
      # v3 tab content
      tabItem(
        tabName = "v3",
        fluidRow(
          column(
            12,
            tags$h1("\tPredicting Trophies (Clash Royale Season 3)"),
            tags$h2("\tWith Linear Regression Model Based on Card Indicators and Card Levels"),
          )
        ),
        tags$hr(),
        fluidRow(
          column(2,
                 fluidRow(column(12, selectInput(inputId = "card1v3", label = "Select Card 1", choices = cards, selected = "Baby Dragon"))),
                 fluidRow(
                   column(6, numericInput(inputId = "card1LvlCv3", label = "Current Lvl", min = 1, max = 13, value = 11)),
                   column(6, numericInput(inputId = "card1LvlUv3", label = "Upgraded", min = 1, max = 13, value = 11))
                 ),
                 tags$hr(),
                 fluidRow(column(12, selectInput(inputId = "card5v3", label = "Select Card 5", choices = cards, selected = "The Log"))),
                 fluidRow(
                   column(6, numericInput(inputId = "card5LvlCv3", label = "Current Lvl", min = 1, max = 13, value = 11)),
                   column(6, numericInput(inputId = "card5LvlUv3", label = "Upgraded", min = 1, max = 13, value = 11))
                 )
          ),
          column(2,
                 fluidRow(column(12, selectInput(inputId = "card2v3", label = "Select Card 2", choices = cards, selected = "Goblin Hut"))),
                 fluidRow(
                   column(6, numericInput(inputId = "card2LvlCv3", label = "Current Lvl", min = 1, max = 13, value = 11)),
                   column(6, numericInput(inputId = "card2LvlUv3", label = "Upgraded", min = 1, max = 13, value = 11))
                 ),
                 tags$hr(),
                 fluidRow(column(12, selectInput(inputId = "card6v3", label = "Select Card 6", choices = cards, selected = "Musketeer"))),
                 fluidRow(
                   column(6, numericInput(inputId = "card6LvlCv3", label = "Current Lvl", min = 1, max = 13, value = 13)),
                   column(6, numericInput(inputId = "card6LvlUv3", label = "Upgraded", min = 1, max = 13, value = 13))
                 )
          ),
          column(2,
                 fluidRow(column(12, selectInput(inputId = "card3v3", label = "Select Card 3", choices = cards, selected = "Graveyard"))),
                 fluidRow(
                   column(6, numericInput(inputId = "card3LvlCv3", label = "Current Lvl", min = 1, max = 13, value = 10)),
                   column(6, numericInput(inputId = "card3LvlUv3", label = "Upgraded", min = 1, max = 13, value = 10))
                 ),
                 tags$hr(),
                 fluidRow(column(12, selectInput(inputId = "card7v3", label = "Select Card 7", choices = cards, selected = "Poison"))),
                 fluidRow(
                   column(6, numericInput(inputId = "card7LvlCv3", label = "Current Lvl", min = 1, max = 13, value = 11)),
                   column(6, numericInput(inputId = "card7LvlUv3", label = "Upgraded", min = 1, max = 13, value = 11))
                 )
          ),
          column(2,
                 fluidRow(column(12, selectInput(inputId = "card4v3", label = "Select Card 4", choices = cards, selected = "Knight"))),
                 fluidRow(
                   column(6, numericInput(inputId = "card4LvlCv3", label = "Current Lvl", min = 1, max = 13, value = 11)),
                   column(6, numericInput(inputId = "card4LvlUv3", label = "Upgraded", min = 1, max = 13, value = 11))
                 ),
                 tags$hr(),
                 fluidRow(column(12, selectInput(inputId = "card8v3", label = "Select Card 8", choices = cards, selected = "Skeletons"))),
                 fluidRow(
                   column(6, numericInput(inputId = "card8LvlCv3", label = "Current Lvl", min = 1, max = 13, value = 10)),
                   column(6, numericInput(inputId = "card8LvlUv3", label = "Upgraded", min = 1, max = 13, value = 10))
                 )
          ),
          column(4, 
            fluidRow(column(12, plotOutput(outputId = "barv3"))),
            tags$hr(),
            fluidRow(column(12, tags$h4(textOutput(outputId = "upgradeCostv3"))))
            )
        )
      ),
      
      
      
      # v4 tab content
      tabItem(
        tabName = "v4",
        useShinyalert(),
        fluidRow(
          column(
            12,
            tags$h2("\tPredicting Trophies (Clash Royale Season 3)"),
            tags$h3("\tWith Linear Regression Model Based on Card Indicators and Card Levels"),
          )
        ),
        #tags$hr(),
        tags$br(),
        fluidRow(
          column(2,
                 fluidRow(column(12, selectInput(inputId = "card1v4", label = "Select Card 1", choices = cards, selected = "Baby Dragon"))),
                 fluidRow(
                   column(6, numericInput(inputId = "card1LvlCv4", label = "Current Lvl", min = 1, max = 13, value = 11)),
                   column(6, numericInput(inputId = "card1LvlUv4", label = "Upgraded", min = 1, max = 13, value = 11))
                 ),
                 tags$hr(),
                 fluidRow(column(12, selectInput(inputId = "card5v4", label = "Select Card 5", choices = cards, selected = "The Log"))),
                 fluidRow(
                   column(6, numericInput(inputId = "card5LvlCv4", label = "Current Lvl", min = 1, max = 13, value = 11)),
                   column(6, numericInput(inputId = "card5LvlUv4", label = "Upgraded", min = 1, max = 13, value = 11))
                 ),
                 tags$hr(),
                 fluidRow(column(12, actionButton(inputId = "viewRoutev4", label = "View Optimized Upgrade Route")))
          ),
          column(2,
                 fluidRow(column(12, selectInput(inputId = "card2v4", label = "Select Card 2", choices = cards, selected = "Goblin Hut"))),
                 fluidRow(
                   column(6, numericInput(inputId = "card2LvlCv4", label = "Current Lvl", min = 1, max = 13, value = 11)),
                   column(6, numericInput(inputId = "card2LvlUv4", label = "Upgraded", min = 1, max = 13, value = 11))
                 ),
                 tags$hr(),
                 fluidRow(column(12, selectInput(inputId = "card6v4", label = "Select Card 6", choices = cards, selected = "Musketeer"))),
                 fluidRow(
                   column(6, numericInput(inputId = "card6LvlCv4", label = "Current Lvl", min = 1, max = 13, value = 13)),
                   column(6, numericInput(inputId = "card6LvlUv4", label = "Upgraded", min = 1, max = 13, value = 13))
                 )
          ),
          column(2,
                 fluidRow(column(12, selectInput(inputId = "card3v4", label = "Select Card 3", choices = cards, selected = "Graveyard"))),
                 fluidRow(
                   column(6, numericInput(inputId = "card3LvlCv4", label = "Current Lvl", min = 1, max = 13, value = 10)),
                   column(6, numericInput(inputId = "card3LvlUv4", label = "Upgraded", min = 1, max = 13, value = 10))
                 ),
                 tags$hr(),
                 fluidRow(column(12, selectInput(inputId = "card7v4", label = "Select Card 7", choices = cards, selected = "Poison"))),
                 fluidRow(
                   column(6, numericInput(inputId = "card7LvlCv4", label = "Current Lvl", min = 1, max = 13, value = 11)),
                   column(6, numericInput(inputId = "card7LvlUv4", label = "Upgraded", min = 1, max = 13, value = 11))
                 )
          ),
          column(2,
                 fluidRow(column(12, selectInput(inputId = "card4v4", label = "Select Card 4", choices = cards, selected = "Knight"))),
                 fluidRow(
                   column(6, numericInput(inputId = "card4LvlCv4", label = "Current Lvl", min = 1, max = 13, value = 11)),
                   column(6, numericInput(inputId = "card4LvlUv4", label = "Upgraded", min = 1, max = 13, value = 11))
                 ),
                 tags$hr(),
                 fluidRow(column(12, selectInput(inputId = "card8v4", label = "Select Card 8", choices = cards, selected = "Skeletons"))),
                 fluidRow(
                   column(6, numericInput(inputId = "card8LvlCv4", label = "Current Lvl", min = 1, max = 13, value = 10)),
                   column(6, numericInput(inputId = "card8LvlUv4", label = "Upgraded", min = 1, max = 13, value = 10))
                 )
          ),
          column(4, 
                 fluidRow(column(12, plotOutput(outputId = "barv4"))),
                 #tags$hr(),
                 fluidRow(column(12, tags$h4(textOutput(outputId = "upgradeCostv4"))))
          )
        ),
        #tags$hr(),
        fluidRow(
          column(6, tableOutput(outputId = "opRoutev4"), offset = 1)
        )
      ),
      
      
      
      # v5 tab content
      tabItem(
        tabName = "v5",
        useShinyalert(),
        fluidRow(
          column(4,
            fluidRow(
              column(6, selectInput(inputId = "card1Lv5", label = "Card 1", choices = cards, selected = "Baby Dragon"), offset = 3),
              column(3, numericInput(inputId = "card1LvlLv5", label = "Level", min = 1, max = 13, value = 11))
            ),
            fluidRow(
              column(6, selectInput(inputId = "card2Lv5", label = "Card 2", choices = cards, selected = "Goblin Hut"), offset = 3),
              column(3, numericInput(inputId = "card2LvlLv5", label = "Level", min = 1, max = 13, value = 11))
            ),
            fluidRow(
              column(6, selectInput(inputId = "card3Lv5", label = "Card 3", choices = cards, selected = "Graveyard"), offset = 3),
              column(3, numericInput(inputId = "card3LvlLv5", label = "Level", min = 1, max = 13, value = 10))
            ),
            fluidRow(
              column(6, selectInput(inputId = "card4Lv5", label = "Card 4", choices = cards, selected = "Knight"), offset = 3),
              column(3, numericInput(inputId = "card4LvlLv5", label = "Level", min = 1, max = 13, value = 11))
            ),
            fluidRow(
              column(6, selectInput(inputId = "card5Lv5", label = "Card 5", choices = cards, selected = "The Log"), offset = 3),
              column(3, numericInput(inputId = "card5LvlLv5", label = "Level", min = 1, max = 13, value = 11))
            ),
            fluidRow(
              column(6, selectInput(inputId = "card6Lv5", label = "Card 6", choices = cards, selected = "Musketeer"), offset = 3),
              column(3, numericInput(inputId = "card6LvlLv5", label = "Level", min = 1, max = 13, value = 13))
            ),
            fluidRow(
              column(6, selectInput(inputId = "card7Lv5", label = "Card 7", choices = cards, selected = "Poison"), offset = 3),
              column(3, numericInput(inputId = "card7LvlLv5", label = "Level", min = 1, max = 13, value = 11))
            ),
            fluidRow(
              column(6, selectInput(inputId = "card8Lv5", label = "Card 8", choices = cards, selected = "Skeletons"), offset = 3),
              column(3, numericInput(inputId = "card8LvlLv5", label = "Level", min = 1, max = 13, value = 10))
            )
          ),
          column(4,
            fluidRow(
              column(7, tags$h3("Predicted Trophies"), offset = 3),
              column(5, tags$h4("For Two Decks"), offset = 4)
            ),
            tags$br(),
            fluidRow(
              column(12, plotOutput(outputId = "barv5"))
            ),
            tags$br(),
            fluidRow(
              column(2, actionButton(inputId = "viewRoutev5", label = "View Optimized Upgrade Route"), offset = 3)
            )
          ),
          column(4,
            fluidRow(
              column(6, htmlOutput(outputId = "card1RSelectorv5")),
              column(3, htmlOutput(outputId = "card1LvlRSelectorv5"))
            ),
            fluidRow(
              column(6, htmlOutput(outputId = "card2RSelectorv5")),
              column(3, htmlOutput(outputId = "card2LvlRSelectorv5"))
            ),
            fluidRow(
              column(6, htmlOutput(outputId = "card3RSelectorv5")),
              column(3, htmlOutput(outputId = "card3LvlRSelectorv5"))
            ),
            fluidRow(
              column(6, htmlOutput(outputId = "card4RSelectorv5")),
              column(3, htmlOutput(outputId = "card4LvlRSelectorv5"))
            ),
            fluidRow(
              column(6, htmlOutput(outputId = "card5RSelectorv5")),
              column(3, htmlOutput(outputId = "card5LvlRSelectorv5"))
            ),
            fluidRow(
              column(6, htmlOutput(outputId = "card6RSelectorv5")),
              column(3, htmlOutput(outputId = "card6LvlRSelectorv5"))
            ),
            fluidRow(
              column(6, htmlOutput(outputId = "card7RSelectorv5")),
              column(3, htmlOutput(outputId = "card7LvlRSelectorv5"))
            ),
            fluidRow(
              column(6, htmlOutput(outputId = "card8RSelectorv5")),
              column(3, htmlOutput(outputId = "card8LvlRSelectorv5"))
            )
          )
        ),
        fluidRow(
          column(5, rHandsontableOutput(outputId = "opRouteLv5")),
          column(5, rHandsontableOutput(outputId = "opRouteRv5"), offset = 2)
        )
      )
      
      
      
      # v6 tab content (potentially)
      
      
      
      
      
      
      
      
      
      
    )
  )
)



server <- function(input, output, session) {
  
  # For tab 1 (version 1)
  output$prediction <- renderText({
    # fill out newx (prediction data frame) based on input
    newx <- newxEmpty
    
    # process selected cards
    cardsInput <- c(input$card1, input$card2, input$card3, input$card4, input$card5, input$card6, input$card7, input$card8)
    
    for (i in 1:length(cardsInput)) {
      for (j in 1:length(names(newx))) {
        if (cardsInput[i] == names(newx)[j]) {
          newx[1,j] <- 1
        }
      }
    }
    
    # process selected levels
    lvlInput <- c(input$card1Lvl, input$card2Lvl, input$card3Lvl, input$card4Lvl, input$card5Lvl, input$card6Lvl, input$card7Lvl, input$card8Lvl)
    
    cardsInputClean <- str_remove_all(cardsInput, "\\.|-")
    cardsInputClean <- paste(cardsInputClean, "Lvl")
    
    for (k in 1:length(cardsInputClean)) {
      for (l in 1:length(names(newx))) {
        if (cardsInputClean[k] == names(newx)[l]) {
          newx[1,l] <- lvlInput[k]
        }
      }
    }
    
    # now use newx for prediction
    predictedTrophies <- as.integer(predict.lm(mod1, newx))
    
    paste("Trophy Prediction:", predictedTrophies)
  })
  
  
  
  # For tab 2 (version 2)
  output$bar <- renderPlot({
    # get predictions
    
    # for before upgrade
    
    # fill out newx (prediction data frame) based on input
    newxBeforev2 <- newxEmpty
    
    # process selected cards
    cardsInputv2 <- c(input$card1v2, input$card2v2, input$card3v2, input$card4v2, input$card5v2, input$card6v2, input$card7v2, input$card8v2)
    
    for (iv2 in 1:length(cardsInputv2)) {
      for (jv2 in 1:length(names(newxBeforev2))) {
        if (cardsInputv2[iv2] == names(newxBeforev2)[jv2]) {
          newxBeforev2[1,jv2] <- 1
        }
      }
    }
    
    # process selected levels
    lvlInputBeforev2 <- c(input$card1LvlC, input$card2LvlC, input$card3LvlC, input$card4LvlC, input$card5LvlC, input$card6LvlC, input$card7LvlC, input$card8LvlC)
    
    cardsInputCleanv2 <- str_remove_all(cardsInputv2, "\\.|-")
    cardsInputCleanv2 <- paste(cardsInputCleanv2, "Lvl")
    
    for (kCv2 in 1:length(cardsInputCleanv2)) {
      for (lCv2 in 1:length(names(newxBeforev2))) {
        if (cardsInputCleanv2[kCv2] == names(newxBeforev2)[lCv2]) {
          newxBeforev2[1,lCv2] <- lvlInputBeforev2[kCv2]
        }
      }
    }
    
    # now use newx for prediction
    predictedTrophiesBeforev2 <- as.integer(predict.lm(mod1, newxBeforev2))
    
    
    
    # for after upgrade
    
    # fill out newx (prediction data frame) based on input
    newxAfterv2 <- newxEmpty
    
    # process selected cards
    cardsInputv2 <- c(input$card1v2, input$card2v2, input$card3v2, input$card4v2, input$card5v2, input$card6v2, input$card7v2, input$card8v2)
    
    for (iUv2 in 1:length(cardsInputv2)) {
      for (jUv2 in 1:length(names(newxAfterv2))) {
        if (cardsInputv2[iUv2] == names(newxAfterv2)[jUv2]) {
          newxAfterv2[1,jUv2] <- 1
        }
      }
    }
    
    # process selected levels
    lvlInputAfterv2 <- c(input$card1LvlU, input$card2LvlU, input$card3LvlU, input$card4LvlU, input$card5LvlU, input$card6LvlU, input$card7LvlU, input$card8LvlU)
    
    for (kUv2 in 1:length(cardsInputCleanv2)) {
      for (lUv2 in 1:length(names(newxAfterv2))) {
        if (cardsInputCleanv2[kUv2] == names(newxAfterv2)[lUv2]) {
          newxAfterv2[1,lUv2] <- lvlInputAfterv2[kUv2]
        }
      }
    }
    
    # now use newx for prediction
    predictedTrophiesAfterv2 <- as.integer(predict.lm(mod1, newxAfterv2))
    
    
    
    # the plot
    df_Barplot <- tibble(
      predictions = c(predictedTrophiesBeforev2, predictedTrophiesAfterv2),
      source = factor(c("Before Upgrade", "After Upgrade"))
    )
    
    df_Barplot$source <- fct_inorder(df_Barplot$source)
    
    df_Barplot %>%
      ggplot(., aes(x = source, y = predictions)) +
      geom_col(width = 0.1, aes(fill = source)) +
      geom_label(aes(label = predictions), size = 6) +
      theme_minimal() +
      scale_fill_brewer(palette = "Paired") +
      theme(
        legend.position = "none",
        line = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(
          color = "black",
          size = rel(1.5),
          vjust = 3
        ),
        plot.background = element_rect(fill = rgb(1,1,1,1))
      )
    
  })
  
  
  
  # for version 3 tab
  output$barv3 <- renderPlot({
    # get predictions
    
    # for before upgrade
    
    # fill out newx (prediction data frame) based on input
    newxBeforev3 <- newxEmpty
    
    # process selected cards
    cardsInputv3 <- c(input$card1v3, input$card2v3, input$card3v3, input$card4v3, input$card5v3, input$card6v3, input$card7v3, input$card8v3)
    
    for (iv3 in 1:length(cardsInputv3)) {
      for (jv3 in 1:length(names(newxBeforev3))) {
        if (cardsInputv3[iv3] == names(newxBeforev3)[jv3]) {
          newxBeforev3[1,jv3] <- 1
        }
      }
    }
    
    # process selected levels
    lvlInputBeforev3 <- c(input$card1LvlCv3, input$card2LvlCv3, input$card3LvlCv3, input$card4LvlCv3, input$card5LvlCv3, input$card6LvlCv3, input$card7LvlCv3, input$card8LvlCv3)
    
    cardsInputCleanv3 <- str_remove_all(cardsInputv3, "\\.|-")
    cardsInputCleanv3 <- paste(cardsInputCleanv3, "Lvl")
    
    for (kCv3 in 1:length(cardsInputCleanv3)) {
      for (lCv3 in 1:length(names(newxBeforev3))) {
        if (cardsInputCleanv3[kCv3] == names(newxBeforev3)[lCv3]) {
          newxBeforev3[1,lCv3] <- lvlInputBeforev3[kCv3]
        }
      }
    }
    
    # now use newx for prediction
    predictedTrophiesBeforev3 <- as.integer(predict.lm(mod1, newxBeforev3))
    
    
    
    # for after upgrade
    
    # fill out newx (prediction data frame) based on input
    newxAfterv3 <- newxEmpty
    
    # process selected cards
    cardsInputv3 <- c(input$card1v3, input$card2v3, input$card3v3, input$card4v3, input$card5v3, input$card6v3, input$card7v3, input$card8v3)
    
    for (iUv3 in 1:length(cardsInputv3)) {
      for (jUv3 in 1:length(names(newxAfterv3))) {
        if (cardsInputv3[iUv3] == names(newxAfterv3)[jUv3]) {
          newxAfterv3[1,jUv3] <- 1
        }
      }
    }
    
    # process selected levels
    lvlInputAfterv3 <- c(input$card1LvlUv3, input$card2LvlUv3, input$card3LvlUv3, input$card4LvlUv3, input$card5LvlUv3, input$card6LvlUv3, input$card7LvlUv3, input$card8LvlUv3)
    
    for (kUv3 in 1:length(cardsInputCleanv3)) {
      for (lUv3 in 1:length(names(newxAfterv3))) {
        if (cardsInputCleanv3[kUv3] == names(newxAfterv3)[lUv3]) {
          newxAfterv3[1,lUv3] <- lvlInputAfterv3[kUv3]
        }
      }
    }
    
    # now use newx for prediction
    predictedTrophiesAfterv3 <- as.integer(predict.lm(mod1, newxAfterv3))
    
    
    
    # the plot
    df_Barplotv3 <- tibble(
      predictions = c(predictedTrophiesBeforev3, predictedTrophiesAfterv3),
      source = factor(c("Before Upgrade", "After Upgrade"))
    )
    
    df_Barplotv3$source <- fct_inorder(df_Barplotv3$source)
    
    df_Barplotv3 %>%
      ggplot(., aes(x = source, y = predictions)) +
      geom_col(width = 0.1, aes(fill = source)) +
      geom_label(aes(label = predictions), size = 6) +
      theme_minimal() +
      scale_fill_brewer(palette = "Paired") +
      theme(
        legend.position = "none",
        line = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(
          color = "black",
          size = rel(1.5),
          vjust = 3
        ),
        plot.background = element_rect(fill = rgb(1,1,1,1))
      )
    
  })
  
  
  
  output$upgradeCostv3 <- renderText({
    costv3 <-
      sum(
        costCalc(input$card1v3, input$card1LvlCv3, input$card1LvlUv3),
        costCalc(input$card2v3, input$card2LvlCv3, input$card2LvlUv3),
        costCalc(input$card3v3, input$card3LvlCv3, input$card3LvlUv3),
        costCalc(input$card4v3, input$card4LvlCv3, input$card4LvlUv3),
        costCalc(input$card5v3, input$card5LvlCv3, input$card5LvlUv3),
        costCalc(input$card6v3, input$card6LvlCv3, input$card6LvlUv3),
        costCalc(input$card7v3, input$card7LvlCv3, input$card7LvlUv3),
        costCalc(input$card8v3, input$card8LvlCv3, input$card8LvlUv3)
      )
    
    paste("Upgrade Cost:", costv3)
  })
  
  
  
  # for version 4 tab
  output$barv4 <- renderPlot({
    # get predictions
    
    # for before upgrade
    
    # fill out newx (prediction data frame) based on input
    newxBeforev4 <- newxEmpty
    
    # process selected cards
    cardsInputv4 <- c(input$card1v4, input$card2v4, input$card3v4, input$card4v4, input$card5v4, input$card6v4, input$card7v4, input$card8v4)
    
    for (iv4 in 1:length(cardsInputv4)) {
      for (jv4 in 1:length(names(newxBeforev4))) {
        if (cardsInputv4[iv4] == names(newxBeforev4)[jv4]) {
          newxBeforev4[1,jv4] <- 1
        }
      }
    }
    
    # process selected levels
    lvlInputBeforev4 <- c(input$card1LvlCv4, input$card2LvlCv4, input$card3LvlCv4, input$card4LvlCv4, input$card5LvlCv4, input$card6LvlCv4, input$card7LvlCv4, input$card8LvlCv4)
    
    cardsInputCleanv4 <- str_remove_all(cardsInputv4, "\\.|-")
    cardsInputCleanv4 <- paste(cardsInputCleanv4, "Lvl")
    
    for (kCv4 in 1:length(cardsInputCleanv4)) {
      for (lCv4 in 1:length(names(newxBeforev4))) {
        if (cardsInputCleanv4[kCv4] == names(newxBeforev4)[lCv4]) {
          newxBeforev4[1,lCv4] <- lvlInputBeforev4[kCv4]
        }
      }
    }
    
    # now use newx for prediction
    predictedTrophiesBeforev4 <- as.integer(predict.lm(mod1, newxBeforev4))
    
    
    
    # for after upgrade
    
    # fill out newx (prediction data frame) based on input
    newxAfterv4 <- newxEmpty
    
    # process selected cards
    cardsInputv4 <- c(input$card1v4, input$card2v4, input$card3v4, input$card4v4, input$card5v4, input$card6v4, input$card7v4, input$card8v4)
    
    for (iUv4 in 1:length(cardsInputv4)) {
      for (jUv4 in 1:length(names(newxAfterv4))) {
        if (cardsInputv4[iUv4] == names(newxAfterv4)[jUv4]) {
          newxAfterv4[1,jUv4] <- 1
        }
      }
    }
    
    # process selected levels
    lvlInputAfterv4 <- c(input$card1LvlUv4, input$card2LvlUv4, input$card3LvlUv4, input$card4LvlUv4, input$card5LvlUv4, input$card6LvlUv4, input$card7LvlUv4, input$card8LvlUv4)
    
    for (kUv4 in 1:length(cardsInputCleanv4)) {
      for (lUv4 in 1:length(names(newxAfterv4))) {
        if (cardsInputCleanv4[kUv4] == names(newxAfterv4)[lUv4]) {
          newxAfterv4[1,lUv4] <- lvlInputAfterv4[kUv4]
        }
      }
    }
    
    # now use newx for prediction
    predictedTrophiesAfterv4 <- as.integer(predict.lm(mod1, newxAfterv4))
    
    
    
    # the plot
    df_Barplotv4 <- tibble(
      predictions = c(predictedTrophiesBeforev4, predictedTrophiesAfterv4),
      source = factor(c("Before Upgrade", "After Upgrade"))
    )
    
    df_Barplotv4$source <- fct_inorder(df_Barplotv4$source)
    
    df_Barplotv4 %>%
      ggplot(., aes(x = source, y = predictions)) +
      geom_col(width = 0.1, aes(fill = source)) +
      geom_label(aes(label = predictions), size = 6) +
      theme_minimal() +
      scale_fill_brewer(palette = "Paired") +
      theme(
        legend.position = "none",
        line = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(
          color = "black",
          size = rel(1.5),
          vjust = 3
        ),
        plot.background = element_rect(fill = rgb(1,1,1,1))
      )
    
  })
  
  
  
  output$upgradeCostv4 <- renderText({
    costv4 <-
      sum(
        costCalc(input$card1v4, input$card1LvlCv4, input$card1LvlUv4),
        costCalc(input$card2v4, input$card2LvlCv4, input$card2LvlUv4),
        costCalc(input$card3v4, input$card3LvlCv4, input$card3LvlUv4),
        costCalc(input$card4v4, input$card4LvlCv4, input$card4LvlUv4),
        costCalc(input$card5v4, input$card5LvlCv4, input$card5LvlUv4),
        costCalc(input$card6v4, input$card6LvlCv4, input$card6LvlUv4),
        costCalc(input$card7v4, input$card7LvlCv4, input$card7LvlUv4),
        costCalc(input$card8v4, input$card8LvlCv4, input$card8LvlUv4)
      )
    
    paste("Upgrade Cost:", costv4)
  })
  
  
  
  viewRouteTable <- eventReactive(input$viewRoutev4, {
    # alert
    shinyalert(
      title = "Please Wait",
      text = "It may take a few seconds",
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = FALSE,
      type = "info",
      showConfirmButton = FALSE,
      showCancelButton = FALSE,
      timer = 2000,
      imageUrl = "",
      animation = TRUE
    )
    
    # the current most updated optimized deck (starting from no upgrades)
    opDeckv4 <- tibble(
      card = c(input$card1v4, input$card2v4, input$card3v4, input$card4v4, input$card5v4, input$card6v4, input$card7v4, input$card8v4),
      level = c(input$card1LvlCv4, input$card2LvlCv4, input$card3LvlCv4, input$card4LvlCv4, input$card5LvlCv4, input$card6LvlCv4, input$card7LvlCv4, input$card8LvlCv4)
    )
    
    # build an empty route data frame
    opRouteDFv4 <- tibble(
      card = "na",
      levelPlus1 = 0,
      goldRequired = 0,
      trophyGain = 0,
      trophyGainPer1000Gold = 0
    )
    
    # while the updated op deck still has cards to upgrade
    while (sum(opDeckv4$level) < 8*13) {
      # remove any rows with card level > 13
      opTestv4 <- opDeckv4
      opTestv4 <-
        opTestv4 %>%
        mutate(levelPlus1 = level + 1) %>%
        select(card, levelPlus1) %>%
        filter(levelPlus1 <= 13)
      
      # build empty stage upgrades df
      stageUpgradesv4 <- opTestv4
      stageUpgradesv4 <-
        stageUpgradesv4 %>%
        mutate(
          goldRequired = 0,
          trophyGain = 0,
          trophyGainPer1000Gold = 0
        )
      
      # the deck before any upgrades
      opDeckBeforev4 <- opDeckv4
      
      
      
      # build newx for before-upgrade deck
      newxOpBeforev4 <- newxEmpty
      
      # process selected cards
      cardsInputOpv4 <- opDeckBeforev4$card
      
      for (av4 in 1:length(cardsInputOpv4)) {
        for (bv4 in 1:length(names(newxOpBeforev4))) {
          if (cardsInputOpv4[av4] == names(newxOpBeforev4)[bv4]) {
            newxOpBeforev4[1,bv4] <- 1
          }
        }
      }
      
      # process selected levels
      lvlInputOpBeforev4 <- opDeckBeforev4$level
      
      cardsInputOpCleanv4 <- str_remove_all(cardsInputOpv4, "\\.|-")
      cardsInputOpCleanv4 <- paste(cardsInputOpCleanv4, "Lvl")
      
      for (cv4 in 1:length(cardsInputOpCleanv4)) {
        for (dv4 in 1:length(names(newxOpBeforev4))) {
          if (cardsInputOpCleanv4[cv4] == names(newxOpBeforev4)[dv4]) {
            newxOpBeforev4[1,dv4] <- lvlInputOpBeforev4[cv4]
          }
        }
      }
      
      # now use newx for prediction
      opDeckBeforeTrophiesv4 <- as.integer(predict.lm(mod1, newxOpBeforev4))
      
      
      
      # for each card that can be upgraded at this stage
      for (ev4 in 1:nrow(opTestv4)) {
        # build deck representing after upgrading that single card
        opDeckAfterv4 <- opDeckBeforev4
        for (fv4 in 1:nrow(opDeckAfterv4)) {
          # if the card in the deck matches the card in question
          if (opDeckAfterv4[fv4,1] == opTestv4[ev4,1]) {
            opDeckAfterv4[fv4, 2] <- opDeckAfterv4[fv4, 2] + 1 # increase level by 1
          }
        }
        
        # build newx for after-upgrade deck
        newxOpAfterv4 <- newxEmpty
        
        # process selected cards
        cardsInputOpv4 <- opDeckAfterv4$card
        
        for (gv4 in 1:length(cardsInputOpv4)) {
          for (hv4 in 1:length(names(newxOpAfterv4))) {
            if (cardsInputOpv4[gv4] == names(newxOpAfterv4)[hv4]) {
              newxOpAfterv4[1,hv4] <- 1
            }
          }
        }
        
        # process selected levels
        lvlInputOpAfterv4 <- opDeckAfterv4$level
        
        for (iv4 in 1:length(cardsInputOpCleanv4)) {
          for (jv4 in 1:length(names(newxOpAfterv4))) {
            if (cardsInputOpCleanv4[iv4] == names(newxOpAfterv4)[jv4]) {
              newxOpAfterv4[1,jv4] <- lvlInputOpAfterv4[iv4]
            }
          }
        }
        
        # now use newx for prediction
        opDeckAfterTrophiesv4 <- as.integer(predict.lm(mod1, newxOpAfterv4))
        
        
        
        # calculate gold spent for upgrading that card
        goldSpentOpv4 <- costCalc(opTestv4$card[ev4], opTestv4$levelPlus1[ev4] - 1, opTestv4$levelPlus1[ev4])
        
        # fill out that row for stage upgrades df
        stageUpgradesv4[ev4,3] <- goldSpentOpv4
        stageUpgradesv4[ev4,4] <- opDeckAfterTrophiesv4 - opDeckBeforeTrophiesv4
        stageUpgradesv4[ev4,5] <- stageUpgradesv4[ev4,4] / stageUpgradesv4[ev4,3] * 1000
      }
      
      
      
      # filter the row with the largest gain/gold ratio and add that to opRouteDF
      opUpgradev4 <-
        stageUpgradesv4 %>%
        filter(trophyGainPer1000Gold == max(trophyGainPer1000Gold)) %>%
        slice(1) # make sure there's only one case
      
      opRouteDFv4 <- bind_rows(opRouteDFv4, opUpgradev4)
      
      # update opDeckv4 (fill in the card that is upgraded)
      for (kv4 in 1:nrow(opDeckv4)) {
        if (opDeckv4[kv4,1] == opUpgradev4[1,1]) {
          opDeckv4[kv4,2] <- opUpgradev4[1,2]
        }
      }
    }
    
    # print out the op route df
    opRouteDFv4 %>%
      slice(-1) %>%
      mutate(
        Step = c(1:nrow(.)),
        Card = card,
        `Upgrade To` = as.integer(levelPlus1),
        `Gold Required` = as.integer(goldRequired),
        `Trophy Gain` = as.integer(trophyGain)
      ) %>%
      select(Step, Card, `Upgrade To`, `Gold Required`, `Trophy Gain`)
  })
  
  
  
  output$opRoutev4 <- renderTable({
    viewRouteTable()
  })
  
  
  
  # for version 5 tab
  
  # reactive select boxes for right deck
  output$card1RSelectorv5 <- renderUI({
    selectInput(inputId = "card1Rv5", label = "Card 1", choices = cards, selected = input$card1Lv5)
  })
  
  output$card1LvlRSelectorv5 <- renderUI({
    numericInput(inputId = "card1LvlRv5", label = "Level", min = 1, max = 13, value = input$card1LvlLv5)
  })
  
  output$card2RSelectorv5 <- renderUI({
    selectInput(inputId = "card2Rv5", label = "Card 2", choices = cards, selected = input$card2Lv5)
  })
  
  output$card2LvlRSelectorv5 <- renderUI({
    numericInput(inputId = "card2LvlRv5", label = "Level", min = 1, max = 13, value = input$card2LvlLv5)
  })
  
  output$card3RSelectorv5 <- renderUI({
    selectInput(inputId = "card3Rv5", label = "Card 3", choices = cards, selected = input$card3Lv5)
  })
  
  output$card3LvlRSelectorv5 <- renderUI({
    numericInput(inputId = "card3LvlRv5", label = "Level", min = 1, max = 13, value = input$card3LvlLv5)
  })
  
  output$card4RSelectorv5 <- renderUI({
    selectInput(inputId = "card4Rv5", label = "Card 4", choices = cards, selected = input$card4Lv5)
  })
  
  output$card4LvlRSelectorv5 <- renderUI({
    numericInput(inputId = "card4LvlRv5", label = "Level", min = 1, max = 13, value = input$card4LvlLv5)
  })
  
  output$card5RSelectorv5 <- renderUI({
    selectInput(inputId = "card5Rv5", label = "Card 5", choices = cards, selected = input$card5Lv5)
  })
  
  output$card5LvlRSelectorv5 <- renderUI({
    numericInput(inputId = "card5LvlRv5", label = "Level", min = 1, max = 13, value = input$card5LvlLv5)
  })
  
  output$card6RSelectorv5 <- renderUI({
    selectInput(inputId = "card6Rv5", label = "Card 6", choices = cards, selected = input$card6Lv5)
  })
  
  output$card6LvlRSelectorv5 <- renderUI({
    numericInput(inputId = "card6LvlRv5", label = "Level", min = 1, max = 13, value = input$card6LvlLv5)
  })
  
  output$card7RSelectorv5 <- renderUI({
    selectInput(inputId = "card7Rv5", label = "Card 7", choices = cards, selected = input$card7Lv5)
  })
  
  output$card7LvlRSelectorv5 <- renderUI({
    numericInput(inputId = "card7LvlRv5", label = "Level", min = 1, max = 13, value = input$card7LvlLv5)
  })
  
  output$card8RSelectorv5 <- renderUI({
    selectInput(inputId = "card8Rv5", label = "Card 8", choices = cards, selected = input$card8Lv5)
  })
  
  output$card8LvlRSelectorv5 <- renderUI({
    numericInput(inputId = "card8LvlRv5", label = "Level", min = 1, max = 13, value = input$card8LvlLv5)
  })
  
  
  
  # for bar plot
  output$barv5 <- renderPlot({
    # get predictions
    
    # for left deck
    
    # fill out newx (prediction data frame) based on input
    newxLv5 <- newxEmpty
    
    # process selected cards
    cardsInputLv5 <- c(input$card1Lv5, input$card2Lv5, input$card3Lv5, input$card4Lv5, input$card5Lv5, input$card6Lv5, input$card7Lv5, input$card8Lv5)
    
    for (av5 in 1:length(cardsInputLv5)) {
      for (bv5 in 1:length(names(newxLv5))) {
        if (cardsInputLv5[av5] == names(newxLv5)[bv5]) {
          newxLv5[1,bv5] <- 1
        }
      }
    }
    
    # process selected levels
    lvlInputLv5 <- c(input$card1LvlLv5, input$card2LvlLv5, input$card3LvlLv5, input$card4LvlLv5, input$card5LvlLv5, input$card6LvlLv5, input$card7LvlLv5, input$card8LvlLv5)
    
    cardsInputCleanLv5 <- str_remove_all(cardsInputLv5, "\\.|-")
    cardsInputCleanLv5 <- paste(cardsInputCleanLv5, "Lvl")
    
    for (cv5 in 1:length(cardsInputCleanLv5)) {
      for (dv5 in 1:length(names(newxLv5))) {
        if (cardsInputCleanLv5[cv5] == names(newxLv5)[dv5]) {
          newxLv5[1,dv5] <- lvlInputLv5[cv5]
        }
      }
    }
    
    # now use newx for prediction
    predictedTrophiesLv5 <- as.integer(predict.lm(mod1, newxLv5))
    
    
    
    # for right deck
    
    # fill out newx (prediction data frame) based on input
    newxRv5 <- newxEmpty
    
    # process selected cards
    cardsInputRv5 <- c(input$card1Rv5, input$card2Rv5, input$card3Rv5, input$card4Rv5, input$card5Rv5, input$card6Rv5, input$card7Rv5, input$card8Rv5)
    
    for (ev5 in 1:length(cardsInputRv5)) {
      for (fv5 in 1:length(names(newxRv5))) {
        if (cardsInputRv5[ev5] == names(newxRv5)[fv5]) {
          newxRv5[1,fv5] <- 1
        }
      }
    }
    
    # process selected levels
    lvlInputRv5 <- c(input$card1LvlRv5, input$card2LvlRv5, input$card3LvlRv5, input$card4LvlRv5, input$card5LvlRv5, input$card6LvlRv5, input$card7LvlRv5, input$card8LvlRv5)
    
    cardsInputCleanRv5 <- str_remove_all(cardsInputRv5, "\\.|-")
    cardsInputCleanRv5 <- paste(cardsInputCleanRv5, "Lvl")
    
    for (gv5 in 1:length(cardsInputCleanRv5)) {
      for (hv5 in 1:length(names(newxRv5))) {
        if (cardsInputCleanRv5[gv5] == names(newxRv5)[hv5]) {
          newxRv5[1,hv5] <- lvlInputRv5[gv5]
        }
      }
    }
    
    # now use newx for prediction
    predictedTrophiesRv5 <- as.integer(predict.lm(mod1, newxRv5))
    
    
    
    # the plot
    df_Barplotv5 <- tibble(
      predictions = c(predictedTrophiesLv5, predictedTrophiesRv5),
      source = factor(c("Deck 1", "Deck 2"))
    )
    
    df_Barplotv5$source <- fct_inorder(df_Barplotv5$source)
    
    df_Barplotv5 %>%
      ggplot(., aes(x = source, y = predictions)) +
      geom_col(width = 0.1, aes(fill = source)) +
      geom_label(aes(label = predictions), size = 6) +
      theme_minimal() +
      scale_fill_brewer(palette = "Paired") +
      theme(
        legend.position = "none",
        line = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(
          color = "black",
          size = rel(1.5),
          vjust = 3
        ),
        plot.background = element_rect(fill = rgb(1,1,1,1))
      )
    
  })
  
  
  
  # View Route Table Left
  viewRouteTableLv5 <- eventReactive(input$viewRoutev5, {
    # alert
    shinyalert(
      title = "Please Wait",
      text = "It may take a few seconds",
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = FALSE,
      type = "info",
      showConfirmButton = FALSE,
      showCancelButton = FALSE,
      timer = 2000,
      imageUrl = "",
      animation = TRUE
    )
    
    # the current most updated optimized deck (starting from no upgrades)
    opDeckLv5 <- tibble(
      card = c(input$card1Lv5, input$card2Lv5, input$card3Lv5, input$card4Lv5, input$card5Lv5, input$card6Lv5, input$card7Lv5, input$card8Lv5),
      level = c(input$card1LvlLv5, input$card2LvlLv5, input$card3LvlLv5, input$card4LvlLv5, input$card5LvlLv5, input$card6LvlLv5, input$card7LvlLv5, input$card8LvlLv5)
    )
    
    # build an empty route data frame
    opRouteDFLv5 <- tibble(
      card = "na",
      levelPlus1 = 0,
      goldRequired = 0,
      trophyGain = 0,
      trophyGainPer1000Gold = 0
    )
    
    # while the updated op deck still has cards to upgrade
    while (sum(opDeckLv5$level) < 8*13) {
      # remove any rows with card level > 13
      opTestLv5 <- opDeckLv5
      opTestLv5 <-
        opTestLv5 %>%
        mutate(levelPlus1 = level + 1) %>%
        select(card, levelPlus1) %>%
        filter(levelPlus1 <= 13)
      
      # build empty stage upgrades df
      stageUpgradesLv5 <- opTestLv5
      stageUpgradesLv5 <-
        stageUpgradesLv5 %>%
        mutate(
          goldRequired = 0,
          trophyGain = 0,
          trophyGainPer1000Gold = 0
        )
      
      # the deck before any upgrades
      opDeckBeforeLv5 <- opDeckLv5
      
      
      
      # build newx for before-upgrade deck
      newxOpBeforeLv5 <- newxEmpty
      
      # process selected cards
      cardsInputOpLv5 <- opDeckBeforeLv5$card
      
      for (iv5 in 1:length(cardsInputOpLv5)) {
        for (jv5 in 1:length(names(newxOpBeforeLv5))) {
          if (cardsInputOpLv5[iv5] == names(newxOpBeforeLv5)[jv5]) {
            newxOpBeforeLv5[1,jv5] <- 1
          }
        }
      }
      
      # process selected levels
      lvlInputOpBeforeLv5 <- opDeckBeforeLv5$level
      
      cardsInputOpCleanLv5 <- str_remove_all(cardsInputOpLv5, "\\.|-")
      cardsInputOpCleanLv5 <- paste(cardsInputOpCleanLv5, "Lvl")
      
      for (kv5 in 1:length(cardsInputOpCleanLv5)) {
        for (lv5 in 1:length(names(newxOpBeforeLv5))) {
          if (cardsInputOpCleanLv5[kv5] == names(newxOpBeforeLv5)[lv5]) {
            newxOpBeforeLv5[1,lv5] <- lvlInputOpBeforeLv5[kv5]
          }
        }
      }
      
      # now use newx for prediction
      opDeckBeforeTrophiesLv5 <- as.integer(predict.lm(mod1, newxOpBeforeLv5))
      
      
      
      # for each card that can be upgraded at this stage
      for (mv5 in 1:nrow(opTestLv5)) {
        # build deck representing after upgrading that single card
        opDeckAfterLv5 <- opDeckBeforeLv5
        for (nv5 in 1:nrow(opDeckAfterLv5)) {
          # if the card in the deck matches the card in question
          if (opDeckAfterLv5[nv5,1] == opTestLv5[mv5,1]) {
            opDeckAfterLv5[nv5, 2] <- opDeckAfterLv5[nv5, 2] + 1 # increase level by 1
          }
        }
        
        # build newx for after-upgrade deck
        newxOpAfterLv5 <- newxEmpty
        
        # process selected cards
        cardsInputOpLv5 <- opDeckAfterLv5$card
        
        for (ov5 in 1:length(cardsInputOpLv5)) {
          for (pv5 in 1:length(names(newxOpAfterLv5))) {
            if (cardsInputOpLv5[ov5] == names(newxOpAfterLv5)[pv5]) {
              newxOpAfterLv5[1,pv5] <- 1
            }
          }
        }
        
        # process selected levels
        lvlInputOpAfterLv5 <- opDeckAfterLv5$level
        
        for (qv5 in 1:length(cardsInputOpCleanLv5)) {
          for (rv5 in 1:length(names(newxOpAfterLv5))) {
            if (cardsInputOpCleanLv5[qv5] == names(newxOpAfterLv5)[rv5]) {
              newxOpAfterLv5[1,rv5] <- lvlInputOpAfterLv5[qv5]
            }
          }
        }
        
        # now use newx for prediction
        opDeckAfterTrophiesLv5 <- as.integer(predict.lm(mod1, newxOpAfterLv5))
        
        
        
        # calculate gold spent for upgrading that card
        goldSpentOpLv5 <- costCalc(opTestLv5$card[mv5], opTestLv5$levelPlus1[mv5] - 1, opTestLv5$levelPlus1[mv5])
        
        # fill out that row for stage upgrades df
        stageUpgradesLv5[mv5,3] <- goldSpentOpLv5
        stageUpgradesLv5[mv5,4] <- opDeckAfterTrophiesLv5 - opDeckBeforeTrophiesLv5
        stageUpgradesLv5[mv5,5] <- stageUpgradesLv5[mv5,4] / stageUpgradesLv5[mv5,3] * 1000
      }
      
      
      
      # filter the row with the largest gain/gold ratio and add that to opRouteDF
      opUpgradeLv5 <-
        stageUpgradesLv5 %>%
        filter(trophyGainPer1000Gold == max(trophyGainPer1000Gold)) %>%
        slice(1) # make sure there's only one case
      
      opRouteDFLv5 <- bind_rows(opRouteDFLv5, opUpgradeLv5)
      
      # update opDeckv4 (fill in the card that is upgraded)
      for (sv5 in 1:nrow(opDeckLv5)) {
        if (opDeckLv5[sv5,1] == opUpgradeLv5[1,1]) {
          opDeckLv5[sv5,2] <- opUpgradeLv5[1,2]
        }
      }
    }
    
    # print out the op route df
    opRouteDFLv5 %>%
      slice(-1) %>%
      mutate(
        Step = c(1:nrow(.)),
        Card = card,
        `Upgrade To` = as.integer(levelPlus1),
        `Gold Required` = as.integer(goldRequired),
        `Trophy Gain` = as.integer(trophyGain)
      ) %>%
      select(Step, Card, `Upgrade To`, `Gold Required`, `Trophy Gain`)
  })
  
  
  
  output$opRouteLv5 <- renderRHandsontable({
    rhandsontable(viewRouteTableLv5(), manualRowMove = TRUE)
  })
  
  
  
  # view route table right
  viewRouteTableRv5 <- eventReactive(input$viewRoutev5, {
    # the current most updated optimized deck (starting from no upgrades)
    opDeckRv5 <- tibble(
      card = c(input$card1Rv5, input$card2Rv5, input$card3Rv5, input$card4Rv5, input$card5Rv5, input$card6Rv5, input$card7Rv5, input$card8Rv5),
      level = c(input$card1LvlRv5, input$card2LvlRv5, input$card3LvlRv5, input$card4LvlRv5, input$card5LvlRv5, input$card6LvlRv5, input$card7LvlRv5, input$card8LvlRv5)
    )
    
    # build an empty route data frame
    opRouteDFRv5 <- tibble(
      card = "na",
      levelPlus1 = 0,
      goldRequired = 0,
      trophyGain = 0,
      trophyGainPer1000Gold = 0
    )
    
    # while the updated op deck still has cards to upgrade
    while (sum(opDeckRv5$level) < 8*13) {
      # remove any rows with card level > 13
      opTestRv5 <- opDeckRv5
      opTestRv5 <-
        opTestRv5 %>%
        mutate(levelPlus1 = level + 1) %>%
        select(card, levelPlus1) %>%
        filter(levelPlus1 <= 13)
      
      # build empty stage upgrades df
      stageUpgradesRv5 <- opTestRv5
      stageUpgradesRv5 <-
        stageUpgradesRv5 %>%
        mutate(
          goldRequired = 0,
          trophyGain = 0,
          trophyGainPer1000Gold = 0
        )
      
      # the deck before any upgrades
      opDeckBeforeRv5 <- opDeckRv5
      
      
      
      # build newx for before-upgrade deck
      newxOpBeforeRv5 <- newxEmpty
      
      # process selected cards
      cardsInputOpRv5 <- opDeckBeforeRv5$card
      
      for (iiv5 in 1:length(cardsInputOpRv5)) {
        for (jjv5 in 1:length(names(newxOpBeforeRv5))) {
          if (cardsInputOpRv5[iiv5] == names(newxOpBeforeRv5)[jjv5]) {
            newxOpBeforeRv5[1,jjv5] <- 1
          }
        }
      }
      
      # process selected levels
      lvlInputOpBeforeRv5 <- opDeckBeforeRv5$level
      
      cardsInputOpCleanRv5 <- str_remove_all(cardsInputOpRv5, "\\.|-")
      cardsInputOpCleanRv5 <- paste(cardsInputOpCleanRv5, "Lvl")
      
      for (kkv5 in 1:length(cardsInputOpCleanRv5)) {
        for (llv5 in 1:length(names(newxOpBeforeRv5))) {
          if (cardsInputOpCleanRv5[kkv5] == names(newxOpBeforeRv5)[llv5]) {
            newxOpBeforeRv5[1,llv5] <- lvlInputOpBeforeRv5[kkv5]
          }
        }
      }
      
      # now use newx for prediction
      opDeckBeforeTrophiesRv5 <- as.integer(predict.lm(mod1, newxOpBeforeRv5))
      
      
      
      # for each card that can be upgraded at this stage
      for (mmv5 in 1:nrow(opTestRv5)) {
        # build deck representing after upgrading that single card
        opDeckAfterRv5 <- opDeckBeforeRv5
        for (nnv5 in 1:nrow(opDeckAfterRv5)) {
          # if the card in the deck matches the card in question
          if (opDeckAfterRv5[nnv5,1] == opTestRv5[mmv5,1]) {
            opDeckAfterRv5[nnv5, 2] <- opDeckAfterRv5[nnv5, 2] + 1 # increase level by 1
          }
        }
        
        # build newx for after-upgrade deck
        newxOpAfterRv5 <- newxEmpty
        
        # process selected cards
        cardsInputOpRv5 <- opDeckAfterRv5$card
        
        for (oov5 in 1:length(cardsInputOpRv5)) {
          for (ppv5 in 1:length(names(newxOpAfterRv5))) {
            if (cardsInputOpRv5[oov5] == names(newxOpAfterRv5)[ppv5]) {
              newxOpAfterRv5[1,ppv5] <- 1
            }
          }
        }
        
        # process selected levels
        lvlInputOpAfterRv5 <- opDeckAfterRv5$level
        
        for (qqv5 in 1:length(cardsInputOpCleanRv5)) {
          for (rrv5 in 1:length(names(newxOpAfterRv5))) {
            if (cardsInputOpCleanRv5[qqv5] == names(newxOpAfterRv5)[rrv5]) {
              newxOpAfterRv5[1,rrv5] <- lvlInputOpAfterRv5[qqv5]
            }
          }
        }
        
        # now use newx for prediction
        opDeckAfterTrophiesRv5 <- as.integer(predict.lm(mod1, newxOpAfterRv5))
        
        
        
        # calculate gold spent for upgrading that card
        goldSpentOpRv5 <- costCalc(opTestRv5$card[mmv5], opTestRv5$levelPlus1[mmv5] - 1, opTestRv5$levelPlus1[mmv5])
        
        # fill out that row for stage upgrades df
        stageUpgradesRv5[mmv5,3] <- goldSpentOpRv5
        stageUpgradesRv5[mmv5,4] <- opDeckAfterTrophiesRv5 - opDeckBeforeTrophiesRv5
        stageUpgradesRv5[mmv5,5] <- stageUpgradesRv5[mmv5,4] / stageUpgradesRv5[mmv5,3] * 1000
      }
      
      
      
      # filter the row with the largest gain/gold ratio and add that to opRouteDF
      opUpgradeRv5 <-
        stageUpgradesRv5 %>%
        filter(trophyGainPer1000Gold == max(trophyGainPer1000Gold)) %>%
        slice(1) # make sure there's only one case
      
      opRouteDFRv5 <- bind_rows(opRouteDFRv5, opUpgradeRv5)
      
      # update opDeckv4 (fill in the card that is upgraded)
      for (ssv5 in 1:nrow(opDeckRv5)) {
        if (opDeckRv5[ssv5,1] == opUpgradeRv5[1,1]) {
          opDeckRv5[ssv5,2] <- opUpgradeRv5[1,2]
        }
      }
    }
    
    # print out the op route df
    opRouteDFRv5 %>%
      slice(-1) %>%
      mutate(
        Step = c(1:nrow(.)),
        Card = card,
        `Upgrade To` = as.integer(levelPlus1),
        `Gold Required` = as.integer(goldRequired),
        `Trophy Gain` = as.integer(trophyGain)
      ) %>%
      select(Step, Card, `Upgrade To`, `Gold Required`, `Trophy Gain`)
  })
  
  
  
  output$opRouteRv5 <- renderRHandsontable({
    rhandsontable(viewRouteTableRv5(), manualRowMove = TRUE)
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # for version 6 tab (potentially)
  
  
  
  
  
  
  
  
  
  
  
}

shinyApp(ui, server)
