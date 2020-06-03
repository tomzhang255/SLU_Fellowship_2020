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

# for card icons
cr_cards_s12 <- read_csv("cr_cards_s12.csv")
cr_cards_s12 <- 
  cr_cards_s12 %>%
  arrange(name)

cards_s12 <- cr_cards_s12$name



# the app
ui <- fluidPage(
  # suppress error messages
  # tags$style(type="text/css",
  #            ".shiny-output-error { visibility: hidden; }",
  #            ".shiny-output-error:before { visibility: hidden; }"
  # ),
  fluidRow(column(12, tags$h3("Predicting Trophies for Clash Royale Season 3"))),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      fluidRow(
        column(12, tags$h4("Deck 1"), align = "center")
      ),
      fluidRow(
        column(3, uiOutput(outputId = "card1Icon")),
        column(3, uiOutput(outputId = "card2Icon")),
        column(3, uiOutput(outputId = "card3Icon")),
        column(3, uiOutput(outputId = "card4Icon"))
      ),
      fluidRow(
        column(3, textOutput(outputId = "card1IconLvl"), align = "center"),
        column(3, textOutput(outputId = "card2IconLvl"), align = "center"),
        column(3, textOutput(outputId = "card3IconLvl"), align = "center"),
        column(3, textOutput(outputId = "card4IconLvl"), align = "center")
      ),
      fluidRow(
        column(3, uiOutput(outputId = "card5Icon")),
        column(3, uiOutput(outputId = "card6Icon")),
        column(3, uiOutput(outputId = "card7Icon")),
        column(3, uiOutput(outputId = "card8Icon"))
      ),
      fluidRow(
        column(3, textOutput(outputId = "card5IconLvl"), align = "center"),
        column(3, textOutput(outputId = "card6IconLvl"), align = "center"),
        column(3, textOutput(outputId = "card7IconLvl"), align = "center"),
        column(3, textOutput(outputId = "card8IconLvl"), align = "center")
      ),
      br(),
      fluidRow(
        column(12, tags$h4("Deck 2"), align = "center")
      ),
      fluidRow(
        column(3, uiOutput(outputId = "card1IconD2")),
        column(3, uiOutput(outputId = "card2IconD2")),
        column(3, uiOutput(outputId = "card3IconD2")),
        column(3, uiOutput(outputId = "card4IconD2"))
      ),
      fluidRow(
        column(3, textOutput(outputId = "card1IconLvlD2"), align = "center"),
        column(3, textOutput(outputId = "card2IconLvlD2"), align = "center"),
        column(3, textOutput(outputId = "card3IconLvlD2"), align = "center"),
        column(3, textOutput(outputId = "card4IconLvlD2"), align = "center")
      ),
      fluidRow(
        column(3, uiOutput(outputId = "card5IconD2")),
        column(3, uiOutput(outputId = "card6IconD2")),
        column(3, uiOutput(outputId = "card7IconD2")),
        column(3, uiOutput(outputId = "card8IconD2"))
      ),
      fluidRow(
        column(3, textOutput(outputId = "card5IconLvlD2"), align = "center"),
        column(3, textOutput(outputId = "card6IconLvlD2"), align = "center"),
        column(3, textOutput(outputId = "card7IconLvlD2"), align = "center"),
        column(3, textOutput(outputId = "card8IconLvlD2"), align = "center")
      )
    ),
    mainPanel(
      tabsetPanel(
        id = "inTabset",
        tabPanel(
          title = "Deck Comparison",
          fluidRow(
            column(4,
                   fluidRow(
                     column(12, tags$h4(tags$span(style="color:DodgerBlue", "Deck 1: Original Deck")), align = "center")
                   ),
                   fluidRow(
                     column(8, selectInput(inputId = "card1Lv5", label = "Card 1", choices = cards, selected = "Baby Dragon")),
                     column(4, numericInput(inputId = "card1LvlLv5", label = "Level", min = 1, max = 13, value = 11))
                   ),
                   fluidRow(
                     column(8, selectInput(inputId = "card2Lv5", label = "Card 2", choices = cards, selected = "Goblin Hut")),
                     column(4, numericInput(inputId = "card2LvlLv5", label = "Level", min = 1, max = 13, value = 11))
                   ),
                   fluidRow(
                     column(8, selectInput(inputId = "card3Lv5", label = "Card 3", choices = cards, selected = "Graveyard")),
                     column(4, numericInput(inputId = "card3LvlLv5", label = "Level", min = 1, max = 13, value = 10))
                   ),
                   fluidRow(
                     column(8, selectInput(inputId = "card4Lv5", label = "Card 4", choices = cards, selected = "Knight")),
                     column(4, numericInput(inputId = "card4LvlLv5", label = "Level", min = 1, max = 13, value = 11))
                   ),
                   fluidRow(
                     column(8, selectInput(inputId = "card5Lv5", label = "Card 5", choices = cards, selected = "The Log")),
                     column(4, numericInput(inputId = "card5LvlLv5", label = "Level", min = 1, max = 13, value = 11))
                   ),
                   fluidRow(
                     column(8, selectInput(inputId = "card6Lv5", label = "Card 6", choices = cards, selected = "Musketeer")),
                     column(4, numericInput(inputId = "card6LvlLv5", label = "Level", min = 1, max = 13, value = 13))
                   ),
                   fluidRow(
                     column(8, selectInput(inputId = "card7Lv5", label = "Card 7", choices = cards, selected = "Poison")),
                     column(4, numericInput(inputId = "card7LvlLv5", label = "Level", min = 1, max = 13, value = 11))
                   ),
                   fluidRow(
                     column(8, selectInput(inputId = "card8Lv5", label = "Card 8", choices = cards, selected = "Skeletons")),
                     column(4, numericInput(inputId = "card8LvlLv5", label = "Level", min = 1, max = 13, value = 10))
                   )
            ),
            column(4,
                   fluidRow(
                     column(12, tags$h3("Predicted Trophies"), align = "center")
                   ),
                   tags$br(),
                   fluidRow(
                     column(12, plotOutput(outputId = "barv5"))
                   )
            ),
            column(4,
                   fluidRow(
                     column(12, tags$h4(tags$span(style="color:DarkBlue", "Deck 2: Modifications to Deck 1")), align = "center")
                   ),
                   fluidRow(
                     column(8, htmlOutput(outputId = "card1RSelectorv5")),
                     column(4, htmlOutput(outputId = "card1LvlRSelectorv5"))
                   ),
                   fluidRow(
                     column(8, htmlOutput(outputId = "card2RSelectorv5")),
                     column(4, htmlOutput(outputId = "card2LvlRSelectorv5"))
                   ),
                   fluidRow(
                     column(8, htmlOutput(outputId = "card3RSelectorv5")),
                     column(4, htmlOutput(outputId = "card3LvlRSelectorv5"))
                   ),
                   fluidRow(
                     column(8, htmlOutput(outputId = "card4RSelectorv5")),
                     column(4, htmlOutput(outputId = "card4LvlRSelectorv5"))
                   ),
                   fluidRow(
                     column(8, htmlOutput(outputId = "card5RSelectorv5")),
                     column(4, htmlOutput(outputId = "card5LvlRSelectorv5"))
                   ),
                   fluidRow(
                     column(8, htmlOutput(outputId = "card6RSelectorv5")),
                     column(4, htmlOutput(outputId = "card6LvlRSelectorv5"))
                   ),
                   fluidRow(
                     column(8, htmlOutput(outputId = "card7RSelectorv5")),
                     column(4, htmlOutput(outputId = "card7LvlRSelectorv5"))
                   ),
                   fluidRow(
                     column(8, htmlOutput(outputId = "card8RSelectorv5")),
                     column(4, htmlOutput(outputId = "card8LvlRSelectorv5"))
                   )
            )
          ),
        ),
        tabPanel(
          title = "Upgrade Route",
          useShinyalert(),
          fluidRow(
            column(12, tags$h4("Optimized Upgrade Route for Deck 1"))
          ),
          tags$br(),
          fluidRow(
            column(4, actionButton(inputId = "viewUpdate", label = "View Optimized Upgrade Route / Update Plot")),
            column(1, actionButton(inputId = "reset", label = "Reset"))
          ),
          tags$br(),
          fluidRow(
            column(7,
                   fluidRow(
                     column(12, rHandsontableOutput(outputId = "opRouteLv5")),
                     column(12, tags$h5(textOutput(outputId = "noteRoute")))
                   )
            ),
            column(5, plotOutput(outputId = "geomLine"))
          ),
          fluidRow(# experimental
            column(12, rHandsontableOutput(outputId = "tableOut"))
          )
        )
      )
    )
  )
)



server <- function(input, output, session) {
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
    paste("Level", input$card1LvlLv5)
  })
  
  output$card2IconLvl <- renderText({
    paste("Level", input$card2LvlLv5)
  })
  
  output$card3IconLvl <- renderText({
    paste("Level", input$card3LvlLv5)
  })
  
  output$card4IconLvl <- renderText({
    paste("Level", input$card4LvlLv5)
  })
  
  output$card5IconLvl <- renderText({
    paste("Level", input$card5LvlLv5)
  })
  
  output$card6IconLvl <- renderText({
    paste("Level", input$card6LvlLv5)
  })
  
  output$card7IconLvl <- renderText({
    paste("Level", input$card7LvlLv5)
  })
  
  output$card8IconLvl <- renderText({
    paste("Level", input$card8LvlLv5)
  })
  
  
  
  # for card icon levels of right deck
  output$card1IconLvlD2 <- renderText({
    paste("Level", input$card1LvlRv5)
  })
  
  output$card2IconLvlD2 <- renderText({
    paste("Level", input$card2LvlRv5)
  })
  
  output$card3IconLvlD2 <- renderText({
    paste("Level", input$card3LvlRv5)
  })
  
  output$card4IconLvlD2 <- renderText({
    paste("Level", input$card4LvlRv5)
  })
  
  output$card5IconLvlD2 <- renderText({
    paste("Level", input$card5LvlRv5)
  })
  
  output$card6IconLvlD2 <- renderText({
    paste("Level", input$card6LvlRv5)
  })
  
  output$card7IconLvlD2 <- renderText({
    paste("Level", input$card7LvlRv5)
  })
  
  output$card8IconLvlD2 <- renderText({
    paste("Level", input$card8LvlRv5)
  })
  
  
  
  # reactive values
  values <- reactiveValues()
  
  
  
  viewCounter <- 0
  
  
  
  # for bar plot
  output$barv5 <- renderPlot({
    viewCounter <<- 0
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
    
    values$leftTrophies <- predictedTrophiesLv5
    
    
    
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
  observeEvent(input$viewUpdate, {
    viewCounter <<- viewCounter + 1
    output$opRouteLv5 <- renderRHandsontable({
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
      opRouteDFLv5 <-
        opRouteDFLv5 %>%
        slice(-1) %>%
        mutate(
          Step = c(1:nrow(.)),
          Card = card,
          `Upgrade To` = as.integer(levelPlus1),
          `Gold Required` = as.integer(goldRequired),
          `Trophy\nGain` = as.integer(trophyGain),
          `Trophy Gain\n/ 1000 Gold` = trophyGainPer1000Gold
        ) %>%
        select(Step, Card, `Upgrade To`, `Gold Required`, `Trophy\nGain`, `Trophy Gain\n/ 1000 Gold`)
      
      values$hot <<- NULL
      values$hot <<- rhandsontable(opRouteDFLv5, manualRowMove = TRUE)
    })
    
  })
  
  
  
  # route note
  output$noteRoute <- renderText({
    if (is.null(input$opRouteLv5)) {
      return (NULL)
    } else {
      "Note: The rows in the table are adjustable and can be used to change the order if some upgrades are feasible."
    }
  })
  
  
  
  # save route as sorted df
  observe({
    if(!is.null(input$opRouteLv5)) {
      values$DF <- hot_to_r(input$opRouteLv5)
      isolate(values$DF <-
        values$DF %>%
          arrange(desc(`Trophy Gain\n/ 1000 Gold`))
      )
    }
  })
  
  
  
  # for line plot
  output$geomLine <- renderPlot({
    if (is.null(input$opRouteLv5)) {
      return(NULL)
    } else {
      if (viewCounter == 1 | viewCounter == 0) {
        data <- values$DF
        data[, "Cumulative_Gold"] <- cumsum(data$`Gold Required`)
        data[, "Trophies"] <- cumsum(data$`Trophy\nGain`) + values$leftTrophies
        options(scipen=10000)
        data %>%
          ggplot(., aes(x = Cumulative_Gold, y = Trophies)) +
          geom_line() +
          theme_bw() +
          labs(
            x = "Cumulative Gold Spent",
            y = "Trophies",
            title = "Line Plot of Trophies vs Cumulative Gold Spent"
          ) +
          theme(
            axis.title = element_text(size = rel(1.3)),
            axis.text = element_text(size = rel(1.3)),
            plot.title = element_text(size = rel(1.3))
          )
      } else {
        if (!is.null(values$DF)) {
          data1 <- values$DF
          data1[, "Cumulative_Gold"] <- cumsum(data1$`Gold Required`)
          data1[, "Trophies"] <- cumsum(data1$`Trophy\nGain`) + values$leftTrophies
          data1[, "Route"] <- "Optimized Route"
          
          data2 <- hot_to_r(input$opRouteLv5)
          data2[, "Cumulative_Gold"] <- cumsum(data2$`Gold Required`)
          data2[, "Trophies"] <- cumsum(data2$`Trophy\nGain`) + values$leftTrophies
          data2[, "Route"] <- "Adjusted Route"
          
          newData <- bind_rows(data1, data2)
          newData$Route <- fct_inorder(newData$Route)
          
          options(scipen=10000)
          ggplot(newData, aes(x = Cumulative_Gold, y = Trophies, color = Route)) +
            geom_line() +
            theme_bw() +
            labs(
              x = "Cumulative Gold Spent",
              y = "Trophies",
              title = "Line Plot of Trophies vs Cumulative Gold Spent"
            ) +
            scale_color_manual(values = c("black", "red")) +
            theme(
              legend.position = "bottom",
              axis.title = element_text(size = rel(1.3)),
              axis.text = element_text(size = rel(1.3)),
              plot.title = element_text(size = rel(1.3)),
              legend.title = element_blank(),
              legend.text = element_text(size = rel(1.15))
            )
        } else {
          data <- hot_to_r(input$opRouteLv5)
          data[, "Cumulative_Gold"] <- cumsum(data$`Gold Required`)
          data[, "Trophies"] <- cumsum(data$`Trophy\nGain`) + values$leftTrophies
          options(scipen=10000)
          data %>%
            ggplot(., aes(x = Cumulative_Gold, y = Trophies)) +
            geom_line() +
            theme_bw() +
            labs(
              x = "Cumulative Gold Spent",
              y = "Trophies",
              title = "Line Plot of Trophies vs Cumulative Gold Spent"
            ) +
            theme(
              axis.title = element_text(size = rel(1.3)),
              axis.text = element_text(size = rel(1.3)),
              plot.title = element_text(size = rel(1.3))
            )
        }
      }
    }
  })
  
  
  
  # reset button
  observeEvent(input$reset, {
    viewCounter <<- 0
    output$opRouteLv5 <- NULL
    output$opRouteLv5 <- renderRHandsontable({
      values$hot <- NULL
      values$hot <- rhandsontable(values$DF, manualRowMove = F)
      values$hot
    })
  })
  
}

shinyApp(ui, server)
