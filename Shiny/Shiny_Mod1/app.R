library(shiny)
library(tidyverse)

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



# the app
ui <- fluidPage(
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
)



server <- function(input, output, session) {
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
}

shinyApp(ui, server)
