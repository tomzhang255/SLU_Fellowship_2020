library(shiny)
library(shinydashboard)
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
ui <- dashboardPage(
  dashboardHeader(title = "Model 1"),
  
  dashboardSidebar(
    sidebarMenu(
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
      )
      
      
      
      # v3 tab content (potentially)
      
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
  
}

shinyApp(ui, server)
