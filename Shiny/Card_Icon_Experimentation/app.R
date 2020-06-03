library(shiny)
library(tidyverse)

cr_cards_s12 <- read_csv("cr_cards_s12.csv")
cr_cards_s12 <- 
  cr_cards_s12 %>%
  arrange(name)

cards_s12 <- cr_cards_s12$name



ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(3, selectInput(inputId = "card1", label = "Card 1", choices = cards_s12, selected = "Wizard")),
        column(3, uiOutput(outputId = "card1Icon"))
      )
    ),
    mainPanel(
      "NA"
    )
  )
)



server <- function(input, output, session) {
  # output$card1Icon <- renderText({
  #   for (i in 1:length(cards)) {
  #     if (cards[i] == input$card1) {
  #       iconURL <-  cr_cards_s12$iconUrl[i]
  #     }
  #   }
  #   c('<img src="',iconURL,'">')
  # })
  
  output$card1Icon <- renderUI({
    for (i in 1:length(cards_s12)) {
      if (cards_s12[i] == input$card1) {
        iconURL <-  cr_cards_s12$iconUrl[i]
      }
    }
    
    tags$img(src = iconURL, width = "100%", height = "auto")
  })
  
  
  
  
  
  
  
  
  
}

shinyApp(ui, server)