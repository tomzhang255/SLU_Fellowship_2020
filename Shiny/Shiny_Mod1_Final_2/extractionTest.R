library(shiny)
library(rvest)

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



ui <- fluidPage(
  textInput(inputId = "playerTag", label = "Player Tag", value = "#9YJUPU9LY"),
  textOutput(outputId = "card1"),
  textOutput(outputId = "card2"),
  textOutput(outputId = "card3"),
  textOutput(outputId = "card4")
  
)

server <- function(input, output, session) {
  #values <- reactiveValues()
  #values$playerDeck <- extractCard(input$playerTag)

  output$card1 <- renderText({
    extractCard(input$playerTag)[[1,1]]
  })
  
  output$card2 <- renderText({
    extractCard(input$playerTag)[[2,1]]
  })
  
  output$card3 <- renderText({
    extractCard(input$playerTag)[[3,1]]
  })
  
  output$card4 <- renderText({
    extractCard(input$playerTag)[[4,1]]
  })
  
}

shinyApp(ui, server)