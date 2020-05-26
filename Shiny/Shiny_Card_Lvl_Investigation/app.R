library(shiny)
library(tidyverse)
coefs_clean <- read_csv("coefs_clean.csv")
coefs_clean$Card <- factor(coefs_clean$Card)
cardOptions <- levels(coefs_clean$Card)

ui <- fluidPage(
  titlePanel("Investigating How Level Increases Impact Trophy Changes"),
  selectInput(inputId = "card", label = "Select a Card", choices = cardOptions, selected = cardOptions[1]),
  plotOutput(outputId = "point")
)

server <- function(input, output, session) {
  output$point <- renderPlot({
    # to filter rows based on selected card
    selectedData <- filter(coefs_clean, Card == input$card)
    # the plot
    ggplot(selectedData, aes(x = selectedData$Level, y = selectedData$Slope)) +
      geom_point() +
      geom_smooth(se = F, method = "loess", color = "blue") +
      geom_smooth(se = F, method = "lm", color = "red") +
      labs(x = "Level", y = "Coefficient",
           title = "Scatterplot of Model Coefficient vs Card Level",
           subtitle = "From the Model Based on Indicators of Each Card and Card-Level Combination to Predict Trophies") +
      theme_bw()
  })
}

shinyApp(ui, server)