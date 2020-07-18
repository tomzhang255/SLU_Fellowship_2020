library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)
library(rvest)

# helper function
trophy_reset <- function(x){
    gates <- c(0, 300, 600, 1000, 1300, 1600, 2000, 2300, 2600, 3000, 3300, 3600, 4000)
    if(x < 4000)  lb <- gates[which(x - gates < 0)[1] - 1]
    
    if(x > 4000) lb = ceiling(x - (x - 4000)/2)
    
    lb
}

#load necessary data
cardProfiles <- read_csv("cr_allcard_profiles_s12.csv")
card_names <- unique(cardProfiles$card)


# Define UI for application that draws a histogram
ui = navbarPage("Clash Royale",
                tabPanel("Card Trophy Profile",
                         sidebarLayout(
                             sidebarPanel(
                                 
                                 # Include clarifying text ----
                                 helpText('This tab will allow you to investigate the 
                                   win rates your current deck in a trophy range near you. Grab your player tag*, choose you trophy range and click on the button below to prodice a plot 
                                      of Win Percentages for each card. It uses your deck information from "Royale API" 
                                          in combination with our own data and analyses to investigate your results.'),
                                 helpText("Not sure where you Player Tag is? Click on your Profile and it is 
                                          just below your name. You can copy it to your clipboard just by clicking on it."),
                                 
                                 textInput(inputId = "playerTag",
                                           "Player ID", 
                                           placeholder = "Tag #XXXXXX", ),
                                 
                                 actionButton("profileGo","Plot my Cards!")
                                 
                                 
                             ), # closes sidebarPanel
                             mainPanel(
                                 plotlyOutput("cardPlot")
                             ) #closes mainPanel
                         ) # closes sidebarLayout
                ) # closes tabPanel
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    data <- eventReactive(input$profileGo, {
    
        IDtag = stringr::str_remove(input$playerTag, pattern = "#")
        #url2 <- paste('https://royaleapi.com/player/',IDtag, sep = '')
        url2 <- "https://royaleapi.com/player/9YJUPU9LY"
        
        #Reading the HTML code from the website
        webpage2 <- read_html(url2)
        card_data_html <- html_nodes(webpage2,'[class="deck_card  ui image"]')
        level_data_html <- html_nodes(webpage2,'.cardlevel')
        cardselected <- html_attr(card_data_html, "alt")
        levels <- readr::parse_number(html_text(level_data_html))

        trophy_data_html <- html_nodes(webpage2,'.item') 
        trophy_player <- trophy_data_html %>% html_text %>% stringr::str_detect(., pattern = " PB") %>% trophy_data_html[.] %>%html_text %>%
            stringr::str_split(pattern = "/", simplify = T) %>% readr::parse_number()
        
        minTrophy = trophy_reset(trophy_player[1])
        maxTrophy = trophy_player[2] + 300
        
        

        cards2use <- data.frame(card = cardselected, level = levels, stringsAsFactors = FALSE) #%>% 
            #filter(card != '') %>%
            #mutate(level = as.numeric(level))
        
        
        dat <- cardProfiles %>%
            filter(trophies < maxTrophy) %>%
            filter(trophies >= minTrophy) %>%
            #left_join(., card_info, by  = c("card"="name")) %>% 
            #mutate(level = level + levelAdjust) %>%
            semi_join(., cards2use, by = c("card", "level")) %>%
            mutate(win.rate = round(100* win/(win+lose), 1),
                   cardlevel = paste(card, ' (',level,')', sep = '')
            ) %>%
            select(cardlevel, trophies, win.rate)
        res = list(x = dat, trophies=trophy_player)
        res
    })
    
    output$cardPlot <- renderPlotly({
        #     if(is.null(input$cardselected)) return()
        
        x <- data()$x
        
        g <- ggplot(data = x, aes(x = trophies, y = win.rate, color = cardlevel)) +
            geom_hline(yintercept = 50, linetype = 2) +
            geom_vline(xintercept = data()$trophies[1], color = "grey") +
            annotate("text", x=data()$trophies[1], y=0, label= "Current Trophies")
        
        if(data()$trophies[2] > data()$trophies[1]){
            g <- g +   
                geom_vline(xintercept = data()$trophies[2], color = "grey") +
                annotate("text", x=data()$trophies[2], y=10, label= "Personal Best")            
        }

        g <- g + geom_smooth(se = F, size = 2)  +
            scale_color_brewer(palette = "Dark2", name = "Card (Level)") +
            theme_bw() +
            xlab("Trophies") + ylab("Win Rate")
        #ggplotly(g, tooltip = c("card"))
        ggplotly(g)
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
