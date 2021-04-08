library(mwshiny)
library(shiny)
library(shinyMobile)
library(tidyverse)
library(shinyalert)
library(rhandsontable)
library(rvest)
library(plotly)
#library(shinyBS)

# the model
mod3 <- readRDS("mod_4000_s21_lm_2_strip.rds")
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
  mutate(name = str_replace(name, "^Heal$", "Heal Spirit")) %>%
  # season 12: skeleton dragons
  bind_rows(., tibble(name = "Skeleton Dragons", rarity = "Common")) %>%
  # season 16: Electro Giant
  bind_rows(., tibble(name = "Electro Giant", rarity = "Epic")) %>%
  # season 16: Electro Spirit
  bind_rows(., tibble(name = "Electro Spirit", rarity = "Common")) %>%
  # season 18: Mother Witch
  bind_rows(., tibble(name = "Mother Witch", rarity = "Legendary"))

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
cr_cards_s21 <- read_csv("cr_cards_s21.csv")
cr_cards_s21 <- 
  cr_cards_s21 %>%
  arrange(name)

cards_s21 <- cr_cards_s21$name

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

# helper function
trophy_reset <- function(x){
  gates <- c(0, 300, 600, 1000, 1300, 1600, 2000, 2300, 2600, 3000, 3300, 3600, 4000)
  if(x < 4000)  lb <- gates[which(x - gates < 0)[1] - 1]
  
  if(x > 4000) lb = ceiling(x - (x - 4000)/2)
  
  lb
}

# card profiles for win rate plot
cardProfiles <- read_csv("cr_allcard_profiles_s21.csv")







# modify window selector page
mwsSelectorPage2 <- function(win_titles) {
  #  get each of the windows to select
  win_select <- lapply(seq_along(win_titles), function(i) {
    # get each of the titles
    win_title <- win_titles[[i]]
    # create a link for each window
    tags$h2(
      tags$a(href = paste0("?",win_title),
             paste(i, ". ", win_title, sep = "")
      ),
      align = "center"
    )
  })
  
  # create and return the window selector page
  shiny::bootstrapPage(
    shiny::div(class = "Window",
               shiny::div(
                 style = htmltools::css(
                   position = "absolute", top = "50%", left = "50%",
                   margin_right = "-50%", transform = "translate(-50%, -50%)"),
                 list(tags$h1("Pick Your Device:", align = "center"), tags$br(), win_select[1], tags$br(), win_select[2]) # specific for 2-window app
               )
    )
  )
}

environment(mwsSelectorPage2) <- asNamespace('mwshiny')
assignInNamespace("mwsSelectorPage", mwsSelectorPage2, ns = "mwshiny")







# UI
ui_list <- list()



ui_list[["Desktop"]] <- fluidPage(
  # suppress error messages
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  useShinyalert(),
  fluidRow(column(12, tags$h3("Clash Royale Card-Upgrade Investigation"))),
  sidebarLayout(
    conditionalPanel(
      condition = "input.tabs != 'Info'",
      sidebarPanel(
        width = 3,
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
      )
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "tabs",
        tabPanel(
          title = "Info",
          fluidRow(column(12, tags$h3(tags$span(style = "color:firebrick", "Introduction")))),
          fluidRow(column(12, tags$h4("The purpose of this app is to investigate the most effective card-upgrade strategy in Clash Royale. It consists of three main features: deck comparison, upgrade route, and win rate investigation."))),
          fluidRow(column(12, tags$h4(tags$em("Note: This app was last updated on 04/08/2021")))),
          fluidRow(column(12, tags$h4(tags$em(tagList('Have any feedback to help us improve the site? Please visit the Google Form at:', tags$a("https://forms.gle/et4joTJgr151Chqk9", href = "https://forms.gle/et4joTJgr151Chqk9")))))),
          br(),
          fluidRow(column(12, tags$h3(tags$span(style = "color:firebrick", "Deck Comparison")))),
          fluidRow(column(12, tags$h4('Under the "Deck Comparison" tab, the user will be able to build two decks and compare their trophies predicted by our model. To build Deck 1, the user can either select 8 cards manually or load a deck with their player tag. As for Deck 2, its composition depends on Deck 1. That is, whenever Deck 1 is altered, Deck 2 gets updated automatically. However, directly modifying Deck 2 will not affect Deck 1. Further, the user can select a stage of the season for all predictions.'))),
          fluidRow(column(12, tags$h4(tags$em("Note: New cards are not available in this app until after they have been in play at least one full season; please select any substitutes accordingly.")))),
          br(),
          fluidRow(column(12, tags$h3(tags$span(style = "color:firebrick", "Upgrade Route")))),
          fluidRow(column(12, tags$h4('Under the "Upgrade Route" tab, the app will generate a table of the optimized upgrade route for Deck 1. Each row will be an available upgrade for the deck, until every card has reached maximum level. The suggested steps are arranged based on the cost-effectiveness of the upgrades (trophy gain per 1000 gold spent). To visualize the upgrade route, this app also provides a plot of trophies vs cumulative gold spent.'))),
          fluidRow(column(12, tags$h4(tags$em('Note: The rows in the table are adjustable; simply drag them around to change the order. Then, click on "Update Plot" button to see how the path changes.')))),
          br(),
          fluidRow(column(12, tags$h3(tags$span(style = "color:firebrick", "Win Rate Investigation")))),
          fluidRow(column(12, tags$h4('Under this tab, the user will be able to investigate the win rates of Deck 1 in an appropriate trophy range.')))
        ),
        tabPanel(
          title = "Deck Comparison",
          fluidRow(
            column(4,
                   # adding manual selection back
                   fluidRow(
                     column(12, tags$h4(tags$span(style="color:DodgerBlue", "Deck 1: Original Deck")), align = "center")
                   ),
                   fluidRow(
                     column(8, htmlOutput(outputId = "card1LSelectorv5")),
                     column(4, htmlOutput(outputId = "card1LvlLSelectorv5"))
                   ),
                   fluidRow(
                     column(8, htmlOutput(outputId = "card2LSelectorv5")),
                     column(4, htmlOutput(outputId = "card2LvlLSelectorv5"))
                   ),
                   fluidRow(
                     column(8, htmlOutput(outputId = "card3LSelectorv5")),
                     column(4, htmlOutput(outputId = "card3LvlLSelectorv5"))
                   ),
                   fluidRow(
                     column(8, htmlOutput(outputId = "card4LSelectorv5")),
                     column(4, htmlOutput(outputId = "card4LvlLSelectorv5"))
                   ),
                   fluidRow(
                     column(8, htmlOutput(outputId = "card5LSelectorv5")),
                     column(4, htmlOutput(outputId = "card5LvlLSelectorv5"))
                   ),
                   fluidRow(
                     column(8, htmlOutput(outputId = "card6LSelectorv5")),
                     column(4, htmlOutput(outputId = "card6LvlLSelectorv5"))
                   ),
                   fluidRow(
                     column(8, htmlOutput(outputId = "card7LSelectorv5")),
                     column(4, htmlOutput(outputId = "card7LvlLSelectorv5"))
                   ),
                   fluidRow(
                     column(8, htmlOutput(outputId = "card8LSelectorv5")),
                     column(4, htmlOutput(outputId = "card8LvlLSelectorv5"))
                   )
            ),
            # layout modifications . . . 
            column(4,
                   fluidRow(
                     column(12, tags$h3("Predicted Trophies"), align = "center")
                   ),
                   fluidRow(
                     column(12, plotOutput(outputId = "barv5", height = "300px"))
                   ),
                   fluidRow(column(12, tags$h5(tags$span(style = "color:black", textOutput(outputId = "interpretNote"))))),
                   #br(),
                   fluidRow(column(12, tags$h5(tags$span(style = "color:Firebrick", "Select a Stage of the Season for Predictions")))),
                   fluidRow(column(12, selectInput(inputId = "seasonStage", label = NULL, choices = c("Early Season", "Mid Season", "Late Season"), selected = "Late Season"))),
                   fluidRow(column(12, tags$h5(tags$span(style = "color:Firebrick", "Load Deck 1 with Player Tag")))),
                   fluidRow(
                     column(6, textInput(inputId = "playerTag", label = NULL, value = "")),
                     column(6, actionButton(inputId = "updateDeck", label = "Load Deck"))
                   ),
                   fluidRow(column(12, tags$h5("Try Tommy Z-7's Deck: #9YJUPU9LY"))),
                   fluidRow(column(12, tags$h5("Try Morderth's Deck: #9U9Y02GPL"))),
                   fluidRow(column(12, tags$h5("Try PatienceV's Deck: #P9Y9VV2VC")))
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
          )
        ),
        tabPanel(
          title = "Upgrade Route",
          #useShinyalert(),
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
                   fluidRow(column(12, rHandsontableOutput(outputId = "opRouteLv5"))),
                   fluidRow(column(12, tags$h5(textOutput(outputId = "noteRoute"))))
            ),
            column(5,
                   fluidRow(column(12, tags$h4(textOutput(outputId = "plotTitle")))),
                   fluidRow(column(12, plotlyOutput(outputId = "geomLine")))
            )
          )
        ),
        tabPanel(
          title = "Win Rate Investigation",
          fluidRow(
            column(12, tags$h4("Win Rate Investigation for Deck 1"))
          ),
          br(),
          fluidRow(column(12, actionButton(inputId = "viewWinRate", label = "View / Update Win Rate Plot"))),
          br(),
          fluidRow(
            column(12, plotlyOutput("winRatePlot"))
          )
        )
      )
    )
  )
)



ui_list[["Mobile"]] <- f7Page(
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  title = "shinyMobile",
  init = f7Init(
    skin = "auto",
    theme = "light"
  ),
  f7TabLayout(
    navbar = f7Navbar(
      title = "Clash Royale",
      subtitle = "Card-Upgrade Investigation",
      hairline = TRUE,
      shadow = TRUE
    ),
    f7Tabs(
      animated = TRUE,
      id = "tabs",
      f7Tab(
        tabName = "Info",
        icon = f7Icon("info_circle_fill"),
        f7Card(
          title = tags$span(style = "color:firebrick", "Introduction"),
          "The purpose of this app is to investigate the most effective card-upgrade strategy in Clash Royale. It consists of three main features: deck comparison, upgrade route, and win rate investigation.",
          footer = 
            tags$em("Note: This app was last updated on 04/08/2021.", tagList('And if you have any feedback to help us improve the site, please visit the Google Form at:', f7Link(label = "https://forms.gle/et4joTJgr151Chqk9", src = "https://forms.gle/et4joTJgr151Chqk9", external = TRUE)))
        ),
        f7Card(
          title = tags$span(style = "color:firebrick", "Deck Comparison"),
          'Under the "Deck Comparison" tab, the user will be able to build two decks and compare their trophies predicted by our model. To build Deck 1, the user can either select 8 cards manually or load a deck with their player tag. As for Deck 2, its composition depends on Deck 1. That is, whenever Deck 1 is altered, Deck 2 gets updated automatically. However, directly modifying Deck 2 will not affect Deck 1. Further, the user can select a stage of the season for all predictions.',
          footer = tags$em("Note: New cards are not available in this app until after they have been in play at least one full season; please select any substitutes accordingly.")
        ),
        f7Card(
          title = tags$span(style = "color:firebrick", "Upgrade Route"),
          'Under the "Upgrade Route" tab, the app will generate a table of the optimized upgrade route for Deck 1. Each row will be an available upgrade for the deck, until every card has reached maximum level. The suggested steps are arranged based on the cost-effectiveness of the upgrades (trophy gain per 1000 gold spent). To visualize the upgrade route, this app also provides a plot of trophies vs cumulative gold spent.',
          footer = tags$em('Note: The rows in the table are adjustable; simply drag them around to change the order. Then, click on "Update Plot" button to see how the path changes.')
        ),
        f7Card(
          title = tags$span(style = "color:firebrick", "Win Rate Investigation"),
          'Under this tab, the user will be able to investigate the win rates of Deck 1 in an appropriate trophy range.'
        )
      ),
      f7Tab(
        tabName = "Deck 1",
        icon = f7Icon("layers_alt_fill"),
        f7Card(
          f7Col(
            f7Text(inputId = "playerTagM", label = "Load with Player Tag", value = "#9YJUPU9LY", placeholder = "#9YJUPU9LY"),
            f7Button(inputId = "updateDeckM", label = "Load Deck"),
            align = "center"
          ),
          footer = tagList(
            f7Badge("#9YJUPU9LY"),
            f7Badge("#9U9Y02GPL"),
            f7Badge("#P9Y9VV2VC")
          )
        ),
        f7Card(
          f7Picker(inputId = "seasonStageM", label = "Select a Stage of the Season", placeholder = NULL, choices = c("Early Season", "Mid Season", "Late Season"), value = "Late Season", openIn = "sheet"),
          align = "center"
        ),
        f7Card(
          htmlOutput(outputId = "card1LSelectorv5M"),
          htmlOutput(outputId = "card1LvlLSelectorv5M"),
          align = "center"
        ),
        f7Card(
          htmlOutput(outputId = "card2LSelectorv5M"),
          htmlOutput(outputId = "card2LvlLSelectorv5M"),
          align = "center"
        ),
        f7Card(
          htmlOutput(outputId = "card3LSelectorv5M"),
          htmlOutput(outputId = "card3LvlLSelectorv5M"),
          align = "center"
        ),
        f7Card(
          htmlOutput(outputId = "card4LSelectorv5M"),
          htmlOutput(outputId = "card4LvlLSelectorv5M"),
          align = "center"
        ),
        f7Card(
          htmlOutput(outputId = "card5LSelectorv5M"),
          htmlOutput(outputId = "card5LvlLSelectorv5M"),
          align = "center"
        ),
        f7Card(
          htmlOutput(outputId = "card6LSelectorv5M"),
          htmlOutput(outputId = "card6LvlLSelectorv5M"),
          align = "center"
        ),
        f7Card(
          htmlOutput(outputId = "card7LSelectorv5M"),
          htmlOutput(outputId = "card7LvlLSelectorv5M"),
          align = "center"
        ),
        f7Card(
          htmlOutput(outputId = "card8LSelectorv5M"),
          htmlOutput(outputId = "card8LvlLSelectorv5M"),
          align = "center"
        )
      ),
      f7Tab(
        tabName = "Deck 2",
        icon = f7Icon("layers_alt_fill"),
        f7Card(
          htmlOutput(outputId = "card1RSelectorv5M"),
          htmlOutput(outputId = "card1LvlRSelectorv5M"),
          align = "center"
        ),
        f7Card(
          htmlOutput(outputId = "card2RSelectorv5M"),
          htmlOutput(outputId = "card2LvlRSelectorv5M"),
          align = "center"
        ),
        f7Card(
          htmlOutput(outputId = "card3RSelectorv5M"),
          htmlOutput(outputId = "card3LvlRSelectorv5M"),
          align = "center"
        ),
        f7Card(
          htmlOutput(outputId = "card4RSelectorv5M"),
          htmlOutput(outputId = "card4LvlRSelectorv5M"),
          align = "center"
        ),
        f7Card(
          htmlOutput(outputId = "card5RSelectorv5M"),
          htmlOutput(outputId = "card5LvlRSelectorv5M"),
          align = "center"
        ),
        f7Card(
          htmlOutput(outputId = "card6RSelectorv5M"),
          htmlOutput(outputId = "card6LvlRSelectorv5M"),
          align = "center"
        ),
        f7Card(
          htmlOutput(outputId = "card7RSelectorv5M"),
          htmlOutput(outputId = "card7LvlRSelectorv5M"),
          align = "center"
        ),
        f7Card(
          htmlOutput(outputId = "card8RSelectorv5M"),
          htmlOutput(outputId = "card8LvlRSelectorv5M"),
          align = "center"
        )
      ),
      f7Tab(
        tabName = "Decks",
        icon = f7Icon("tray_2_fill"),
        f7Card(
          title = "Deck 1",
          f7Row(
            f7Col(uiOutput(outputId = "card1IconM")),
            f7Col(uiOutput(outputId = "card2IconM")),
            f7Col(uiOutput(outputId = "card3IconM")),
            f7Col(uiOutput(outputId = "card4IconM"))
          ),
          f7Row(
            f7Col(textOutput(outputId = "card1IconLvlM"), align = "center"),
            f7Col(textOutput(outputId = "card2IconLvlM"), align = "center"),
            f7Col(textOutput(outputId = "card3IconLvlM"), align = "center"),
            f7Col(textOutput(outputId = "card4IconLvlM"), align = "center")
          ),
          f7Row(
            f7Col(uiOutput(outputId = "card5IconM")),
            f7Col(uiOutput(outputId = "card6IconM")),
            f7Col(uiOutput(outputId = "card7IconM")),
            f7Col(uiOutput(outputId = "card8IconM"))
          ),
          f7Row(
            f7Col(textOutput(outputId = "card5IconLvlM"), align = "center"),
            f7Col(textOutput(outputId = "card6IconLvlM"), align = "center"),
            f7Col(textOutput(outputId = "card7IconLvlM"), align = "center"),
            f7Col(textOutput(outputId = "card8IconLvlM"), align = "center")
          )
        ),
        f7Card(
          title = "Deck 2",
          f7Row(
            f7Col(uiOutput(outputId = "card1IconD2M")),
            f7Col(uiOutput(outputId = "card2IconD2M")),
            f7Col(uiOutput(outputId = "card3IconD2M")),
            f7Col(uiOutput(outputId = "card4IconD2M"))
          ),
          f7Row(
            f7Col(textOutput(outputId = "card1IconLvlD2M"), align = "center"),
            f7Col(textOutput(outputId = "card2IconLvlD2M"), align = "center"),
            f7Col(textOutput(outputId = "card3IconLvlD2M"), align = "center"),
            f7Col(textOutput(outputId = "card4IconLvlD2M"), align = "center")
          ),
          f7Row(
            f7Col(uiOutput(outputId = "card5IconD2M")),
            f7Col(uiOutput(outputId = "card6IconD2M")),
            f7Col(uiOutput(outputId = "card7IconD2M")),
            f7Col(uiOutput(outputId = "card8IconD2M"))
          ),
          f7Row(
            f7Col(textOutput(outputId = "card5IconLvlD2M"), align = "center"),
            f7Col(textOutput(outputId = "card6IconLvlD2M"), align = "center"),
            f7Col(textOutput(outputId = "card7IconLvlD2M"), align = "center"),
            f7Col(textOutput(outputId = "card8IconLvlD2M"), align = "center")
          )
        )
      ),
      f7Tab(
        tabName = "Cf.",
        icon = f7Icon("arrow_right_arrow_left_square_fill"),
        # chart_bar_alt_fill
        # arrow_right_arrow_left_square_fill
        f7Card(
          title = "Predicted Trophies",
          plotOutput(outputId = "barv5M", height = "300px")
        ),
        f7Card(
          title = "Interpretation",
          textOutput(outputId = "interpretNoteM")
        )
        
      ),
      f7Tab(
        tabName = "Route",
        icon = f7Icon("map_fill"),
        f7Card(
          title = "Optimized Upgrade Route for Deck 1",
          f7Button(inputId = "viewUpdateM", label = "View Route / Update Plot"),
          footer = f7Button(inputId = "resetM", label = "Reset")
        ),
        f7Card(
          rHandsontableOutput(outputId = "opRouteLv5M"),
          footer = textOutput(outputId = "noteRouteM")
        ),
        f7Card(
          textOutput(outputId = "plotTitleM"),
          plotlyOutput(outputId = "geomLineM")
        )
      ),
      f7Tab(
        tabName = "Rate",
        icon = f7Icon("chart_bar_square_fill"),
        f7Card(
          title = "Win Rate Investigation for Deck 1",
          f7Button(inputId = "viewWinRateM", label = "View Route / Update Plot")
        ),
        f7Card(
          plotlyOutput("winRatePlotM")
        )
      )
    )
  )
)



# calc?
serv_calc <- list()

values <- reactiveValues()
valuesM <- reactiveValues()

values$playerDeck <- tibble(
  playerCards = c("Baby Dragon", "Goblin Hut", "Graveyard", "Knight", "The Log", "Musketeer", "Poison", "Guards"),
  levels = c(11, 11, 10, 11, 11, 13, 11, 13)
)
valuesM$playerDeck <- tibble(
  playerCards = c("Baby Dragon", "Goblin Hut", "Graveyard", "Knight", "The Log", "Musketeer", "Poison", "Guards"),
  levels = c(11, 11, 10, 11, 11, 13, 11, 13)
)



# update deck button
serv_calc[[1]] <- function(input, sess){
  observeEvent(input$updateDeck, {
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
    
    values$playerDeck <- extractCard(input$playerTag)
  })
}
serv_calc[[5]] <- function(input, sess) {
  # update deck button
  observeEvent(input$updateDeckM, {
    # alert
    f7Notif(
      icon = f7Icon("hourglass_tophalf_fill"),
      title = "Notification",
      subtitle = "Please Wait",
      text = "It may take a few seconds",
      closeTimeout = 3500
    )
    
    valuesM$playerDeck <- extractCard(input$playerTagM)
  })
}



values$viewCounter <- 0
valuesM$viewCounter <- 0



serv_calc[[2]] <- function(input, sess) {
  # View Route Table Left
  observeEvent(input$viewUpdate, {
    values$viewCounter <- values$viewCounter + 1
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
      
      # process season stage (battletime)
      if (input$seasonStage == "Early Season") {
        battleTime <- 10
      }
      if (input$seasonStage == "Mid Season") {
        battleTime <- 20
      }
      if (input$seasonStage == "Late Season") {
        battleTime <- 30
      }
      
      newxOpBeforeLv5 <- mutate(newxOpBeforeLv5, battletime = battleTime)
      
      # process selected cards
      cardsInputOpLv5 <- opDeckBeforeLv5$card
      cardsInputOpLv5 <- str_replace_all(cardsInputOpLv5, " ", "_")
      cardsInputOpLv5 <- str_replace_all(cardsInputOpLv5, "-", "_")
      
      for (iv5 in 1:length(cardsInputOpLv5)) {
        for (jv5 in 1:length(names(newxOpBeforeLv5))) {
          if (cardsInputOpLv5[iv5] == names(newxOpBeforeLv5)[jv5]) {
            newxOpBeforeLv5[1,jv5] <- 1
          }
        }
      }
      
      # process selected levels
      lvlInputOpBeforeLv5 <- opDeckBeforeLv5$level
      
      #cardsInputOpCleanLv5 <- str_remove_all(cardsInputOpLv5, "\\.|-")
      cardsInputOpCleanLv5 <- paste(cardsInputOpLv5, "_Level", sep = "")
      
      for (kv5 in 1:length(cardsInputOpCleanLv5)) {
        for (lv5 in 1:length(names(newxOpBeforeLv5))) {
          if (cardsInputOpCleanLv5[kv5] == names(newxOpBeforeLv5)[lv5]) {
            newxOpBeforeLv5[1,lv5] <- lvlInputOpBeforeLv5[kv5]
          }
        }
      }
      
      # now use newx for prediction
      adjPred3 <- tibble(
        pred = predict.lm(mod3, newdata = newxOpBeforeLv5), # a prediction from newx
        predRounded = round(pred / 100) * 100
      ) %>%
        left_join(., residAdj, by = "predRounded") %>%
        mutate(
          adjPred05 = pred + resid05,
          adjPred25 = pred + resid25,
          adjPred50 = pred + resid50,
          adjPred75 = pred + resid75,
          adjPred95 = pred + resid95
        ) %>%
        select(8:12) %>%
        round()
      
      opDeckBeforeTrophiesLv5 <- adjPred3[[3]] # consider the 50th percentile prediction
      
      
      
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
        
        # process season stage (battletime)
        if (input$seasonStage == "Early Season") {
          battleTime <- 10
        }
        if (input$seasonStage == "Mid Season") {
          battleTime <- 20
        }
        if (input$seasonStage == "Late Season") {
          battleTime <- 30
        }
        
        newxOpAfterLv5 <- mutate(newxOpAfterLv5, battletime = battleTime)
        
        # process selected cards
        cardsInputOpLv5 <- opDeckAfterLv5$card
        cardsInputOpLv5 <- str_replace_all(cardsInputOpLv5, " ", "_")
        cardsInputOpLv5 <- str_replace_all(cardsInputOpLv5, "-", "_")
        
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
        adjPred4 <- tibble(
          pred = predict.lm(mod3, newdata = newxOpAfterLv5), # a prediction from newx
          predRounded = round(pred / 100) * 100
        ) %>%
          left_join(., residAdj, by = "predRounded") %>%
          mutate(
            adjPred05 = pred + resid05,
            adjPred25 = pred + resid25,
            adjPred50 = pred + resid50,
            adjPred75 = pred + resid75,
            adjPred95 = pred + resid95
          ) %>%
          select(8:12) %>%
          round()
        
        opDeckAfterTrophiesLv5 <- adjPred4[[3]] # consider the 50th percentile prediction
        
        
        
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
      select(Step, Card, `Upgrade To`, `Gold Required`, `Trophy\nGain`, `Trophy Gain\n/ 1000 Gold`) %>%
#      arrange(desc(`Trophy Gain\n/ 1000 Gold`)) %>% # some glitch
      mutate(
        Step = c(1:nrow(.))
      )
    
    values$hot <<- NULL
    values$hot <<- rhandsontable(opRouteDFLv5, manualRowMove = TRUE)
    
    if (values$viewCounter == 1) {
      values$orig <<- opRouteDFLv5
    }
    
    
  })
}

serv_calc[[6]] <- function(input, sess) {
  # View Route Table Left
  observeEvent(input$viewUpdateM, {
    valuesM$viewCounter <- valuesM$viewCounter + 1
    # alert
    f7Notif(
      icon = f7Icon("hourglass_tophalf_fill"),
      title = "Notification",
      subtitle = "Please Wait",
      text = "It may take a few seconds",
      closeTimeout = 3500
    )
    
    # the current most updated optimized deck (starting from no upgrades)
    opDeckLv5 <- tibble(
      card = c(input$card1Lv5M, input$card2Lv5M, input$card3Lv5M, input$card4Lv5M, input$card5Lv5M, input$card6Lv5M, input$card7Lv5M, input$card8Lv5M),
      level = c(input$card1LvlLv5M, input$card2LvlLv5M, input$card3LvlLv5M, input$card4LvlLv5M, input$card5LvlLv5M, input$card6LvlLv5M, input$card7LvlLv5M, input$card8LvlLv5M)
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
      
      # process season stage (battletime)
      if (input$seasonStageM == "Early Season") {
        battleTime <- 10
      }
      if (input$seasonStageM == "Mid Season") {
        battleTime <- 20
      }
      if (input$seasonStageM == "Late Season") {
        battleTime <- 30
      }
      
      newxOpBeforeLv5 <- mutate(newxOpBeforeLv5, battletime = battleTime)
      
      # process selected cards
      cardsInputOpLv5 <- opDeckBeforeLv5$card
      cardsInputOpLv5 <- str_replace_all(cardsInputOpLv5, " ", "_")
      cardsInputOpLv5 <- str_replace_all(cardsInputOpLv5, "-", "_")
      
      for (iv5 in 1:length(cardsInputOpLv5)) {
        for (jv5 in 1:length(names(newxOpBeforeLv5))) {
          if (cardsInputOpLv5[iv5] == names(newxOpBeforeLv5)[jv5]) {
            newxOpBeforeLv5[1,jv5] <- 1
          }
        }
      }
      
      # process selected levels
      lvlInputOpBeforeLv5 <- opDeckBeforeLv5$level
      
      #cardsInputOpCleanLv5 <- str_remove_all(cardsInputOpLv5, "\\.|-")
      cardsInputOpCleanLv5 <- paste(cardsInputOpLv5, "_Level", sep = "")
      
      for (kv5 in 1:length(cardsInputOpCleanLv5)) {
        for (lv5 in 1:length(names(newxOpBeforeLv5))) {
          if (cardsInputOpCleanLv5[kv5] == names(newxOpBeforeLv5)[lv5]) {
            newxOpBeforeLv5[1,lv5] <- lvlInputOpBeforeLv5[kv5]
          }
        }
      }
      
      # now use newx for prediction
      adjPred3 <- tibble(
        pred = predict.lm(mod3, newdata = newxOpBeforeLv5), # a prediction from newx
        predRounded = round(pred / 100) * 100
      ) %>%
        left_join(., residAdj, by = "predRounded") %>%
        mutate(
          adjPred05 = pred + resid05,
          adjPred25 = pred + resid25,
          adjPred50 = pred + resid50,
          adjPred75 = pred + resid75,
          adjPred95 = pred + resid95
        ) %>%
        select(8:12) %>%
        round()
      
      opDeckBeforeTrophiesLv5 <- adjPred3[[3]] # consider the 50th percentile prediction
      
      
      
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
        
        # process season stage (battletime)
        if (input$seasonStageM == "Early Season") {
          battleTime <- 10
        }
        if (input$seasonStageM == "Mid Season") {
          battleTime <- 20
        }
        if (input$seasonStageM == "Late Season") {
          battleTime <- 30
        }
        
        newxOpAfterLv5 <- mutate(newxOpAfterLv5, battletime = battleTime)
        
        # process selected cards
        cardsInputOpLv5 <- opDeckAfterLv5$card
        cardsInputOpLv5 <- str_replace_all(cardsInputOpLv5, " ", "_")
        cardsInputOpLv5 <- str_replace_all(cardsInputOpLv5, "-", "_")
        
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
        adjPred4 <- tibble(
          pred = predict.lm(mod3, newdata = newxOpAfterLv5), # a prediction from newx
          predRounded = round(pred / 100) * 100
        ) %>%
          left_join(., residAdj, by = "predRounded") %>%
          mutate(
            adjPred05 = pred + resid05,
            adjPred25 = pred + resid25,
            adjPred50 = pred + resid50,
            adjPred75 = pred + resid75,
            adjPred95 = pred + resid95
          ) %>%
          select(8:12) %>%
          round()
        
        opDeckAfterTrophiesLv5 <- adjPred4[[3]] # consider the 50th percentile prediction
        
        
        
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
      select(Step, Card, `Upgrade To`, `Gold Required`, `Trophy\nGain`, `Trophy Gain\n/ 1000 Gold`) %>%
#      arrange(desc(`Trophy Gain\n/ 1000 Gold`)) %>% # some glitch
      mutate(
        Step = c(1:nrow(.))
      )
    
    valuesM$hot <<- NULL
    valuesM$hot <<- rhandsontable(opRouteDFLv5, manualRowMove = TRUE)
    
    if (valuesM$viewCounter == 1) {
      valuesM$orig <<- opRouteDFLv5
    }
    
    
  })
}



serv_calc[[3]] <- function(input, sess) {
  # save route as sorted df
  observe({
    if(!is.null(input$opRouteLv5)) {
      values$DF <- hot_to_r(input$opRouteLv5)
      isolate(values$DF <-
                values$DF #%>%
                #arrange(desc(`Trophy Gain\n/ 1000 Gold`))
      )
    }
  })
}
serv_calc[[7]] <- function(input, sess) {
  # save route as sorted df
  observe({
    if(!is.null(input$opRouteLv5M)) {
      valuesM$DF <- hot_to_r(input$opRouteLv5M)
      isolate(valuesM$DF <-
                valuesM$DF #%>%
                #arrange(desc(`Trophy Gain\n/ 1000 Gold`))
      )
    }
  })
}




serv_calc[[4]] <- function(input, sess) {
  # reset button
  observeEvent(input$reset, {
    values$viewCounter <- 0
    #output$opRouteLv5 <- NULL
    #output$opRouteLv5 <- renderRHandsontable({
    values$hot <- NULL
    values$hot <- rhandsontable(values$orig, manualRowMove = F)
    #values$hot
    #})
  })
}
serv_calc[[8]] <- function(input, sess) {
  # reset button
  observeEvent(input$resetM, {
    valuesM$viewCounter <- 0
    #output$opRouteLv5 <- NULL
    #output$opRouteLv5 <- renderRHandsontable({
    valuesM$hot <- NULL
    valuesM$hot <- rhandsontable(valuesM$orig, manualRowMove = F)
    #values$hot
    #})
  })
}




serv_calc[[9]] <- function(input, sess) {
  # plot win rate - desktop: 9 - next index: 10
  observeEvent(input$viewWinRate, {
    # instead - use 0.25 and 0.95 percentile trophies
    trophy_player <- c(values$left05, values$left95)
    
    minTrophy = trophy_reset(trophy_player[1])
    maxTrophy = trophy_player[2] + 300
    
    
    
    cards2use <- tibble(
      card = c(input$card1Lv5, input$card2Lv5, input$card3Lv5, input$card4Lv5, input$card5Lv5, input$card6Lv5, input$card7Lv5, input$card8Lv5),
      level = c(input$card1LvlLv5, input$card2LvlLv5, input$card3LvlLv5, input$card4LvlLv5, input$card5LvlLv5, input$card6LvlLv5, input$card7LvlLv5, input$card8LvlLv5)
    )
    
    
    dat <- cardProfiles %>%
      filter(trophies < maxTrophy) %>%
      filter(trophies >= minTrophy) %>%
      semi_join(., cards2use, by = c("card", "level")) %>%
      mutate(win.rate = round(100* win/(win+lose), 1),
             cardlevel = paste(card, ' (',level,')', sep = '')
      ) %>%
      select(cardlevel, trophies, win.rate)
    
    values$res <- list(x = dat, trophies=trophy_player)
  })
}
serv_calc[[10]] <- function(input, sess) {
  # plot win rate - desktop: 9 - next index: 10
  observeEvent(input$viewWinRateM, {
    # instead - use 0.25 and 0.95 percentile trophies
    trophy_player <- c(valuesM$left05, valuesM$left95)
    
    minTrophy = trophy_reset(trophy_player[1])
    maxTrophy = trophy_player[2] + 300
    
    
    
    cards2use <- tibble(
      card = c(input$card1Lv5M, input$card2Lv5M, input$card3Lv5M, input$card4Lv5M, input$card5Lv5M, input$card6Lv5M, input$card7Lv5M, input$card8Lv5M),
      level = c(input$card1LvlLv5M, input$card2LvlLv5M, input$card3LvlLv5M, input$card4LvlLv5M, input$card5LvlLv5M, input$card6LvlLv5M, input$card7LvlLv5M, input$card8LvlLv5M)
    )
    
    
    dat <- cardProfiles %>%
      filter(trophies < maxTrophy) %>%
      filter(trophies >= minTrophy) %>%
      semi_join(., cards2use, by = c("card", "level")) %>%
      mutate(win.rate = round(100* win/(win+lose), 1),
             cardlevel = paste(card, ' (',level,')', sep = '')
      ) %>%
      select(cardlevel, trophies, win.rate)
    
    valuesM$res <- list(x = dat, trophies=trophy_player)
  })
}






# output
output <- list()



# reactive select boxes for left deck (can be pre-loaded from values$playerDeck <- playerTag)
output$card1LSelectorv5 <- function(input, sess) {
  renderUI({
    selectInput(inputId = "card1Lv5", label = "Card 1", choices = cards, selected = values$playerDeck[[1,1]])
  })
}

output$card1LvlLSelectorv5 <- function(input, sess) {
  renderUI({
    numericInput(inputId = "card1LvlLv5", label = "Level", min = 1, max = 13, value = values$playerDeck[[1,2]])
  })
}

output$card2LSelectorv5 <- function(input, sess) {
  renderUI({
    selectInput(inputId = "card2Lv5", label = "Card 2", choices = cards, selected = values$playerDeck[[2,1]])
  })
}

output$card2LvlLSelectorv5 <- function(input, sess) {
  renderUI({
    numericInput(inputId = "card2LvlLv5", label = "Level", min = 1, max = 13, value = values$playerDeck[[2,2]])
  })
}

output$card3LSelectorv5 <- function(input, sess) {
  renderUI({
    selectInput(inputId = "card3Lv5", label = "Card 3", choices = cards, selected = values$playerDeck[[3,1]])
  })
}



output$card3LvlLSelectorv5 <- function(input, sess) {
  renderUI({
    numericInput(inputId = "card3LvlLv5", label = "Level", min = 1, max = 13, value = values$playerDeck[[3,2]])
  })
}



output$card4LSelectorv5 <- function(input, sess) {
  renderUI({
    selectInput(inputId = "card4Lv5", label = "Card 4", choices = cards, selected = values$playerDeck[[4,1]])
  })
}



output$card4LvlLSelectorv5 <- function(input, sess) {
  renderUI({
    numericInput(inputId = "card4LvlLv5", label = "Level", min = 1, max = 13, value = values$playerDeck[[4,2]])
  })
}



output$card5LSelectorv5 <- function(input, sess) {
  renderUI({
    selectInput(inputId = "card5Lv5", label = "Card 5", choices = cards, selected = values$playerDeck[[5,1]])
  })
}


output$card5LvlLSelectorv5 <- function(input, sess) {
  renderUI({
    numericInput(inputId = "card5LvlLv5", label = "Level", min = 1, max = 13, value = values$playerDeck[[5,2]])
  })
}


output$card6LSelectorv5 <- function(input, sess) {
  renderUI({
    selectInput(inputId = "card6Lv5", label = "Card 6", choices = cards, selected = values$playerDeck[[6,1]])
  })
}


output$card6LvlLSelectorv5 <- function(input, sess) {
  renderUI({
    numericInput(inputId = "card6LvlLv5", label = "Level", min = 1, max = 13, value = values$playerDeck[[6,2]])
  })
}

output$card7LSelectorv5 <- function(input, sess) {
  renderUI({
    selectInput(inputId = "card7Lv5", label = "Card 7", choices = cards, selected = values$playerDeck[[7,1]])
  })
}

output$card7LvlLSelectorv5 <- function(input, sess) {
  renderUI({
    numericInput(inputId = "card7LvlLv5", label = "Level", min = 1, max = 13, value = values$playerDeck[[7,2]])
  })
}

output$card8LSelectorv5 <- function(input, sess) {
  renderUI({
    selectInput(inputId = "card8Lv5", label = "Card 8", choices = cards, selected = values$playerDeck[[8,1]])
  })
}

output$card8LvlLSelectorv5 <- function(input, sess) {
  renderUI({
    numericInput(inputId = "card8LvlLv5", label = "Level", min = 1, max = 13, value = values$playerDeck[[8,2]])
  })
}



# reactive select boxes for right deck
output$card1RSelectorv5 <- function(input, sess) {
  renderUI({
    values$playerDeck
    selectInput(inputId = "card1Rv5", label = "Card 1", choices = cards, selected = input$card1Lv5)
  })
}

output$card1LvlRSelectorv5 <- function(input, sess) {
  renderUI({
    values$playerDeck
    numericInput(inputId = "card1LvlRv5", label = "Level", min = 1, max = 13, value = input$card1LvlLv5)
  })
}

output$card2RSelectorv5 <- function(input, sess) {
  renderUI({
    values$playerDeck
    selectInput(inputId = "card2Rv5", label = "Card 2", choices = cards, selected = input$card2Lv5)
  })
}

output$card2LvlRSelectorv5 <- function(input, sess) {
  renderUI({
    values$playerDeck
    numericInput(inputId = "card2LvlRv5", label = "Level", min = 1, max = 13, value = input$card2LvlLv5)
  })
}

output$card3RSelectorv5 <- function(input, sess) {
  renderUI({
    values$playerDeck
    selectInput(inputId = "card3Rv5", label = "Card 3", choices = cards, selected = input$card3Lv5)
  })
}

output$card3LvlRSelectorv5 <- function(input, sess) {
  renderUI({
    values$playerDeck
    numericInput(inputId = "card3LvlRv5", label = "Level", min = 1, max = 13, value = input$card3LvlLv5)
  })
}

output$card4RSelectorv5 <- function(input, sess) {
  renderUI({
    values$playerDeck
    selectInput(inputId = "card4Rv5", label = "Card 4", choices = cards, selected = input$card4Lv5)
  })
}

output$card4LvlRSelectorv5 <- function(input, sess) {
  renderUI({
    values$playerDeck
    numericInput(inputId = "card4LvlRv5", label = "Level", min = 1, max = 13, value = input$card4LvlLv5)
  })
}

output$card5RSelectorv5 <- function(input, sess) {
  renderUI({
    values$playerDeck
    selectInput(inputId = "card5Rv5", label = "Card 5", choices = cards, selected = input$card5Lv5)
  })
}

output$card5LvlRSelectorv5 <- function(input, sess) {
  renderUI({
    values$playerDeck
    numericInput(inputId = "card5LvlRv5", label = "Level", min = 1, max = 13, value = input$card5LvlLv5)
  })
}

output$card6RSelectorv5 <- function(input, sess) {
  renderUI({
    values$playerDeck
    selectInput(inputId = "card6Rv5", label = "Card 6", choices = cards, selected = input$card6Lv5)
  })
}

output$card6LvlRSelectorv5 <- function(input, sess) {
  renderUI({
    values$playerDeck
    numericInput(inputId = "card6LvlRv5", label = "Level", min = 1, max = 13, value = input$card6LvlLv5)
  })
}

output$card7RSelectorv5 <- function(input, sess) {
  renderUI({
    values$playerDeck
    selectInput(inputId = "card7Rv5", label = "Card 7", choices = cards, selected = input$card7Lv5)
  })
}

output$card7LvlRSelectorv5 <- function(input, sess) {
  renderUI({
    values$playerDeck
    numericInput(inputId = "card7LvlRv5", label = "Level", min = 1, max = 13, value = input$card7LvlLv5)
  })
}

output$card8RSelectorv5 <- function(input, sess) {
  renderUI({
    values$playerDeck
    selectInput(inputId = "card8Rv5", label = "Card 8", choices = cards, selected = input$card8Lv5)
  })
}

output$card8LvlRSelectorv5 <- function(input, sess) {
  renderUI({
    values$playerDeck
    numericInput(inputId = "card8LvlRv5", label = "Level", min = 1, max = 13, value = input$card8LvlLv5)
  })
}



# for card icons of left deck
output$card1Icon <- function(input, sess) {
  renderUI({
    for (icon1 in 1:length(cards_s21)) {
      if (cards_s21[icon1] == input$card1Lv5) {
        icon1URL <-  cr_cards_s21$iconUrl[icon1]
      }
    }
    
    tags$img(src = icon1URL, width = "100%", height = "auto")
  })
}

output$card2Icon <- function(input, sess) {
  renderUI({
    for (icon2 in 1:length(cards_s21)) {
      if (cards_s21[icon2] == input$card2Lv5) {
        icon2URL <-  cr_cards_s21$iconUrl[icon2]
      }
    }
    
    tags$img(src = icon2URL, width = "100%", height = "auto")
  })
}

output$card3Icon <- function(input, sess) {
  renderUI({
    for (icon3 in 1:length(cards_s21)) {
      if (cards_s21[icon3] == input$card3Lv5) {
        icon3URL <-  cr_cards_s21$iconUrl[icon3]
      }
    }
    
    tags$img(src = icon3URL, width = "100%", height = "auto")
  })
}

output$card4Icon <- function(input, sess) {
  renderUI({
    for (icon4 in 1:length(cards_s21)) {
      if (cards_s21[icon4] == input$card4Lv5) {
        icon4URL <-  cr_cards_s21$iconUrl[icon4]
      }
    }
    
    tags$img(src = icon4URL, width = "100%", height = "auto")
  })
}

output$card5Icon <- function(input, sess) {
  renderUI({
    for (icon5 in 1:length(cards_s21)) {
      if (cards_s21[icon5] == input$card5Lv5) {
        icon5URL <-  cr_cards_s21$iconUrl[icon5]
      }
    }
    
    tags$img(src = icon5URL, width = "100%", height = "auto")
  })
}

output$card6Icon <- function(input, sess) {
  renderUI({
    for (icon6 in 1:length(cards_s21)) {
      if (cards_s21[icon6] == input$card6Lv5) {
        icon6URL <-  cr_cards_s21$iconUrl[icon6]
      }
    }
    
    tags$img(src = icon6URL, width = "100%", height = "auto")
  })
}

output$card7Icon <- function(input, sess) {
  renderUI({
    for (icon7 in 1:length(cards_s21)) {
      if (cards_s21[icon7] == input$card7Lv5) {
        icon7URL <-  cr_cards_s21$iconUrl[icon7]
      }
    }
    
    tags$img(src = icon7URL, width = "100%", height = "auto")
  })
}

output$card8Icon <- function(input, sess) {
  renderUI({
    for (icon8 in 1:length(cards_s21)) {
      if (cards_s21[icon8] == input$card8Lv5) {
        icon8URL <-  cr_cards_s21$iconUrl[icon8]
      }
    }
    
    tags$img(src = icon8URL, width = "100%", height = "auto")
  })
}



# for card icons of right deck
output$card1IconD2 <- function(input, sess) {
  renderUI({
    for (icon1D2 in 1:length(cards_s21)) {
      if (cards_s21[icon1D2] == input$card1Rv5) {
        icon1URLD2 <-  cr_cards_s21$iconUrl[icon1D2]
      }
    }
    
    tags$img(src = icon1URLD2, width = "100%", height = "auto")
  })
}

output$card2IconD2 <- function(input, sess) {
  renderUI({
    for (icon2D2 in 1:length(cards_s21)) {
      if (cards_s21[icon2D2] == input$card2Rv5) {
        icon2URLD2 <-  cr_cards_s21$iconUrl[icon2D2]
      }
    }
    
    tags$img(src = icon2URLD2, width = "100%", height = "auto")
  })
}

output$card3IconD2 <- function(input, sess) {
  renderUI({
    for (icon3D2 in 1:length(cards_s21)) {
      if (cards_s21[icon3D2] == input$card3Rv5) {
        icon3URLD2 <-  cr_cards_s21$iconUrl[icon3D2]
      }
    }
    
    tags$img(src = icon3URLD2, width = "100%", height = "auto")
  })
}

output$card4IconD2 <- function(input, sess) {
  renderUI({
    for (icon4D2 in 1:length(cards_s21)) {
      if (cards_s21[icon4D2] == input$card4Rv5) {
        icon4URLD2 <-  cr_cards_s21$iconUrl[icon4D2]
      }
    }
    
    tags$img(src = icon4URLD2, width = "100%", height = "auto")
  })
}

output$card5IconD2 <- function(input, sess) {
  renderUI({
    for (icon5D2 in 1:length(cards_s21)) {
      if (cards_s21[icon5D2] == input$card5Rv5) {
        icon5URLD2 <-  cr_cards_s21$iconUrl[icon5D2]
      }
    }
    
    tags$img(src = icon5URLD2, width = "100%", height = "auto")
  })
}

output$card6IconD2 <- function(input, sess) {
  renderUI({
    for (icon6D2 in 1:length(cards_s21)) {
      if (cards_s21[icon6D2] == input$card6Rv5) {
        icon6URLD2 <-  cr_cards_s21$iconUrl[icon6D2]
      }
    }
    
    tags$img(src = icon6URLD2, width = "100%", height = "auto")
  })
}

output$card7IconD2 <- function(input, sess) {
  renderUI({
    for (icon7D2 in 1:length(cards_s21)) {
      if (cards_s21[icon7D2] == input$card7Rv5) {
        icon7URLD2 <-  cr_cards_s21$iconUrl[icon7D2]
      }
    }
    
    tags$img(src = icon7URLD2, width = "100%", height = "auto")
  })
}

output$card8IconD2 <- function(input, sess) {
  renderUI({
    for (icon8D2 in 1:length(cards_s21)) {
      if (cards_s21[icon8D2] == input$card8Rv5) {
        icon8URLD2 <-  cr_cards_s21$iconUrl[icon8D2]
      }
    }
    
    tags$img(src = icon8URLD2, width = "100%", height = "auto")
  })
}



# for card icon levels of left deck
output$card1IconLvl <- function(input, sess) {
  renderText({
    paste("lvl", input$card1LvlLv5)
  })
}

output$card2IconLvl <- function(input, sess) {
  renderText({
    paste("lvl", input$card2LvlLv5)
  })
}

output$card3IconLvl <- function(input, sess) {
  renderText({
    paste("lvl", input$card3LvlLv5)
  })
}

output$card4IconLvl <- function(input, sess) {
  renderText({
    paste("lvl", input$card4LvlLv5)
  })
}

output$card5IconLvl <- function(input, sess) {
  renderText({
    paste("lvl", input$card5LvlLv5)
  })
}

output$card6IconLvl <- function(input, sess) {
  renderText({
    paste("lvl", input$card6LvlLv5)
  })
}

output$card7IconLvl <- function(input, sess) {
  renderText({
    paste("lvl", input$card7LvlLv5)
  })
}

output$card8IconLvl <- function(input, sess) {
  renderText({
    paste("lvl", input$card8LvlLv5)
  })
}



# for card icon levels of right deck
output$card1IconLvlD2 <- function(input, sess) {
  renderText({
    paste("lvl", input$card1LvlRv5)
  })
}

output$card2IconLvlD2 <- function(input, sess) {
  renderText({
    paste("lvl", input$card2LvlRv5)
  })
}

output$card3IconLvlD2 <- function(input, sess) {
  renderText({
    paste("lvl", input$card3LvlRv5)
  })
}

output$card4IconLvlD2 <- function(input, sess) {
  renderText({
    paste("lvl", input$card4LvlRv5)
  })
}

output$card5IconLvlD2 <- function(input, sess) {
  renderText({
    paste("lvl", input$card5LvlRv5)
  })
}

output$card6IconLvlD2 <- function(input, sess) {
  renderText({
    paste("lvl", input$card6LvlRv5)
  })
}

output$card7IconLvlD2 <- function(input, sess) {
  renderText({
    paste("lvl", input$card7LvlRv5)
  })
}

output$card8IconLvlD2 <- function(input, sess) {
  renderText({
    paste("lvl", input$card8LvlRv5)
  })
}



# for bar plot
output$barv5 <- function(input, sess) {
  renderPlot({
    values$viewCounter <- 0
    # get predictions
    
    # for left deck
    
    # fill out newx (prediction data frame) based on input
    newxLv5 <- newxEmpty
    
    # process season stage (battletime)
    if (input$seasonStage == "Early Season") {
      battleTime <- 10
    }
    if (input$seasonStage == "Mid Season") {
      battleTime <- 20
    }
    if (input$seasonStage == "Late Season") {
      battleTime <- 30
    }
    
    newxLv5 <- mutate(newxLv5, battletime = battleTime)
    
    # process selected cards
    cardsInputLv5 <- c(input$card1Lv5, input$card2Lv5, input$card3Lv5, input$card4Lv5, input$card5Lv5, input$card6Lv5, input$card7Lv5, input$card8Lv5)
    cardsInputLv5 <- str_replace_all(cardsInputLv5, " ", "_")
    cardsInputLv5 <- str_replace_all(cardsInputLv5, "-", "_")
    
    for (av5 in 1:length(cardsInputLv5)) {
      for (bv5 in 1:length(names(newxLv5))) {
        if (cardsInputLv5[av5] == names(newxLv5)[bv5]) {
          newxLv5[1,bv5] <- 1
        }
      }
    }
    
    # process selected levels
    lvlInputLv5 <- c(input$card1LvlLv5, input$card2LvlLv5, input$card3LvlLv5, input$card4LvlLv5, input$card5LvlLv5, input$card6LvlLv5, input$card7LvlLv5, input$card8LvlLv5)
    
    #cardsInputCleanLv5 <- str_remove_all(cardsInputLv5, "\\.|-")
    cardsInputCleanLv5 <- paste(cardsInputLv5, "_Level", sep = "")
    
    for (cv5 in 1:length(cardsInputCleanLv5)) {
      for (dv5 in 1:length(names(newxLv5))) {
        if (cardsInputCleanLv5[cv5] == names(newxLv5)[dv5]) {
          newxLv5[1,dv5] <- lvlInputLv5[cv5]
        }
      }
    }
    
    # now use newx for prediction
    adjPred1 <- tibble(
      pred = predict.lm(mod3, newdata = newxLv5), # a prediction from newx
      predRounded = round(pred / 100) * 100
    ) %>%
      left_join(., residAdj, by = "predRounded") %>%
      mutate(
        adjPred05 = pred + resid05,
        adjPred25 = pred + resid25,
        adjPred50 = pred + resid50,
        adjPred75 = pred + resid75,
        adjPred95 = pred + resid95
      ) %>%
      select(8:12) %>%
      round()
    
    predictedTrophiesLv5 <- adjPred1
    values$leftTrophies <- predictedTrophiesLv5[[3]] # the 50th percentile prediction
    values$left95 <- predictedTrophiesLv5[[5]] # 95th percentile, for interpretation note
    values$left05 <- predictedTrophiesLv5[[1]] # 05th percentile, for win rate plot
    
    
    
    # for right deck
    
    # fill out newx (prediction data frame) based on input
    newxRv5 <- newxEmpty
    
    # process season stage (battletime)
    if (input$seasonStage == "Early Season") {
      battleTime <- 10
    }
    if (input$seasonStage == "Mid Season") {
      battleTime <- 20
    }
    if (input$seasonStage == "Late Season") {
      battleTime <- 30
    }
    
    newxRv5 <- mutate(newxRv5, battletime = battleTime)
    
    # process selected cards
    cardsInputRv5 <- c(input$card1Rv5, input$card2Rv5, input$card3Rv5, input$card4Rv5, input$card5Rv5, input$card6Rv5, input$card7Rv5, input$card8Rv5)
    cardsInputRv5 <- str_replace_all(cardsInputRv5, " ", "_")
    cardsInputRv5 <- str_replace_all(cardsInputRv5, "-", "_")
    
    for (ev5 in 1:length(cardsInputRv5)) {
      for (fv5 in 1:length(names(newxRv5))) {
        if (cardsInputRv5[ev5] == names(newxRv5)[fv5]) {
          newxRv5[1,fv5] <- 1
        }
      }
    }
    
    # process selected levels
    lvlInputRv5 <- c(input$card1LvlRv5, input$card2LvlRv5, input$card3LvlRv5, input$card4LvlRv5, input$card5LvlRv5, input$card6LvlRv5, input$card7LvlRv5, input$card8LvlRv5)
    
    #cardsInputCleanRv5 <- str_remove_all(cardsInputRv5, "\\.|-")
    cardsInputCleanRv5 <- paste(cardsInputRv5, "_Level", sep = "")
    
    for (gv5 in 1:length(cardsInputCleanRv5)) {
      for (hv5 in 1:length(names(newxRv5))) {
        if (cardsInputCleanRv5[gv5] == names(newxRv5)[hv5]) {
          newxRv5[1,hv5] <- lvlInputRv5[gv5]
        }
      }
    }
    
    # now use newx for prediction
    adjPred2 <- tibble(
      pred = predict.lm(mod3, newdata = newxRv5), # a prediction from newx
      predRounded = round(pred / 100) * 100
    ) %>%
      left_join(., residAdj, by = "predRounded") %>%
      mutate(
        adjPred05 = pred + resid05,
        adjPred25 = pred + resid25,
        adjPred50 = pred + resid50,
        adjPred75 = pred + resid75,
        adjPred95 = pred + resid95
      ) %>%
      select(8:12) %>%
      round()
    
    predictedTrophiesRv5 <- adjPred2
    
    
    
    # the plot
    df_Barplotv5 <- tibble(
      prediction = c(as_vector(predictedTrophiesLv5), as_vector(predictedTrophiesRv5)),
      display = c(
        predictedTrophiesLv5[[1]] %/% 10,
        predictedTrophiesLv5[[2]] - predictedTrophiesLv5[[1]],
        predictedTrophiesLv5[[3]] - predictedTrophiesLv5[[2]],
        predictedTrophiesLv5[[4]] - predictedTrophiesLv5[[3]],
        predictedTrophiesLv5[[5]] - predictedTrophiesLv5[[4]],
        predictedTrophiesRv5[[1]] %/% 10,
        predictedTrophiesRv5[[2]] - predictedTrophiesRv5[[1]],
        predictedTrophiesRv5[[3]] - predictedTrophiesRv5[[2]],
        predictedTrophiesRv5[[4]] - predictedTrophiesRv5[[3]],
        predictedTrophiesRv5[[5]] - predictedTrophiesRv5[[4]]
      ),
      cumDisplay = c(
        cumsum(display[1:5]),
        cumsum(display[6:10])
      ),
      Percentile = paste(str_remove_all(c(names(predictedTrophiesLv5), names(predictedTrophiesRv5)), "[:alpha:]"), "th"),
      deck = c("Deck 1", "Deck 1", "Deck 1", "Deck 1", "Deck 1", "Deck 2", "Deck 2", "Deck 2", "Deck 2", "Deck 2")
    )
    
    df_Barplotv5 %>%
      ggplot(., aes(x = deck)) +
      geom_col(aes(y = display, fill = Percentile), width = 0.1, position = position_stack(reverse = T)) +
      geom_label(data = df_Barplotv5[df_Barplotv5$deck == "Deck 1", ], aes(y = cumDisplay, label = prediction, hjust = 1.5), size = 5) +
      geom_label(data = df_Barplotv5[df_Barplotv5$deck == "Deck 2", ], aes(y = cumDisplay, label = prediction, hjust = -0.5), size = 5) +
      theme_minimal() +
      scale_fill_viridis_d(option = "viridis") +
      guides(fill = guide_legend(reverse=TRUE)) +
      theme(
        legend.position = c(0.5, 0.25),
        legend.text = element_text(size = rel(1.2)),
        legend.title = element_text(size = rel(1.2)),
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



# interpretation note
output$interpretNote <- function(input, sess) {
  renderText({
    paste("Interpretation: For instance,", values$left95, "is the estimated 95th percentile trophy rating for all players using Deck 1. If you are above that rating, then you are in the top 5% of players with a similar deck.")
  })
}



# route note
output$noteRoute <- function(input, sess) {
  renderText({
    if (is.null(input$opRouteLv5)) {
      return (NULL)
    } else {
      'Note: The rows in the table are adjustable; simply drag them around to change the order. Then, click on "Update Plot" button to see how the path changes. Also, all trophy predictions here are in the 50th percentile.'
    }
  })
}



# plot title
output$plotTitle <- function(input, sess) {
  renderText({
    if (is.null(input$opRouteLv5)) {
      return (NULL)
    } else {
      "Line Plot of Trophies vs Cumulative Gold Spent"
    }
  })
}



# for line plot
output$geomLine <- function(input, sess) {
  renderPlotly({
    if (is.null(input$opRouteLv5)) {
      return(NULL)
    } else {# "2" temporary bug fix
      if (values$viewCounter %in% c(0, 1)) {
        data <- values$orig
        data[, "Cumulative_Gold"] <- cumsum(data$`Gold Required`)
        data[, "Trophies"] <- cumsum(data$`Trophy\nGain`) + values$leftTrophies
        options(scipen=10000)
        
        g <-
          data %>%
          ggplot(.,aes(x = Cumulative_Gold, y = Trophies)
          ) +
          geom_point(
            aes(
              text = paste(
                "Step", Step,
                "\nUpgrade", Card, "to Level", `Upgrade To`,
                "\nGold Required:", `Gold Required`,
                "\nTrophy Gain:", `Trophy\nGain`,
                "\nResulted Trophies:", Trophies,
                "\nCumulative Gold Spent:", Cumulative_Gold
              )
            )
          ) +
          geom_line(size = rel(1.15)) +
          theme_bw() +
          labs(
            x = "Cumulative Gold Spent",
            y = "Trophies"
          )
        
        p <- ggplotly(g, tooltip = "text")
        p
      } else {
        if (!is.null(values$DF)) {
          data1 <- values$orig
          data1[, "Cumulative_Gold"] <- cumsum(data1$`Gold Required`)
          data1[, "Trophies"] <- cumsum(data1$`Trophy\nGain`) + values$leftTrophies
          data1[, "Route"] <- "Optimized Route"
          
          data2 <- hot_to_r(input$opRouteLv5)
          data2[, "Cumulative_Gold"] <- cumsum(data2$`Gold Required`)
          data2[, "Trophies"] <- cumsum(data2$`Trophy\nGain`) + values$leftTrophies
          data2[, "Route"] <- "Adjusted Route"
          data2[, "Step"] <- data1[, "Step"] # display the updated step on the plot
          
          newData <- bind_rows(data1, data2)
          newData$Route <- fct_inorder(newData$Route)
          
          options(scipen=10000)
          
          g <-
            ggplot(newData, aes(x = Cumulative_Gold, y = Trophies, color = Route)) +
            geom_point(
              aes(
                text = paste(
                  "Step", Step,
                  "\nUpgrade", Card, "to Level", `Upgrade To`,
                  "\nGold Required:", `Gold Required`,
                  "\nTrophy Gain:", `Trophy\nGain`,
                  "\nResulted Trophies:", Trophies,
                  "\nCumulative Gold Spent:", Cumulative_Gold
                )
              )
            ) +
            geom_line(size = 1.15) +
            theme_bw() +
            labs(
              x = "Cumulative Gold Spent",
              y = "Trophies"
            ) +
            scale_color_manual(values = c("black", "red")) +
            theme(
              legend.position = "bottom",
              legend.title = element_blank()
            )
          p <-
            ggplotly(g, tooltip = "text") %>%
            layout(legend = list(orientation = "h", x = 0, y = -0.2))
          p
        } else {
          data <- hot_to_r(input$opRouteLv5)
          data[, "Cumulative_Gold"] <- cumsum(data$`Gold Required`)
          data[, "Trophies"] <- cumsum(data$`Trophy\nGain`) + values$leftTrophies
          options(scipen=10000)
          
          g <-
            data %>%
            ggplot(., aes(x = Cumulative_Gold, y = Trophies)) +
            geom_point(
              aes(
                text = paste(
                  "Step", Step,
                  "\nUpgrade", Card, "to Level", `Upgrade To`,
                  "\nGold Required:", `Gold Required`,
                  "\nTrophy Gain:", `Trophy\nGain`,
                  "\nResulted Trophies:", Trophies,
                  "\nCumulative Gold Spent:", Cumulative_Gold
                )
              )
            ) +
            geom_line(size = 1.15) +
            theme_bw() +
            labs(
              x = "Cumulative Gold Spent",
              y = "Trophies"
            )
          
          p <- ggplotly(g, tooltip = "text")
          p
        }
      }
    }
  })
}



# display hot
output$opRouteLv5 <- function(input, sess) {
  renderRHandsontable({
    values$hot
  })
}



# display win rate plot
output$winRatePlot <- function(input, sess) {
  renderPlotly({
    x <- (values$res)$x
    
    g <- ggplot(data = x, aes(x = trophies, y = win.rate, color = cardlevel)) +
      geom_hline(yintercept = 50, linetype = 2) +
      geom_vline(xintercept = values$res$trophies[1], color = "grey") +
      annotate("text", x=values$res$trophies[1], y=0, label= "5th Percentile Trophy")
    
    if(values$res$trophies[2] > values$res$trophies[1]){
      g <- g +   
        geom_vline(xintercept = values$res$trophies[2], color = "grey") +
        annotate("text", x=values$res$trophies[2], y=10, label= "95th Percentile Trophy")            
    }
    
    g <- g + geom_smooth(se = F, size = 2)  +
      scale_color_brewer(palette = "Dark2", name = "Card (Level)") +
      theme_bw() +
      xlab("Trophies") + ylab("Win Rate")
    ggplotly(g)
  })
}










# mobile server

# reactive select boxes for left deck (can be pre-loaded from values$playerDeck <- playerTag)
output$card1LSelectorv5M <- function(input, sess) {
  renderUI({
    f7SmartSelect(inputId = "card1Lv5M", label = "Card 1", choices = cards, selected = valuesM$playerDeck[[1,1]], openIn = "popup")
  })
}

output$card1LvlLSelectorv5M <- function(input, sess) {
  renderUI({
    f7Stepper(inputId = "card1LvlLv5M", label = NULL, min = 1, max = 13, value = valuesM$playerDeck[[1,2]], fill = TRUE)
  })
}

output$card2LSelectorv5M <- function(input, sess) {
  renderUI({
    f7SmartSelect(inputId = "card2Lv5M", label = "Card 2", choices = cards, selected = valuesM$playerDeck[[2,1]], openIn = "popup")
  })
}

output$card2LvlLSelectorv5M <- function(input, sess) {
  renderUI({
    f7Stepper(inputId = "card2LvlLv5M", label = NULL, min = 1, max = 13, value = valuesM$playerDeck[[2,2]], fill = TRUE)
  })
}

output$card3LSelectorv5M <- function(input, sess) {
  renderUI({
    f7SmartSelect(inputId = "card3Lv5M", label = "Card 3", choices = cards, selected = valuesM$playerDeck[[3,1]], openIn = "popup")
  })
}

output$card3LvlLSelectorv5M <- function(input, sess) {
  renderUI({
    f7Stepper(inputId = "card3LvlLv5M", label = NULL, min = 1, max = 13, value = valuesM$playerDeck[[3,2]], fill = TRUE)
  })
}

output$card4LSelectorv5M <- function(input, sess) {
  renderUI({
    f7SmartSelect(inputId = "card4Lv5M", label = "Card 4", choices = cards, selected = valuesM$playerDeck[[4,1]], openIn = "popup")
  })
}

output$card4LvlLSelectorv5M <- function(input, sess) {
  renderUI({
    f7Stepper(inputId = "card4LvlLv5M", label = NULL, min = 1, max = 13, value = valuesM$playerDeck[[4,2]], fill = TRUE)
  })
}

output$card5LSelectorv5M <- function(input, sess) {
  renderUI({
    f7SmartSelect(inputId = "card5Lv5M", label = "Card 5", choices = cards, selected = valuesM$playerDeck[[5,1]], openIn = "popup")
  })
}

output$card5LvlLSelectorv5M <- function(input, sess) {
  renderUI({
    f7Stepper(inputId = "card5LvlLv5M", label = NULL, min = 1, max = 13, value = valuesM$playerDeck[[5,2]], fill = TRUE)
  })
}

output$card6LSelectorv5M <- function(input, sess) {
  renderUI({
    f7SmartSelect(inputId = "card6Lv5M", label = "Card 6", choices = cards, selected = valuesM$playerDeck[[6,1]], openIn = "popup")
  })
}

output$card6LvlLSelectorv5M <- function(input, sess) {
  renderUI({
    f7Stepper(inputId = "card6LvlLv5M", label = NULL, min = 1, max = 13, value = valuesM$playerDeck[[6,2]], fill = TRUE)
  })
}

output$card7LSelectorv5M <- function(input, sess) {
  renderUI({
    f7SmartSelect(inputId = "card7Lv5M", label = "Card 7", choices = cards, selected = valuesM$playerDeck[[7,1]], openIn = "popup")
  })
}

output$card7LvlLSelectorv5M <- function(input, sess) {
  renderUI({
    f7Stepper(inputId = "card7LvlLv5M", label = NULL, min = 1, max = 13, value = valuesM$playerDeck[[7,2]], fill = TRUE)
  })
}

output$card8LSelectorv5M <- function(input, sess) {
  renderUI({
    f7SmartSelect(inputId = "card8Lv5M", label = "Card 8", choices = cards, selected = valuesM$playerDeck[[8,1]], openIn = "popup")
  })
}

output$card8LvlLSelectorv5M <- function(input, sess) {
  renderUI({
    f7Stepper(inputId = "card8LvlLv5M", label = NULL, min = 1, max = 13, value = valuesM$playerDeck[[8,2]], fill = TRUE)
  })
}



# reactive select boxes for right deck
output$card1RSelectorv5M <- function(input, sess) {
  renderUI({
    valuesM$playerDeck
    f7SmartSelect(inputId = "card1Rv5M", label = "Card 1", choices = cards, selected = input$card1Lv5M, openIn = "popup")
  })
}

output$card1LvlRSelectorv5M <- function(input, sess) {
  renderUI({
    valuesM$playerDeck
    f7Stepper(inputId = "card1LvlRv5M", label = NULL, min = 1, max = 13, value = input$card1LvlLv5M, fill = TRUE)
  })
}

output$card2RSelectorv5M <- function(input, sess) {
  renderUI({
    valuesM$playerDeck
    f7SmartSelect(inputId = "card2Rv5M", label = "Card 2", choices = cards, selected = input$card2Lv5M, openIn = "popup")
  })
}

output$card2LvlRSelectorv5M <- function(input, sess) {
  renderUI({
    valuesM$playerDeck
    f7Stepper(inputId = "card2LvlRv5M", label = NULL, min = 1, max = 13, value = input$card2LvlLv5M, fill = TRUE)
  })
}

output$card3RSelectorv5M <- function(input, sess) {
  renderUI({
    valuesM$playerDeck
    f7SmartSelect(inputId = "card3Rv5M", label = "Card 3", choices = cards, selected = input$card3Lv5M, openIn = "popup")
  })
}

output$card3LvlRSelectorv5M <- function(input, sess) {
  renderUI({
    valuesM$playerDeck
    f7Stepper(inputId = "card3LvlRv5M", label = NULL, min = 1, max = 13, value = input$card3LvlLv5M, fill = TRUE)
  })
}

output$card4RSelectorv5M <- function(input, sess) {
  renderUI({
    valuesM$playerDeck
    f7SmartSelect(inputId = "card4Rv5M", label = "Card 4", choices = cards, selected = input$card4Lv5M, openIn = "popup")
  })
}

output$card4LvlRSelectorv5M <- function(input, sess) {
  renderUI({
    valuesM$playerDeck
    f7Stepper(inputId = "card4LvlRv5M", label = NULL, min = 1, max = 13, value = input$card4LvlLv5M, fill = TRUE)
  })
}

output$card5RSelectorv5M <- function(input, sess) {
  renderUI({
    valuesM$playerDeck
    f7SmartSelect(inputId = "card5Rv5M", label = "Card 5", choices = cards, selected = input$card5Lv5M, openIn = "popup")
  })
}

output$card5LvlRSelectorv5M <- function(input, sess) {
  renderUI({
    valuesM$playerDeck
    f7Stepper(inputId = "card5LvlRv5M", label = NULL, min = 1, max = 13, value = input$card5LvlLv5M, fill = TRUE)
  })
}

output$card6RSelectorv5M <- function(input, sess) {
  renderUI({
    valuesM$playerDeck
    f7SmartSelect(inputId = "card6Rv5M", label = "Card 6", choices = cards, selected = input$card6Lv5M, openIn = "popup")
  })
}

output$card6LvlRSelectorv5M <- function(input, sess) {
  renderUI({
    valuesM$playerDeck
    f7Stepper(inputId = "card6LvlRv5M", label = NULL, min = 1, max = 13, value = input$card6LvlLv5M, fill = TRUE)
  })
}

output$card7RSelectorv5M <- function(input, sess) {
  renderUI({
    valuesM$playerDeck
    f7SmartSelect(inputId = "card7Rv5M", label = "Card 7", choices = cards, selected = input$card7Lv5M, openIn = "popup")
  })
}

output$card7LvlRSelectorv5M <- function(input, sess) {
  renderUI({
    valuesM$playerDeck
    f7Stepper(inputId = "card7LvlRv5M", label = NULL, min = 1, max = 13, value = input$card7LvlLv5M, fill = TRUE)
  })
}

output$card8RSelectorv5M <- function(input, sess) {
  renderUI({
    valuesM$playerDeck
    f7SmartSelect(inputId = "card8Rv5M", label = "Card 8", choices = cards, selected = input$card8Lv5M, openIn = "popup")
  })
}

output$card8LvlRSelectorv5M <- function(input, sess) {
  renderUI({
    valuesM$playerDeck
    f7Stepper(inputId = "card8LvlRv5M", label = NULL, min = 1, max = 13, value = input$card8LvlLv5M, fill = TRUE)
  })
}



# for card icons of left deck
output$card1IconM <- function(input, sess) {
  renderUI({
    for (icon1 in 1:length(cards_s21)) {
      if (cards_s21[icon1] == input$card1Lv5M) {
        icon1URL <-  cr_cards_s21$iconUrl[icon1]
      }
    }
    
    tags$img(src = icon1URL, width = "100%", height = "auto")
  })
}

output$card2IconM <- function(input, sess) {
  renderUI({
    for (icon2 in 1:length(cards_s21)) {
      if (cards_s21[icon2] == input$card2Lv5M) {
        icon2URL <-  cr_cards_s21$iconUrl[icon2]
      }
    }
    
    tags$img(src = icon2URL, width = "100%", height = "auto")
  })
}

output$card3IconM <- function(input, sess) {
  renderUI({
    for (icon3 in 1:length(cards_s21)) {
      if (cards_s21[icon3] == input$card3Lv5M) {
        icon3URL <-  cr_cards_s21$iconUrl[icon3]
      }
    }
    
    tags$img(src = icon3URL, width = "100%", height = "auto")
  })
}

output$card4IconM <- function(input, sess) {
  renderUI({
    for (icon4 in 1:length(cards_s21)) {
      if (cards_s21[icon4] == input$card4Lv5M) {
        icon4URL <-  cr_cards_s21$iconUrl[icon4]
      }
    }
    
    tags$img(src = icon4URL, width = "100%", height = "auto")
  })
}

output$card5IconM <- function(input, sess) {
  renderUI({
    for (icon5 in 1:length(cards_s21)) {
      if (cards_s21[icon5] == input$card5Lv5M) {
        icon5URL <-  cr_cards_s21$iconUrl[icon5]
      }
    }
    
    tags$img(src = icon5URL, width = "100%", height = "auto")
  })
}

output$card6IconM <- function(input, sess) {
  renderUI({
    for (icon6 in 1:length(cards_s21)) {
      if (cards_s21[icon6] == input$card6Lv5M) {
        icon6URL <-  cr_cards_s21$iconUrl[icon6]
      }
    }
    
    tags$img(src = icon6URL, width = "100%", height = "auto")
  })
}

output$card7IconM <- function(input, sess) {
  renderUI({
    for (icon7 in 1:length(cards_s21)) {
      if (cards_s21[icon7] == input$card7Lv5M) {
        icon7URL <-  cr_cards_s21$iconUrl[icon7]
      }
    }
    
    tags$img(src = icon7URL, width = "100%", height = "auto")
  })
}

output$card8IconM <- function(input, sess) {
  renderUI({
    for (icon8 in 1:length(cards_s21)) {
      if (cards_s21[icon8] == input$card8Lv5M) {
        icon8URL <-  cr_cards_s21$iconUrl[icon8]
      }
    }
    
    tags$img(src = icon8URL, width = "100%", height = "auto")
  })
}



# for card icons of right deck
output$card1IconD2M <- function(input, sess) {
  renderUI({
    for (icon1D2 in 1:length(cards_s21)) {
      if (cards_s21[icon1D2] == input$card1Rv5M) {
        icon1URLD2 <-  cr_cards_s21$iconUrl[icon1D2]
      }
    }
    
    tags$img(src = icon1URLD2, width = "100%", height = "auto")
  })
}

output$card2IconD2M <- function(input, sess) {
  renderUI({
    for (icon2D2 in 1:length(cards_s21)) {
      if (cards_s21[icon2D2] == input$card2Rv5M) {
        icon2URLD2 <-  cr_cards_s21$iconUrl[icon2D2]
      }
    }
    
    tags$img(src = icon2URLD2, width = "100%", height = "auto")
  })
}

output$card3IconD2M <- function(input, sess) {
  renderUI({
    for (icon3D2 in 1:length(cards_s21)) {
      if (cards_s21[icon3D2] == input$card3Rv5M) {
        icon3URLD2 <-  cr_cards_s21$iconUrl[icon3D2]
      }
    }
    
    tags$img(src = icon3URLD2, width = "100%", height = "auto")
  })
}

output$card4IconD2M <- function(input, sess) {
  renderUI({
    for (icon4D2 in 1:length(cards_s21)) {
      if (cards_s21[icon4D2] == input$card4Rv5M) {
        icon4URLD2 <-  cr_cards_s21$iconUrl[icon4D2]
      }
    }
    
    tags$img(src = icon4URLD2, width = "100%", height = "auto")
  })
}

output$card5IconD2M <- function(input, sess) {
  renderUI({
    for (icon5D2 in 1:length(cards_s21)) {
      if (cards_s21[icon5D2] == input$card5Rv5M) {
        icon5URLD2 <-  cr_cards_s21$iconUrl[icon5D2]
      }
    }
    
    tags$img(src = icon5URLD2, width = "100%", height = "auto")
  })
}

output$card6IconD2M <- function(input, sess) {
  renderUI({
    for (icon6D2 in 1:length(cards_s21)) {
      if (cards_s21[icon6D2] == input$card6Rv5M) {
        icon6URLD2 <-  cr_cards_s21$iconUrl[icon6D2]
      }
    }
    
    tags$img(src = icon6URLD2, width = "100%", height = "auto")
  })
}

output$card7IconD2M <- function(input, sess) {
  renderUI({
    for (icon7D2 in 1:length(cards_s21)) {
      if (cards_s21[icon7D2] == input$card7Rv5M) {
        icon7URLD2 <-  cr_cards_s21$iconUrl[icon7D2]
      }
    }
    
    tags$img(src = icon7URLD2, width = "100%", height = "auto")
  })
}

output$card8IconD2M <- function(input, sess) {
  renderUI({
    for (icon8D2 in 1:length(cards_s21)) {
      if (cards_s21[icon8D2] == input$card8Rv5M) {
        icon8URLD2 <-  cr_cards_s21$iconUrl[icon8D2]
      }
    }
    
    tags$img(src = icon8URLD2, width = "100%", height = "auto")
  })
}



# for card icon levels of left deck
output$card1IconLvlM <- function(input, sess) {
  renderText({
    paste("lvl", input$card1LvlLv5M)
  })
}

output$card2IconLvlM <- function(input, sess) {
  renderText({
    paste("lvl", input$card2LvlLv5M)
  })
}

output$card3IconLvlM <- function(input, sess) {
  renderText({
    paste("lvl", input$card3LvlLv5M)
  })
}

output$card4IconLvlM <- function(input, sess) {
  renderText({
    paste("lvl", input$card4LvlLv5M)
  })
}

output$card5IconLvlM <- function(input, sess) {
  renderText({
    paste("lvl", input$card5LvlLv5M)
  })
}

output$card6IconLvlM <- function(input, sess) {
  renderText({
    paste("lvl", input$card6LvlLv5M)
  })
}

output$card7IconLvlM <- function(input, sess) {
  renderText({
    paste("lvl", input$card7LvlLv5M)
  })
}

output$card8IconLvlM <- function(input, sess) {
  renderText({
    paste("lvl", input$card8LvlLv5M)
  })
}



# for card icon levels of right deck
output$card1IconLvlD2M <- function(input, sess) {
  renderText({
    paste("lvl", input$card1LvlRv5M)
  })
}

output$card2IconLvlD2M <- function(input, sess) {
  renderText({
    paste("lvl", input$card2LvlRv5M)
  })
}

output$card3IconLvlD2M <- function(input, sess) {
  renderText({
    paste("lvl", input$card3LvlRv5M)
  })
}

output$card4IconLvlD2M <- function(input, sess) {
  renderText({
    paste("lvl", input$card4LvlRv5M)
  })
}

output$card5IconLvlD2M <- function(input, sess) {
  renderText({
    paste("lvl", input$card5LvlRv5M)
  })
}

output$card6IconLvlD2M <- function(input, sess) {
  renderText({
    paste("lvl", input$card6LvlRv5M)
  })
}

output$card7IconLvlD2M <- function(input, sess) {
  renderText({
    paste("lvl", input$card7LvlRv5M)
  })
}

output$card8IconLvlD2M <- function(input, sess) {
  renderText({
    paste("lvl", input$card8LvlRv5M)
  })
}



# for bar plot
output$barv5M <- function(input, sess) {
  renderPlot({
    valuesM$viewCounter <- 0
    # get predictions
    
    # for left deck
    
    # fill out newx (prediction data frame) based on input
    newxLv5 <- newxEmpty
    
    # process season stage (battletime)
    if (input$seasonStageM == "Early Season") {
      battleTime <- 10
    }
    if (input$seasonStageM == "Mid Season") {
      battleTime <- 20
    }
    if (input$seasonStageM == "Late Season") {
      battleTime <- 30
    }
    
    newxLv5 <- mutate(newxLv5, battletime = battleTime)
    
    # process selected cards
    cardsInputLv5 <- c(input$card1Lv5M, input$card2Lv5M, input$card3Lv5M, input$card4Lv5M, input$card5Lv5M, input$card6Lv5M, input$card7Lv5M, input$card8Lv5M)
    cardsInputLv5 <- str_replace_all(cardsInputLv5, " ", "_")
    cardsInputLv5 <- str_replace_all(cardsInputLv5, "-", "_")
    
    for (av5 in 1:length(cardsInputLv5)) {
      for (bv5 in 1:length(names(newxLv5))) {
        if (cardsInputLv5[av5] == names(newxLv5)[bv5]) {
          newxLv5[1,bv5] <- 1
        }
      }
    }
    
    # process selected levels
    lvlInputLv5 <- c(input$card1LvlLv5M, input$card2LvlLv5M, input$card3LvlLv5M, input$card4LvlLv5M, input$card5LvlLv5M, input$card6LvlLv5M, input$card7LvlLv5M, input$card8LvlLv5M)
    
    #cardsInputCleanLv5 <- str_remove_all(cardsInputLv5, "\\.|-")
    cardsInputCleanLv5 <- paste(cardsInputLv5, "_Level", sep = "")
    
    for (cv5 in 1:length(cardsInputCleanLv5)) {
      for (dv5 in 1:length(names(newxLv5))) {
        if (cardsInputCleanLv5[cv5] == names(newxLv5)[dv5]) {
          newxLv5[1,dv5] <- lvlInputLv5[cv5]
        }
      }
    }
    
    # now use newx for prediction
    adjPred1 <- tibble(
      pred = predict.lm(mod3, newdata = newxLv5), # a prediction from newx
      predRounded = round(pred / 100) * 100
    ) %>%
      left_join(., residAdj, by = "predRounded") %>%
      mutate(
        adjPred05 = pred + resid05,
        adjPred25 = pred + resid25,
        adjPred50 = pred + resid50,
        adjPred75 = pred + resid75,
        adjPred95 = pred + resid95
      ) %>%
      select(8:12) %>%
      round()
    
    predictedTrophiesLv5 <- adjPred1
    valuesM$leftTrophies <- predictedTrophiesLv5[[3]] # the 50th percentile prediction
    valuesM$left95 <- predictedTrophiesLv5[[5]] # 95th percentile, for interpretation note
    valuesM$left05 <- predictedTrophiesLv5[[1]] # 05th percentile, for win rate plot
    
    
    
    # for right deck
    
    # fill out newx (prediction data frame) based on input
    newxRv5 <- newxEmpty
    
    # process season stage (battletime)
    if (input$seasonStageM == "Early Season") {
      battleTime <- 10
    }
    if (input$seasonStageM == "Mid Season") {
      battleTime <- 20
    }
    if (input$seasonStageM == "Late Season") {
      battleTime <- 30
    }
    
    newxRv5 <- mutate(newxRv5, battletime = battleTime)
    
    # process selected cards
    cardsInputRv5 <- c(input$card1Rv5M, input$card2Rv5M, input$card3Rv5M, input$card4Rv5M, input$card5Rv5M, input$card6Rv5M, input$card7Rv5M, input$card8Rv5M)
    cardsInputRv5 <- str_replace_all(cardsInputRv5, " ", "_")
    cardsInputRv5 <- str_replace_all(cardsInputRv5, "-", "_")
    
    for (ev5 in 1:length(cardsInputRv5)) {
      for (fv5 in 1:length(names(newxRv5))) {
        if (cardsInputRv5[ev5] == names(newxRv5)[fv5]) {
          newxRv5[1,fv5] <- 1
        }
      }
    }
    
    # process selected levels
    lvlInputRv5 <- c(input$card1LvlRv5M, input$card2LvlRv5M, input$card3LvlRv5M, input$card4LvlRv5M, input$card5LvlRv5M, input$card6LvlRv5M, input$card7LvlRv5M, input$card8LvlRv5M)
    
    #cardsInputCleanRv5 <- str_remove_all(cardsInputRv5, "\\.|-")
    cardsInputCleanRv5 <- paste(cardsInputRv5, "_Level", sep = "")
    
    for (gv5 in 1:length(cardsInputCleanRv5)) {
      for (hv5 in 1:length(names(newxRv5))) {
        if (cardsInputCleanRv5[gv5] == names(newxRv5)[hv5]) {
          newxRv5[1,hv5] <- lvlInputRv5[gv5]
        }
      }
    }
    
    # now use newx for prediction
    adjPred2 <- tibble(
      pred = predict.lm(mod3, newdata = newxRv5), # a prediction from newx
      predRounded = round(pred / 100) * 100
    ) %>%
      left_join(., residAdj, by = "predRounded") %>%
      mutate(
        adjPred05 = pred + resid05,
        adjPred25 = pred + resid25,
        adjPred50 = pred + resid50,
        adjPred75 = pred + resid75,
        adjPred95 = pred + resid95
      ) %>%
      select(8:12) %>%
      round()
    
    predictedTrophiesRv5 <- adjPred2
    
    
    
    # the plot
    df_Barplotv5 <- tibble(
      prediction = c(as_vector(predictedTrophiesLv5), as_vector(predictedTrophiesRv5)),
      display = c(
        predictedTrophiesLv5[[1]] %/% 10,
        predictedTrophiesLv5[[2]] - predictedTrophiesLv5[[1]],
        predictedTrophiesLv5[[3]] - predictedTrophiesLv5[[2]],
        predictedTrophiesLv5[[4]] - predictedTrophiesLv5[[3]],
        predictedTrophiesLv5[[5]] - predictedTrophiesLv5[[4]],
        predictedTrophiesRv5[[1]] %/% 10,
        predictedTrophiesRv5[[2]] - predictedTrophiesRv5[[1]],
        predictedTrophiesRv5[[3]] - predictedTrophiesRv5[[2]],
        predictedTrophiesRv5[[4]] - predictedTrophiesRv5[[3]],
        predictedTrophiesRv5[[5]] - predictedTrophiesRv5[[4]]
      ),
      cumDisplay = c(
        cumsum(display[1:5]),
        cumsum(display[6:10])
      ),
      Percentile = paste(str_remove_all(c(names(predictedTrophiesLv5), names(predictedTrophiesRv5)), "[:alpha:]"), "th"),
      deck = c("Deck 1", "Deck 1", "Deck 1", "Deck 1", "Deck 1", "Deck 2", "Deck 2", "Deck 2", "Deck 2", "Deck 2")
    )
    
    df_Barplotv5 %>%
      ggplot(., aes(x = deck)) +
      geom_col(aes(y = display, fill = Percentile), width = 0.1, position = position_stack(reverse = T)) +
      geom_label(data = df_Barplotv5[df_Barplotv5$deck == "Deck 1", ], aes(y = cumDisplay, label = prediction, hjust = 1.5), size = 5) +
      geom_label(data = df_Barplotv5[df_Barplotv5$deck == "Deck 2", ], aes(y = cumDisplay, label = prediction, hjust = -0.5), size = 5) +
      theme_minimal() +
      scale_fill_viridis_d(option = "viridis") +
      guides(fill = guide_legend(reverse=TRUE)) +
      theme(
        legend.position = c(0.5, 0.25),
        legend.text = element_text(size = rel(1.2)),
        legend.title = element_text(size = rel(1.2)),
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



# interpretation note
output$interpretNoteM <- function(input, sess) {
  renderText({
    paste("For instance,", valuesM$left95, "is the estimated 95th percentile trophy rating for all players using Deck 1. If you are above that rating, then you are in the top 5% of players with a similar deck.")
  })
}



# route note
output$noteRouteM <- function(input, sess) {
  renderText({
    if (is.null(input$opRouteLv5M)) {
      return (NULL)
    } else {
      'Note: The rows in the table are adjustable; simply drag them around to change the order. Then, click on "Update Plot" button to see how the path changes. Also, all trophy predictions here are in the 50th percentile.'
    }
  })
}



# plot title
output$plotTitleM <- function(input, sess) {
  renderText({
    if (is.null(input$opRouteLv5M)) {
      return (NULL)
    } else {
      "Line Plot of Trophies vs Cumulative Gold Spent"
    }
  })
}



# for line plot
output$geomLineM <- function(input, sess) {
  renderPlotly({
    if (is.null(input$opRouteLv5M)) {
      return(NULL)
    } else {# ==2 temporary bug fix
      if (valuesM$viewCounter %in% c(0, 1)) {
        data <- valuesM$orig
        data[, "Cumulative_Gold"] <- cumsum(data$`Gold Required`)
        data[, "Trophies"] <- cumsum(data$`Trophy\nGain`) + valuesM$leftTrophies
        options(scipen=10000)
        
        g <-
          data %>%
          ggplot(.,aes(x = Cumulative_Gold, y = Trophies)
          ) +
          geom_point(
            aes(
              text = paste(
                "Step", Step,
                "\nUpgrade", Card, "to Level", `Upgrade To`,
                "\nGold Required:", `Gold Required`,
                "\nTrophy Gain:", `Trophy\nGain`,
                "\nResulted Trophies:", Trophies,
                "\nCumulative Gold Spent:", Cumulative_Gold
              )
            )
          ) +
          geom_line(size = rel(1.15)) +
          theme_bw() +
          labs(
            x = "Cumulative Gold Spent",
            y = "Trophies"
          )
        
        p <- ggplotly(g, tooltip = "text")
        p
      } else {
        if (!is.null(valuesM$DF)) {
          data1 <- valuesM$orig
          data1[, "Cumulative_Gold"] <- cumsum(data1$`Gold Required`)
          data1[, "Trophies"] <- cumsum(data1$`Trophy\nGain`) + valuesM$leftTrophies
          data1[, "Route"] <- "Optimized Route"
          
          data2 <- hot_to_r(input$opRouteLv5M)
          data2[, "Cumulative_Gold"] <- cumsum(data2$`Gold Required`)
          data2[, "Trophies"] <- cumsum(data2$`Trophy\nGain`) + valuesM$leftTrophies
          data2[, "Route"] <- "Adjusted Route"
          data2[, "Step"] <- data1[, "Step"] # display the updated step on the plot
          
          newData <- bind_rows(data1, data2)
          newData$Route <- fct_inorder(newData$Route)
          
          options(scipen=10000)
          
          g <-
            ggplot(newData, aes(x = Cumulative_Gold, y = Trophies, color = Route)) +
            geom_point(
              aes(
                text = paste(
                  "Step", Step,
                  "\nUpgrade", Card, "to Level", `Upgrade To`,
                  "\nGold Required:", `Gold Required`,
                  "\nTrophy Gain:", `Trophy\nGain`,
                  "\nResulted Trophies:", Trophies,
                  "\nCumulative Gold Spent:", Cumulative_Gold
                )
              )
            ) +
            geom_line(size = 1.15) +
            theme_bw() +
            labs(
              x = "Cumulative Gold Spent",
              y = "Trophies"
            ) +
            scale_color_manual(values = c("black", "red")) +
            theme(
              legend.position = "bottom",
              legend.title = element_blank()
            )
          p <-
            ggplotly(g, tooltip = "text") %>%
            layout(legend = list(orientation = "h", x = 0, y = -0.2))
          p
        } else {
          data <- hot_to_r(input$opRouteLv5M)
          data[, "Cumulative_Gold"] <- cumsum(data$`Gold Required`)
          data[, "Trophies"] <- cumsum(data$`Trophy\nGain`) + valuesM$leftTrophies
          options(scipen=10000)
          
          g <-
            data %>%
            ggplot(., aes(x = Cumulative_Gold, y = Trophies)) +
            geom_point(
              aes(
                text = paste(
                  "Step", Step,
                  "\nUpgrade", Card, "to Level", `Upgrade To`,
                  "\nGold Required:", `Gold Required`,
                  "\nTrophy Gain:", `Trophy\nGain`,
                  "\nResulted Trophies:", Trophies,
                  "\nCumulative Gold Spent:", Cumulative_Gold
                )
              )
            ) +
            geom_line(size = 1.15) +
            theme_bw() +
            labs(
              x = "Cumulative Gold Spent",
              y = "Trophies"
            )
          
          p <- ggplotly(g, tooltip = "text")
          p
        }
      }
    }
  })
}



# display hot
output$opRouteLv5M <- function(input, sess) {
  renderRHandsontable({
    valuesM$hot
  })
}



# display win rate plot
output$winRatePlotM <- function(input, sess) {
  renderPlotly({
    x <- (valuesM$res)$x
    
    g <- ggplot(data = x, aes(x = trophies, y = win.rate, color = cardlevel)) +
      geom_hline(yintercept = 50, linetype = 2) +
      geom_vline(xintercept = valuesM$res$trophies[1], color = "grey") +
      annotate("text", x=valuesM$res$trophies[1], y=0, label= "5th Percentile Trophy")
    
    if(valuesM$res$trophies[2] > valuesM$res$trophies[1]){
      g <- g +   
        geom_vline(xintercept = valuesM$res$trophies[2], color = "grey") +
        annotate("text", x=valuesM$res$trophies[2], y=10, label= "95th Percentile Trophy")            
    }
    
    g <- g + geom_smooth(se = F, size = 2)  +
      scale_color_brewer(palette = "Dark2", name = "Card (Level)") +
      theme_bw() +
      xlab("Trophies") + ylab("Win Rate")
      
    ggplotly(g)  %>%
      layout(legend = list(orientation = "h", x = 0, y = -0.5))
  })
}










mwsApp(ui_list, serv_calc, output)
