#install.packages( "maps", dependencies = TRUE) #run this to install R package maps
################################- warning this will update existing packages if already installed

#*save the following code in a file named app.R *
library(shiny)
library(maps)

##Section 1 ____________________________________________________
#load your data or create a data table as follows:
countyData = read.table(
  text = "State County
 Delaware Kent
 Delaware 'New Castle'
 Delaware Sussex
 'Rhode Island' Bristol
 'Rhode Island' Kent
 'Rhode Island' Newport
 'Rhode Island' Providence
 'Rhode Island' Washington",
  header = TRUE, stringsAsFactors = FALSE)

##Section 2 ____________________________________________________
#set up the user interface
ui = shinyUI(
  fluidPage( #allows layout to fill browser window
    titlePanel("Reactive select input boxes"),
    #adds a title to page and browser tab
    #-use "title = 'tab name'" to name browser tab
    sidebarPanel( #designates location of following items
      htmlOutput("state_selector"),#add selectinput boxs
      htmlOutput("county_selector")# from objects created in server
    ),
    
    mainPanel(
      plotOutput("plot1") #put plot item in main area
    )
  ) )


##Section 3 ____________________________________________________
#server controls what is displayed by the user interface
server = shinyServer(function(input, output) {
  #creates logic behind ui outputs ** pay attention to letter case in names
  
  output$state_selector = renderUI({ #creates State select box object called in ui
    selectInput(inputId = "state", #name of input
                label = "State:", #label displayed in ui
                choices = as.character(unique(countyData$State)),
                # calls unique values from the State column in the previously created table
                selected = "Delaware") #default choice (not required)
  })
  output$county_selector = renderUI({#creates County select box object called in ui
    
    data_available = countyData[countyData$State == input$state, "County"]
    #creates a reactive list of available counties based on the State selection made
    
    selectInput(inputId = "county", #name of input
                label = "County:", #label displayed in ui
                choices = unique(data_available), #calls list of available counties
                selected = unique(data_available)[1])
  })
  
  output$plot1 = renderPlot({ #creates a the plot to go in the mainPanel
    map('county', region = input$state)
    #uses the map function based on the state selected
    map('county', region =paste(input$state,input$county, sep=','),
        add = T, fill = T, col = 'red')
    #adds plot of the selected county filled in red
  })
})#close the shinyServer

##Section 4____________________________________________________
shinyApp(ui = ui, server = server) #need this if combining ui and server into one file.