#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# National College of Ireland
# Data Visualization Project
# Interactive Visualization 1
# Title: Overview of Flights in Brazil by date
# Author: Douglas Zikcuhr

# Loading libraries
library(lubridate)
library(tidyverse)
library(dplyr)
library(leaflet)
library(sp)
library(geosphere)
library(shiny)
library(leaflet.extras)
library(DT)
library(data.table)
library(plotly)

# Function to load and return the dataframe
loadTotalByDay <- function(file){
  # Read the file using fread from data.table library - Much faster!
  df <- fread(file = file,
              colClasses = c("factor","factor","double","double","factor","factor","factor","double","double",
                             "factor","factor","character","character","numeric","numeric","numeric"))
  
  # Converting the DepartureYMD variable to Date
  df$DepartureYMD <- ymd(df$DepartureYMD)
  
  # Rouding the Average Deplay - We don't want the user to see a lot of decimal places, right?
  df <- df %>%
    mutate(AverageDepartureDelay = round(AverageDepartureDelay,2),
           AverageArrivalDelay = round(AverageArrivalDelay,2)) 
  
  # Return the dataframe
  return(df)
}

# Load the dataframe totalByDay - previously manipulated to be aggregated by date and route.
totalByDay <- loadTotalByDay(file = 'TotalByDay.csv')

# Shiny User interface
ui <- fluidPage(
  
  # Title of panel
  titlePanel(paste("Brazilian Flight Radar from","31/12/2014","to","31/07/2017"), 
             windowTitle = "Data Visualization CA2 - Plot 1"),
  sidebarLayout(
    
    # The panel has the select input
    sidebarPanel(
      width = 3,
      
      # Well panel keep things tidy - Flight selection is the first
      wellPanel(h3("Flight Selection"),
                
                # The helptext output is used to give the user hints of how to use the dashboard
                helpText("Use these options to filter the records."),
                
                # Select Flight date input
                dateInput(inputId = "date", 
                          label = "Date", 
                          min = min(totalByDay$DepartureYMD), 
                          max = max(totalByDay$DepartureYMD),
                          value = max(totalByDay$DepartureYMD)-1
                ),
                
                # Select Flight Type input
                checkboxGroupInput(inputId = "type",
                                   label = "Flight Type",
                                   choices = levels(totalByDay$Flight.Type),
                                   selected = levels(totalByDay$Flight.Type)
                )
      ),
      
      # Well panel for Sankey Diagram configuration
      wellPanel(h3("Sankey Configuration"),
                
                # Hint about this panel
                helpText("Change the number of rows and check the Sankey Diagram"),
                
                # Number of top routes for Sankey
                sliderInput(inputId = "sankey_rows",
                            label = "Number of Top Routes",
                            min = 1,
                            max = 50,
                            step = 5,
                            value = 20
                )
      ),
      wellPanel(h5("Built with",
                   img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                   "by",
                   img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
                   "."))
    ),
    
    # Output panel:
    mainPanel(
      
      # Tabset to create different tabs      
      tabsetPanel(
        
        #First tab for Map
        tabPanel("Map",
                 
                 # Well panel to organise the output
                 wellPanel(
                   
                   # Description of the plot
                   h3("Route Map From/To Brazil on the date"),
                   hr(),
                   
                   # Hint about how the map works.
                   helpText("Click on the lines to see the route. Click on the red circle to see."),
                   br(),
                   
                   # Outputting the leaflet map.
                   leafletOutput(outputId = "mymap"),
                   
                   # Another hint below the map
                   helpText("Red lines means heavy flow. Green line means light flow")
                 )),
        
        # Second tab for Sankey Diagram
        tabPanel("Sankey Diagram",
                 
                 #Well panel to organise the output
                 wellPanel(
                   # Description of the plot
                   h3("Sankey Diagram - Top busiest routes"),
                   hr(),
                   
                   # A hint about the Sankey Diagram
                   helpText("Click over the boxes to see flow information."),
                   
                   # Outputting the sankey diagram
                   plotlyOutput(outputId = "sankey")
                 )),
        
        # Third tab - Showing the data and allowing the user to download it
        tabPanel("Data",
                 
                 # Well panel for tidying the visualization
                 wellPanel(
                   
                   #Description of the tab
                   h3("Detailed data"),
                   hr(),
                   
                   # A hint about the Data Table
                   helpText("It's possible to change the data that is being listed using the filter options."),
                   
                   # Outputting the table
                   dataTableOutput(outputId = "datatable"),
                   
                   # A hint about the Data Table
                   helpText("Click on the button to download the data"),
                   
                   # Option to download the correspondent data
                   downloadButton("downloadData", "Download")
                 )
        )
      ),
      
      # Listing the total of records found.
      uiOutput(outputId = "n"),
      
      h5(img(src = "https://www.ncirl.ie/Portals/0/nciLogo.png", height = "30px"),
         br(),
         "Student: Douglas Zickuhr",
         br(),
         "Student Number: 17111781")
    )
  )
)

# Server - Shinny
server <- function(input, output, session) {
  
  # Reactive function to filter the data based on the input
  oneDay <- reactive({
    
    # Requesting input$data and input$type
    req(input$date)
    req(input$type)
    
    # Returning the data filtered
    filter(totalByDay, DepartureYMD == input$date & 
             Flight.Type %in% input$type)
  })
  
  # Reactive function to create the airport points based on the filtered data
  airportsPoints <- reactive({
    req(input$date)
    req(input$type)
    aiportsPointsDF <- oneDay()
    aiportsPointsDF1 <- aiportsPointsDF %>% 
      group_by(City.From,Latitude.From,Longitude.From) %>%
      summarise() %>%
      mutate(City = City.From, Lat = Latitude.From, Long = Longitude.From) %>%
      select(City,Lat,Long)
    
    aiportsPointsDF2 <- aiportsPointsDF %>% 
      group_by(City.To,Latitude.To,Longitude.To) %>%
      summarise() %>%
      mutate(City = City.To, Lat = Latitude.To, Long = Longitude.To) %>%
      select(City,Lat,Long)
    
    airportsOutput <- rbind(aiportsPointsDF1,aiportsPointsDF1) %>%
      distinct(City,Lat,Long)
    
    return(airportsOutput)
  })
  
  points <- reactive({
    req(input$date)
    req(input$type)
    linesGeneration <- function(x){
      return <- gcIntermediate(c(oneDay$Longitude.To[x], 
                                 oneDay$Latitude.To[x]), 
                               c(oneDay$Longitude.From[x], 
                                 oneDay$Latitude.From[x]),
                               n = 50, addStartEnd = TRUE, 
                               breakAtDateLine = TRUE)
      return <- Line(return)
      return <- Lines(list(return), ID = x)
      return
    }
    
    oneDay <- oneDay()
    Sl <- map(1:nrow(oneDay), linesGeneration)
    Sl <- SpatialLines(Sl)
    spatdf <- SpatialLinesDataFrame(Sl, oneDay, match.ID = TRUE)
    spatdf
  })
  
  sankey <- reactive({
    req(input$date)
    req(input$type)
    req(input$sankey_rows)
    df <- oneDay() %>%
      select(City.From,City.To,Total) %>%
      arrange(desc(Total)) %>%
      distinct() %>%
      head(input$sankey_rows)
    
    nodes1 <- data.frame(name=c(unique(levels(factor(df[,1])))))
    nodes2 <- data.frame(name=c(unique(levels(factor(df[,2])))))
    nam1 <- seq_along(nodes1[,1])-1
    nam2 <- seq(length(nam1),length(nam1) + length(nodes2[,1])-1)
    names(nam1) <- nodes1[,1]
    names(nam2) <- nodes2[,1]
    links <- data.frame(source = nam1[as.character(df[,1])],
                        target = nam2[as.character(df[,2])],
                        value = df[,3])
    
    p <- plot_ly(
      type = "sankey",
      orientation = "h",
      
      node = list(
        label = c(names(nam1),names(nam2)),
        pad = 15,
        thickness = 20,
        line = list(
          color = "black",
          width = 0.5
        )
      ),
      
      link = list(
        source = links$source,
        target = links$target,
        value =  links$value
      )
    ) %>% 
      layout(
        title = "Basic Sankey Diagram",
        font = list(
          size = 10
        )
      )
    
    p
  })
  
  # Create scatterplot object the plotOutput function is expecting 
  output$mymap <- renderLeaflet({
    factpal <- colorFactor(palette =  c('green','yellow','orange','red'), points()$Total)
    leaflet(data = points()) %>% 
      addTiles() %>% 
      addPolylines(fillOpacity = 0.8, 
                   color = ~factpal(points()$Total),
                   weight = 1.5,
                   highlight = highlightOptions(
                     color = "black",
                     fillOpacity = 1,
                     bringToFront = TRUE),
                   label = paste(oneDay()$Route," - ",oneDay()$Total, "flights.")) %>%
      addCircleMarkers(airportsPoints()$Long, 
                       airportsPoints()$Lat, 
                       radius=1, 
                       color = "red",
                       fillOpacity = 1, 
                       fill = 1, 
                       fillColor = 1,
                       label = airportsPoints()$City) %>%
      suspendScroll()
  })
  
  output$datatable <- renderDataTable({
    table <- datatable(oneDay()[,c("Route","Total")])
    return(table)
  })
  
  output$sankey <- renderPlotly({
    sankey()
  })
  
  output$n <- renderUI({
    req(input$type)
    types <- oneDay()
    
    types <- types %>%
      filter(Flight.Type %in% input$type) %>%
      group_by(Flight.Type) %>%
      summarise(Total = sum(Total))
    
    n <- matrix("",length(types$Flight.Type))
    
    for (i in 1:length(types$Flight.Type)){
      desc <- levels(types$Flight.Type)[as.numeric(types[i,1])]
      n[i] <- paste(types[i,2],desc, "flights were found at this date.<br>")
    }
    
    return(HTML(n))
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("flights-",format.Date(input$date,"%Y-%m-%d") , ".csv", sep = "")
    },
    content = function(file) {
      write.csv(oneDay()[,c("Route","Total")], file, row.names = FALSE)
    }
  )
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)