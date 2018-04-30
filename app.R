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
    
    # Request input$date and input$type to go ahead
    req(input$date)
    req(input$type)
    
    # Create a dataframe with the filtered oneDay()
    aiportsPointsDF <- oneDay()
    
    # Aggregating the data related to the Airport.From
    aiportsPointsDF1 <- aiportsPointsDF %>% 
      group_by(City.From,Latitude.From,Longitude.From) %>%
      summarise() %>%
      mutate(City = City.From, Lat = Latitude.From, Long = Longitude.From) %>%
      select(City,Lat,Long)
    
    # Aggregating the data related to the Airport.To
    aiportsPointsDF2 <- aiportsPointsDF %>% 
      group_by(City.To,Latitude.To,Longitude.To) %>%
      summarise() %>%
      mutate(City = City.To, Lat = Latitude.To, Long = Longitude.To) %>%
      select(City,Lat,Long)
    
    # Creating a dataframe to output
    airportsOutput <- rbind(aiportsPointsDF1,aiportsPointsDF1) %>%
      distinct(City,Lat,Long)
    
    # Returning the dataframe to be plot
    return(airportsOutput)
  })
  
  # Reactive function to create the points to be plot on the map
  points <- reactive({
    
    # Requesting input$date and input$type
    req(input$date)
    req(input$type)
    
    # Function to generate the lines betwen the points
    linesGeneration <- function(x){
      
      # gcIntermediate function from geosphere package
      return <- gcIntermediate(c(oneDay$Longitude.To[x], 
                                 oneDay$Latitude.To[x]), 
                               c(oneDay$Longitude.From[x], 
                                 oneDay$Latitude.From[x]),
                               n = 50, addStartEnd = TRUE, 
                               breakAtDateLine = TRUE)
      
      # Create line from return
      return <- Line(return)
      
      # Create list of lines
      return <- Lines(list(return), ID = x)
      
      # Returning the generated Lines
      return
    }
    
    # Creating another Dataframe with the filtered DF oneDay()
    oneDay <- oneDay()
    
    # Interating the dataframe calling the linesGeneration function
    Sl <- map(1:nrow(oneDay), linesGeneration)
    
    # SpatialLines function from geosphere package
    Sl <- SpatialLines(Sl)
    
    # Creating SpatialLinesDataFrame
    spatdf <- SpatialLinesDataFrame(Sl, oneDay, match.ID = TRUE)
    
    # Returning the spatial data frame
    spatdf
  })
  
  
  # Reactive function to generate the Sankey Diagram
  sankey <- reactive({
    # Requesting input$date, input$type and input$sankey_rows
    req(input$date)
    req(input$type)
    req(input$sankey_rows)
    
    # Generating a dataframe to format the data
    df <- oneDay() %>%
      select(City.From,City.To,Total) %>%
      arrange(desc(Total)) %>%
      distinct() %>%
      head(input$sankey_rows)
    
    # Creating nodes for Sankey
    nodes1 <- data.frame(name=c(unique(levels(factor(df[,1])))))
    nodes2 <- data.frame(name=c(unique(levels(factor(df[,2])))))
    
    # Creating names for Sankey
    nam1 <- seq_along(nodes1[,1])-1
    nam2 <- seq(length(nam1),length(nam1) + length(nodes2[,1])-1)
    
    # Assigning names to nam1 and nam2
    names(nam1) <- nodes1[,1]
    names(nam2) <- nodes2[,1]
    
    # Generating links for nodes
    links <- data.frame(source = nam1[as.character(df[,1])],
                        target = nam2[as.character(df[,2])],
                        value = df[,3])
    
    
    # Creating the plot - plotly library
    p <- plot_ly(
      
      # Fixed options
      type = "sankey",
      orientation = "h",
      
      # Generating nodes
      node = list(
        label = c(names(nam1),names(nam2)),
        pad = 15,
        thickness = 20,
        line = list(
          color = "black",
          width = 0.5
        )
      ),
      
      # Generating links
      link = list(
        source = links$source,
        target = links$target,
        value =  links$value
      )
    )
    
    # Returning the plot
    p
  })
  
  # Rendering the Map
  output$mymap <- renderLeaflet({
    
    # Generating a pallete of colours based on the total of flights
    factpal <- colorFactor(palette =  c('green','yellow','orange','red'), points()$Total)
    
    # Plotting the map
    leaflet(data = points()) %>% 
      addTiles() %>% 
      # Adding the polylines
      addPolylines(fillOpacity = 0.8, 
                   color = ~factpal(points()$Total),
                   weight = 1.5,
                   highlight = highlightOptions(
                     color = "black",
                     fillOpacity = 1,
                     bringToFront = TRUE),
                   label = paste(oneDay()$Route," - ",oneDay()$Total, "flights.")) %>%
      # Adding the circles
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
  
  # Rendering the Sankey Diagram
  output$sankey <- renderPlotly({
    sankey()
  })
  
  # Rendering the Data Table
  output$datatable <- renderDataTable({
    table <- datatable(oneDay()[,c("Route","Total")])
    return(table)
  })
  
  # Rendering the information about the filtered data
  output$n <- renderUI({
    
    # Requesting the type
    req(input$type)
    
    # Creating a new dataframe to list the data
    types <- oneDay()
    
    # Aggregating the data to generate the total by type
    types <- types %>%
      filter(Flight.Type %in% input$type) %>%
      group_by(Flight.Type) %>%
      summarise(Total = sum(Total))
    
    # Generating a matrix with the same length of the dataframe
    n <- matrix("",length(types$Flight.Type))
    
    # Iterating the dataframe and populating the matrix to output
    for (i in 1:length(types$Flight.Type)){
      desc <- levels(types$Flight.Type)[as.numeric(types[i,1])]
      n[i] <- paste(types[i,2],desc, "flights were found at this date.<br>")
    }
    
    # Returning the matrix as HTML
    return(HTML(n))
  })
  
  # Rendering the option to download the file
  output$downloadData <- downloadHandler(
    
    # Function to return the filename
    filename = function() {
      paste("flights",format.Date(input$date,"%Y-%m-%d") , ".csv", sep = "")
    },
    # Function to return the data
    content = function(file) {
      write.csv(oneDay()[,c("Route","Total")], file, row.names = FALSE)
    }
  )
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)