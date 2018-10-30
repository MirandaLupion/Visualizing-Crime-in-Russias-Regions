library(shiny)
library(tidyverse)
library(stringr)
library(rsconnect)
library(leaflet)
library(rgdal)

# Data preparation 

crime <- read_rds("r_4_tidy.rds")

# Leaflet preparation
rf_map <- readOGR(dsn = "/Users/MLupion/Desktop/GOV 1005/GOV_1005_Final_Project/RUS_adm", layer = "RUS_adm1")

rf_map <- spTransform(rf_map, CRS("+init=epsg:4326"))

# Define UI for application that draws a graph
ui <- fluidPage(
   
   # Application title
   titlePanel("Crime in the Russian Regions, 1990 - 2010"),
   
   sidebarLayout( 
     sidebarPanel( 
       # Y axis variable for the chart and for the map
       selectInput(inputId = "y", #internal label 
                   label = "Y-axis:", #label that user sees
                   choices = c("Road accidents" = "ROADACCIDENT", 
                               "Crime share" = "CRIMESHARE", 
                               "Murders" = "MURDER"), #vector of choices for user to pick from 
                  selected = "ROADACCIDENT"),
     
      # for the map
      selectInput(inputId = "year", #internal label 
                   label = "Year to map", #label that user sees
                   choices = c(crime$YEAR), #vector of choices for user to pick from 
                   selected = "1990"),
       
        # Select regions 
      selectizeInput(inputId = "region", #internal label
                          label = "Select regions", #label that user sees
                          choices = c(crime$NAME), #choose from this list 
                          multiple = TRUE, # can choose multiple 
                          options = list(maxItems = 5))), #can choose up to five
     # Outputs 
     mainPanel(
       plotOutput(outputId = "scatterplot"),
       leafletOutput("map"),
       p("Source - Inter-university Consortium for Political and Social Research (ICPSR):"),  
       p("ICPSR 35355 Aggregate Data, Regions of Russia (RoR), 1990 - 2010, created by Irina Mirkina."),
       tags$a(href = "https://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/35355", "Click here to learn more about the data."))))

# Server
server <- function(input, output){
  regions_subset <- reactive({ #make the regions included in the data set only the ones that the user chose 
    req(input$region)
    filter(crime, NAME %in% input$region)
  })
  output$scatterplot <- renderPlot({
    ggplot(data = regions_subset(), aes_string(x = regions_subset()$YEAR, y = input$y)) + #plot year on x and value on y
      geom_point(aes(color = regions_subset()$NAME)) + #color by region
      labs(x = "Year", y = input$y) +
      scale_color_discrete(name = "Regions")
    
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(provider = "CartoDB") %>%
      addPolygons(data = rf_map)
  })  
  })}

# Run the application 
shinyApp(ui = ui, server = server)