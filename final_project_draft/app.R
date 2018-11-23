# Load libraries 

library(shiny)
library(tidyverse)
library(stringr)
library(rsconnect)
library(leaflet)
library(rgdal)
library(shinythemes)
library(plotly)

# Read in the data

crime_master <- read_rds("r_4_tidy.rds")

# Prepare two data sets 
# one for the plot and one for the map 

crime_plot <- crime_master
crime_map <- crime_master %>%
  mutate(YEAR = as.character(YEAR))


# Prepare the shape file for the map 
# Read it in
# Project the shape file 

rf_map <- readOGR(dsn = "/Users/MLupion/Desktop/GOV 1005/GOV_1005_Final_Project/RUS_adm", layer = "RUS_adm1")
rf_map <- spTransform(rf_map, CRS("+init=epsg:4326"))


# Create nice labels for the user-selected variables

crime_options <- c("Road accidents" = "ROADACCIDENT", 
                   "Victims of road accidents" = "ROADVICTIM",
                   "Crime share" = "CRIMESHARE", 
                   "Murders" = "MURDER",
                   "Incidences of rape" = "RAPE",
                   "Robberies" = "ROBBERY",
                   "Incidences of hooliganism" = "HOOLIGANISM",
                   "White-collar crimes" = "ECONCRIME",
                   "Incidences of juvenile crime" = "JUVENILECRIME") 

# Define UI for application with a nice theme

ui <- fluidPage(theme = shinytheme("cerulean"),
   
   # Application title
   
   titlePanel("Crime in the Russian Regions, 1990 - 2010"),
   
   # Sidebar layour
   
   sidebarLayout( 
     sidebarPanel( 
       
       h3("Select the inputs"),
       
       # Let users select the year to map
       
       selectInput(inputId = "year", #internal label 
                   label = "Year to map", #label that user sees
                   choices = c(crime_map$YEAR), #vector of choices for user to pick from 
                   selected = "1990"),
       
       # Let users select the indicator to plot
       
       selectInput(inputId = "y", # internal label 
                   label = "Indicator to display on Y-axis", # label that user sees
                   choices = crime_options, # vector of choices for user to pick from 
                  selected = crime_options[3]),
       
       # Let users select the regions to plot
       
       selectizeInput(inputId = "region", # internal label
                      label = "Select regions", # label that user sees
                      choices = c(crime_plot$NAME), # choose from this list 
                      multiple = TRUE, # can choose multiple 
                      options = list(maxItems = 5), # can choose up to five
                      selected = "Moscow")), 
     # Outputs
     
     mainPanel(
       
       # Use a tabset structure with three tabs
       # one for the info about the app
       # another for the plotly output
       # and another for the map leaflet output
       
       tabsetPanel(type = "tabs",
                   tabPanel("About this app", htmlOutput("about")),
                   tabPanel("Plot the indicators", plotlyOutput("scatterplot")),
                   tabPanel("Map an indicator", leafletOutput("map", width = "100%", height = "500px")))
       
     )))
       

# Server

server <- function(input, output){
  
  # Reactive that subsets the regions to plot
  # by filtering the crime_plot data for the user inputed regions
  
  regions_subset <- reactive({ 
    req(input$region)
    filter(crime_plot, NAME %in% input$region)})
  
  # Reactive that filters the crime_map data for the user-selected year
  
  map_subset <- reactive({
    req(input$year)
    filter(crime_map, YEAR == input$year) 

    
  })

  
  # Scatterplot output 
  # Wrap ggplot in ggplotly wrapper
  
  output$scatterplot <- renderPlotly({
  ggplotly(ggplot(data = regions_subset(), aes_string(x = "YEAR", y = input$y, color = "NAME")) + #plot year on x and value on y
      geom_point() + #color by region
      labs(x = "Year", y = names(crime_options[which(crime_options == input$y)])) +
      theme(text = element_text(size = 10), 
            axis.text.y = element_text(angle = 90, hjust = 1)) +
      scale_color_discrete(name = "Regions")) })
  
  
  # Map output
  # Merge the shapefile with the sub_setted map data
  # Color by the selected indicator 
  # Set the options for the leaflet viewer 
  # Allow the user to drag
  # Add a CartoDB base map
  # Set the default view
  # Set the max bounds
  # Add the shapefile with labels
  # Color by the coloring set up
    
  output$map <- renderLeaflet({
    
    map_var <- input$y
    
    rf_map <- merge(rf_map, map_subset(), by = "ID_1", duplicateGeoms = TRUE)
    coloring <- colorNumeric(palette = "Blues",
                             domain = rf_map@data$CRIMESHARE)
    m <- rf_map %>%
      leaflet(options = leafletOptions(dragging = TRUE)) %>%
      addProviderTiles(provider = "CartoDB") %>%
      setView(lng = 37.618423, lat = 55.751244, zoom = 3) %>%
      setMaxBounds(lng1 = 40, lat1 = 30, lng2 = 150, lat2 = 100) %>%
      addPolygons(weight = 1, 
                  label = ~paste0(NAME, ", ", CRIMESHARE),
                  color = ~coloring(CRIMESHARE)) %>%
      
      # Add a legend in the bottom
      
      addLegend("bottomright", 
                pal = coloring, 
                values = ~CRIMESHARE,
                title = names(crime_options[which(crime_options == input$y)]),
                opacity = 1)
    m
  })
  
  output$about <- renderUI({
    HTML(paste(
      h3("Summary"),
      p("This application allows users to visualize crime data for the Russian Federation's federal subjects (administrative units) from 1990 through 2010."),
      h3("Source"), 
      p("Inter-university Consortium for Political and Social Research (ICPSR):"),  
      p("ICPSR 35355 Aggregate Data, Regions of Russia (RoR), 1990 - 2010, created by Irina Mirkina."),
      tags$a(href = "https://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/35355", "Click here to learn more about the data.")))
  })
  
  }

# Run the application 
shinyApp(ui = ui, server = server)