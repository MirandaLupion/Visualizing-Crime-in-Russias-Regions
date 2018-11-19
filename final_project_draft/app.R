library(shiny)
library(tidyverse)
library(stringr)
library(rsconnect)
library(leaflet)
library(rgdal)
library(shinythemes)
library(plotly)
crime_master <- read_rds("r_4_tidy.rds")

# Prepare two data sets
crime_plot <- crime_master
crime_map <- crime_master %>%
  mutate(YEAR = as.character(YEAR))


# Prepare shape file
rf_map <- readOGR(dsn = "/Users/MLupion/Desktop/GOV 1005/GOV_1005_Final_Project/RUS_adm", layer = "RUS_adm1")
rf_map <- spTransform(rf_map, CRS("+init=epsg:4326"))

crime_options <- c("Road accidents" = "ROADACCIDENT", 
                   "Crime share" = "CRIMESHARE", 
                   "Murders" = "MURDER") 

# Define UI for application that draws a graph
ui <- fluidPage(theme = shinytheme("cerulean"),
   
   # Application title
   titlePanel("Crime in the Russian Regions, 1990 - 2010"),
   
   sidebarLayout( 
     sidebarPanel( 
       # Y axis variable for the chart and for the map
       h3("Select the inputs"),
       selectInput(inputId = "year", #internal label 
                   label = "Year to map", #label that user sees
                   choices = c(crime_map$YEAR), #vector of choices for user to pick from 
                   selected = "1990"),
       
       selectInput(inputId = "y", #internal label 
                   label = "Indicator to display on Y-axis", #label that user sees
                   choices = crime_options, #vector of choices for user to pick from 
                  selected = crime_options[1]),
       
       selectizeInput(inputId = "region", #internal label
                      label = "Select regions", #label that user sees
                      choices = c(crime_plot$NAME), #choose from this list 
                      multiple = TRUE, # can choose multiple 
                      options = list(maxItems = 5))), #can choose up to five
     # Outputs 
     mainPanel(
       tabsetPanel(type = "tabs",
                   tabPanel("About this app", htmlOutput("about")),
                   tabPanel("Plot the indicators", plotlyOutput("scatterplot")),
                   tabPanel("Map an indicator", leafletOutput("map", width = "100%", height = "500px")))
       
     )))
       

# Server
server <- function(input, output){
  regions_subset <- reactive({ #make the regions included in the data set only the ones that the user chose 
    req(input$region)
    filter(crime_plot, NAME %in% input$region)})
  
  map_subset <- reactive({
    req(input$year)
    filter(crime_map, YEAR == input$year) 

    
  })

  output$scatterplot <- renderPlotly({
    varnames <- c("Road accidents", "Crime share", "Murders")
  ggplotly(ggplot(data = regions_subset(), aes_string(x = "YEAR", y = input$y, color = "NAME")) + #plot year on x and value on y
      geom_point() + #color by region
      labs(x = "Year", y = names(crime_options[which(crime_options == input$y)])) +
      theme(text = element_text(size = 10), 
            axis.text.y = element_text(angle = 90, hjust = 1)) +
      scale_color_discrete(name = "Regions")) })
    
  output$map <- renderLeaflet({
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
      addLegend("bottomright", 
                pal = coloring, 
                values = ~CRIMESHARE,
                title = "title here",
                opacity = 1)
    m
  })
  
  output$about <- renderUI({
    HTML(paste(
      h3("Source"), 
      p("Inter-university Consortium for Political and Social Research (ICPSR):"),  
      p("ICPSR 35355 Aggregate Data, Regions of Russia (RoR), 1990 - 2010, created by Irina Mirkina."),
      tags$a(href = "https://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/35355", "Click here to learn more about the data.")))
  })
  
  }

# Run the application 
shinyApp(ui = ui, server = server)