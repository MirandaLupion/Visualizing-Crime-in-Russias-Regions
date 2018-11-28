# Load libraries 

library(shiny)
library(stringr)
library(rsconnect)
library(leaflet)
library(rgdal)
library(shinythemes)
library(plotly)
library(DT)
library(tidyverse)


# Read in the master rds file 

crime_master <- read_rds("r_4_tidy.rds")

# Prepare two data sets 
# one for the plot 

crime_plot <- crime_master %>%
  mutate(MURDER = as.numeric(MURDER),
         ROADACCIDENT = as.numeric(ROADACCIDENT),
         ROADVICTIM = as.numeric(ROADVICTIM),
         CRIMESHARE = as.numeric(CRIMESHARE),
         RAPE = as.numeric(RAPE),
         ROBBERY = as.numeric(ROBBERY),
         HOOLIGANISM = as.numeric(HOOLIGANISM),
         ECONCRIME = as.numeric(ECONCRIME),
         JUVENILECRIME = as.numeric(JUVENILECRIME))

# one for the map

crime_map <- crime_master %>%
  mutate(YEAR = as.character(YEAR))


# Prepare the shape file for the map 
# Read it in
# Project the shape file 

rf_map <- readOGR(dsn = "RUS_adm", layer = "RUS_adm1")
rf_map <- spTransform(rf_map, CRS("+init=epsg:4326"))

# Create a vector of nice labels for the user-selected variables for the plot and the table 

crime_options <- c("Road accidents" = "ROADACCIDENT", 
                   "Victims of road accidents" = "ROADVICTIM",
                   "Crime share" = "CRIMESHARE", 
                   "Murders" = "MURDER",
                   "Incidences of rape" = "RAPE",
                   "Robberies" = "ROBBERY",
                   "Incidences of hooliganism" = "HOOLIGANISM",
                   "White-collar crimes" = "ECONCRIME",
                   "Incidences of juvenile crime" = "JUVENILECRIME") 

# Create a vector of nice labels for the user-selected variables for the map

crime_options_map <- c("Road accidents" = "ROADACCIDENT", 
                   "Victims of road accidents" = "ROADVICTIM",
                   "Crime share" = "CRIMESHARE", 
                   "Murders" = "MURDER",
                   "Incidences of juvenile crime" = "JUVENILECRIME") 

# Create a vector of definitions for these variables

crime_definitions <- c("the number of road accidents per 100,000 persons" = "ROADACCIDENT", 
                   "the number of road accident victims per 100,000 persons" = "ROADVICTIM",
                   "the number of registered crimes per 100,000 persons" = "CRIMESHARE", 
                   "the number of murders and murder attempts" = "MURDER",
                   "the number of reported rapes" = "RAPE",
                   "the number of reported robberies" = "ROBBERY",
                   "the number of reported acts of hooliganism" = "HOOLIGANISM",
                   "the number of reported white-collar crimes" = "ECONCRIME",
                   "the number of reported juvenile crimes" = "JUVENILECRIME") 

# Define UI for application with a nice theme
# Use a navBar structure

ui <- navbarPage("Crime in the Russian Regions", theme = shinytheme("flatly"),
                
                 # ABOUT 
                 # First tabPanel gives about information and so the formatting is fluidRow
                 # Holds an an htmlOutput

                 tabPanel("About",
                          fluidRow(
                            column(12,
                                   wellPanel(
                                     htmlOutput("about"))))),
                 # TABLE
                 # Second tabPanel gives us a table that displays the data

                 tabPanel("View the data",
                          sidebarLayout(
                            sidebarPanel(                              
                              
                              # Allow users to select the year to be included in the table
                              
                              selectInput(inputId = "year_table", #internal label 
                                          label = "Year to display in table", #label that user sees
                                          choices = c(1990:2010), #vector of choices for user to pick from 
                                          selected = 2000),
                              
                              # Allow users to select the indicator to be displayed in the table
                              
                              selectInput(inputId = "table_indicator", # internal label 
                                          label = "Indicator* to display in the table", # label that user sees
                                          choices = crime_options, # vector of choices for user to pick from 
                                          selected = crime_options[3]),
                              htmlOutput("define_variables_table")),
                            
                            mainPanel(
                              p("Select the year and the indicator to view the data in the table."),
                              p("Click on the indicator column name to arrange by the indicator."),
                              br(),
                              DT::dataTableOutput("ranking")))),
                 
                 # PLOT
                 # Third tab panel holds the plot
                 # Uses a sidebarLayout 
                 
                 tabPanel("Plot the indicators",
                          sidebarLayout( 
                            sidebarPanel(
                              
                               # Allows the user to select the indicator to display 
                              
                              selectInput(inputId = "y", # internal label 
                                                      label = "Select an indicator* to display on the y-axis", # label that user sees
                                                      choices = crime_options, # vector of choices for user to pick from 
                                                      selected = crime_options[3]),
                              
                              
                              htmlOutput("define_variables_y"),  
                              br(),
                              
                              # Let users select the regions to plot
          
                              selectizeInput(inputId = "region", # internal label
                                                        label = "Select up to five regions", # label that user sees
                                                         choices = c(crime_plot$NAME), # choose from this list 
                                                         multiple = TRUE, # can choose multiple 
                                                         options = list(maxItems = 5), # can choose up to five
                                                         selected = "Moscow")), 
                            # Plot output
                            
                            mainPanel(
                              plotlyOutput("scatterplot")))),
                 

                  # MAP
                  # fourth tabPanel holds the map
                  # Uses a sidebarLayout 
                 
                  tabPanel("Map the indicators",
                           sidebarLayout( 
                             sidebarPanel( 
                               
                               # Allows the user to select the year to map
                               
                               selectInput(inputId = "year", #internal label 
                                           label = "Year to map", #label that user sees
                                           choices = c(crime_map$YEAR), #vector of choices for user to pick from 
                                           selected = "1990"),
                               
                               # Allows the user to select the indicator to map
                               
                               selectInput(inputId = "map_var", # internal label 
                                           label = "Indicator* to map", # label that user sees
                                           choices = crime_options_map # vector of choices for user to pick from 
                                           ),
                               
                               htmlOutput("define_variables_map")),  
                            
                            # Holds the map object
                             
                             mainPanel(
                               p("Please select a year and an indicator. Then allow for up to a minute for the map to load."),
                               br(),
                               leafletOutput("map", width = "100%", height = "500px")))))

# Server

server <- function(input, output){
  
  # Reactive that subsets the regions to plot
  # by filtering the crime_plot data for the user inputed regions
  
  regions_subset <- reactive({ 
    req(input$region)
    filter(crime_plot, NAME %in% input$region)})
  
  # Reactive that filters the crime_map data for the user-selected year
  
  map_subset <- reactive({
    req(input$year, input$map_var)
    crime_map %>%
    filter(YEAR == input$year) %>%
      select(ID_1, NAME, input$map_var) %>%
      rename(selected_var = input$map_var)
    
    
  })
  
  # Scatterplot output 
  # Wrap ggplot in ggplotly wrapper
  
  output$scatterplot <- renderPlotly({
    ggplotly(ggplot(data = regions_subset(), aes_string(x = "YEAR", y = input$y, color = "NAME")) + #plot year on x and value on y
               geom_point(alpha = 0.8) + #color by region
               labs(x = "Year", 
                    y = names(crime_options[which(crime_options == input$y)]),
                    title = paste0(names(crime_options[which(crime_options == input$y)]), " from 1990 to 2010 ")) +
               theme(text = element_text(size = 10), 
                     axis.text.y = element_text(angle = 90, hjust = 1)) +
               scale_color_discrete(name = "Regions")) %>% 
               config(displayModeBar = FALSE) })
  
  # Data table output
  
  output$ranking <- DT::renderDataTable({
  table_data  <- crime_plot %>%
      filter(YEAR == input$year_table) %>%
      select(NAME, input$table_indicator)
  
  DT::datatable(table_data, 
                rownames = FALSE,
                colnames = c("Administrative unit", names(crime_options[which(crime_options == input$table_indicator)])))
  })
   
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

   rf_map <- merge(rf_map, map_subset(), by = "ID_1", duplicateGeoms = TRUE)
    coloring <- colorNumeric(palette = "Blues",
                             domain = rf_map@data$selected_var)
  rf_map %>%
      leaflet(options = leafletOptions(dragging = TRUE)) %>%
      addProviderTiles(provider = "CartoDB") %>%
      fitBounds(lng1 = 40, lat1 = 30, lng2 = 150, lat2 = 100) %>%
      setMaxBounds(lng1 = 20, lat1 = 30, lng2 = 170, lat2 = 100) %>%
      addPolygons(weight = 1,
                  label = ~paste0(NAME, ", ",
                                  names(crime_options_map[which(crime_options_map == input$map_var)]), ": ",  selected_var),
                  color = ~coloring(selected_var)) %>%

      # Add a legend in the bottom

      addLegend("bottomright",
                pal = coloring,
                values = ~selected_var,
                title = names(crime_options_map[which(crime_options_map == input$map_var)]),
                opacity = 1)
  })
  
  # Create a variable descriptor for the plot
  
  output$define_variables_y <- renderUI({
  HTML(paste("* Where ",
             str_to_lower(names(crime_options[which(crime_options == input$y)])),
             " is ",
            names(crime_definitions[which(crime_definitions == input$y)])))  
    
  })
  
  # Create a variable descriptor for the table
  
  output$define_variables_table <- renderUI({
    HTML(paste("* Where ",
               str_to_lower(names(crime_options[which(crime_options == input$table_indicator)])),
               " is ",
               names(crime_definitions[which(crime_definitions == input$table_indicator)])))  
    
  })
  
  # Create a variable descriptor for the map
  
  output$define_variables_map <- renderUI({
    HTML(paste("* Where ", 
               str_to_lower(names(crime_options_map[which(crime_options_map == input$map_var)])),
               " is ",
               names(crime_definitions [which(crime_definitions == input$map_var)]))) 
    
  })
  
  # Create the text element to explain the app
  
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