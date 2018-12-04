# Load libraries with tidyverse last to make sure
# that none of its functionality gets overridden 

library(shiny)
library(stringr)
library(rsconnect)
library(leaflet)
library(rgdal)
library(shinythemes)
library(plotly)
library(DT)
library(tidyverse)


# I prepared my data in the data_preparation.R file and saved the rds to the app folder
# Here I read it in. 
# I rename the variables so that they'll appear nicely in the tooltip function of Plotly. 
# This function, which allows the user to hover over a point and get more info, does not allow for custom names
# using a predefined vector. So to get nice labels, you need to have nice variable names already.

crime_master <- read_rds("r_4_tidy.rds") %>%
  rename(Murder = MURDER, Accidents = ROADACCIDENT, Victims = ROADVICTIM, Share = CRIMESHARE, Rape = RAPE,
         Robbery = ROBBERY, Hooliganism = HOOLIGANISM, Whitecollar = ECONCRIME, Juvenile = JUVENILECRIME) %>%
  
  # I choose to round these two variables so that their quantities will display more nicely in the map, plot, and data table
  
  mutate(Accidents = round(Accidents, digits = 2)) %>%
  mutate(Victims = round(Victims, digits = 2)) 

# I chose to prepare two data sets 
# - one for the plot

crime_plot <- crime_master 

# And one for the map. 

crime_map <- crime_master 

# Read in the shape file for the map from the files stored in the app folder
# Project the shape file in the standard WGS 84 projection. 
# I chose this projection because itis used by most GPS navigation tools and the tech, on which Leaflet is based

rf_map <- readOGR(dsn = "RUS_adm", layer = "RUS_adm1")
rf_map <- spTransform(rf_map, CRS("+init=epsg:4326"))

# Create a vector of nice labels for the user-selected variables for the plot and the table 

crime_options <- c("Road accidents" = "Accidents", 
                   "Victims of road accidents" = "Victims",
                   "Crime share" = "Share", 
                   "Murders" = "Murder",
                   "Incidences of rape" = "Rape",
                   "Robberies" = "Robbery",
                   "Incidences of hooliganism" = "Hooliganism",
                   "White-collar crimes" = "Whitecollar",
                   "Incidences of juvenile crime" = "Juvenile") 

# Create a vector of nice labels for the user-selected variables for the map
# This vector only includes variables with data for every year.
# Including variables that do not have data for every year will throw an error on the map.

crime_options_map <- c("Road accidents" = "Accidents", 
                   "Victims of road accidents" = "Victims",
                   "Crime share" = "Share", 
                   "Murders" = "Murder",
                   "Incidences of juvenile crime" = "Juvenile") 

# Create a vector of definitions for these variables

crime_definitions <- c("the number of road accidents per 100,000 persons" = "Accidents", 
                   "the number of road accident victims per 100,000 persons" = "Victims",
                   "the number of registered crimes per 100,000 persons" = "Share", 
                   "the number of murders and murder attempts" = "Murder",
                   "the number of reported rapes" = "Rape",
                   "the number of reported robberies" = "Robbery",
                   "the number of reported acts of hooliganism" = "Hooliganism",
                   "the number of reported white-collar crimes" = "Whitecollar",
                   "the number of reported juvenile crimes" = "Juvenile") 

# Define UI for application with a nice theme
# Use a navBar structure because the three visual compoenents each require different inputs 

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
                              
                              selectInput(inputId = "year_table",
                                          label = "Year to display in table", 
                                          choices = c(1990:2010), 
                                          selected = 2000), 
                              
                              # Allow users to select the indicator to be displayed in the table
                              
                              selectInput(inputId = "table_indicator", 
                                          label = "Indicator* to display in the table", 
                                          choices = crime_options, 
                                          selected = crime_options[3]),
                              
                              # An HTML text output that reactively defines the selected variables
                              
                              htmlOutput("define_variables_table")),
                            
                            # The main panel holds the data table and instructions for how to 
                            # manipulate the data in the table
                            
                            mainPanel(
                              h4("Instructions"),
                              p("Select the year and the indicator to view the data in the table.", br(),
                                "Click on the indicator column name to arrange by the indicator. Use the search bar to search for a particular region"),
                              br(),
                              DT::dataTableOutput("ranking")))),
                 
                 # PLOT
                 # Third tab panel holds the plot
                 # Uses a sidebarLayout 
                 
                 tabPanel("Plot the indicators",
                          sidebarLayout( 
                            sidebarPanel(
                              
                               # Allows the user to select the indicator to display 
                              
                              selectInput(inputId = "y", 
                                                      label = "Select an indicator* to display on the y-axis", 
                                                      choices = crime_options, 
                                                      selected = crime_options[3]),
                              
                              # An HTML text output that reactively defines the selected variables
                              
                              htmlOutput("define_variables_y"),  
                              br(),
                              
                              # Let users select the regions to plot
                              # Use the options function to allow users to select a max of five regions
          
                              selectizeInput(inputId = "region", 
                                                        label = "Select up to five regions", 
                                                         choices = c(crime_plot$NAME), 
                                                         multiple = TRUE,  
                                                         options = list(maxItems = 5), 
                                                         selected = "Moscow")), 
                            # Plot output
                            
                            mainPanel(
                              h4("Instructions"),
                              p("Select up to five regions and the indicator to view the data in the plot.", br(), "Hover over each point for more information."), 
                              plotlyOutput("scatterplot")))),
                 

                  # MAP
                  # fourth tabPanel holds the map
                  # Uses a sidebarLayout 
                 
                  tabPanel("Map the indicators",
                           sidebarLayout( 
                             sidebarPanel( 
                               
                               # Allows the user to select the year to map
                               
                               selectInput(inputId = "year", 
                                           label = "Year to map", 
                                           choices = c(1990:2010), 
                                           selected = 2000),
                               
                               # Allows the user to select the indicator to map
                               
                               selectInput(inputId = "map_var", 
                                           label = "Indicator* to map", 
                                           choices = crime_options_map,
                                           selected = crime_options_map[1] 
                                           ),
                               
                               
                               # An HTML text output that reactively defines the selected variables
                               
                               htmlOutput("define_variables_map")),  
                            
                            # Holds the map object and instructions.
                             
                             mainPanel(
                               h4("Instructions"),
                               p("Please select a year and an indicator. Then allow for up to a minute for the map to load.", 
                                 br(),"Click and drag the map to pan to other areas. Hover over an individual region to display the data." ),
                               br(),
                               leafletOutput("map", width = "100%", height = "500px")))))

# Server

server <- function(input, output){
  
  # Reactive that subsets the regions to plot in the scatter plot
  # by filtering the crime_plot data for the user inputed regions.
  # Including all the regions in the plot would cause massive overplotting. That's why
  # filtering is important. 
  # Rename two the variables to display more nicely in the plotly output.
  
  regions_subset <- reactive({ 
    req(input$region) 
    crime_plot %>%
      filter(NAME %in% input$region) %>%
      rename(Region = NAME, Year = YEAR) })
  
  # Reactive that filters the crime_map data for the user-selected year
  # and for the user-selected variable. Selecting only the necessary 
  # variables and data makes the map render more quickly.
  # Rename the input$map_var to a single variable name, which makes it easier
  # to work with in conjunction with the shapefile.
  
  map_subset <- reactive({
    req(input$year, input$map_var)
    crime_map %>%
    filter(YEAR == input$year) %>%
      select(ID_1, NAME, input$map_var) %>%
      rename(selected_var = input$map_var) 
    
    
  })
  
  # Scatterplot output 
  # Wrap ggplot in ggplotly wrapper.
  # Use the filtered data to only include user-selected regions.
  # Plot the year on the x-axis and the user-selected variable on the y-axis.
  # Color by region.
  # Use my vector of labels to generate a reactive title and axis labels.
  # Rename the legend and remove the Plotly display bar, which is an eyesore and not useful. 
  
  output$scatterplot <- renderPlotly({
    ggplotly(ggplot(data = regions_subset(), aes_string(x = "Year", y = input$y, color = "Region")) + 
               geom_point(alpha = 0.8) + 
               labs(x = "Year", 
                    y = names(crime_options[which(crime_options == input$y)]),
                    title = paste0(names(crime_options[which(crime_options == input$y)]), " from 1990 to 2010 ")) +
               theme(text = element_text(size = 10), 
                     axis.text.y = element_text(angle = 90, hjust = 1)) +
               scale_color_discrete(name = "Regions")) %>% 
               config(displayModeBar = FALSE) })
  
  # Data table output.
  # Save the data, filtered by the user-selected year and selecting for just the name and indicator
  # to a new dataframe.
  # Display this data frame in the app.
  # Do not display rownames. Set custom and reactive column names.
  
  output$ranking <- DT::renderDataTable({
  table_data  <- crime_plot %>%
      filter(YEAR == input$year_table) %>%
      select(NAME, input$table_indicator)
  
  DT::datatable(table_data, 
                rownames = FALSE,
                colnames = c("Administrative unit", names(crime_options[which(crime_options == input$table_indicator)])))
  })
   
  # Map output
  # See code for comments

  output$map <- renderLeaflet({

    # Merge the shapefile with the sub_setted map data
    
   rf_map <- merge(rf_map, map_subset(), by = "ID_1", duplicateGeoms = TRUE)
   
   # Select the user-inputted variable to map (choropleth) in the domain 
   # Color by the selected indicator
   
    coloring <- colorNumeric(palette = "Blues",
                             domain = rf_map@data$selected_var)
  
  # Set the options for the leaflet viewer
  # Allow the user to drag
  # Add a basemap to help users situate themselves 
  # Set the default view and the max bounds  
  # Add the polygon shapefile 
  # Add custom and reactive pop-up labels and color by the user-selected indicator  
    
  rf_map %>%
      leaflet(options = leafletOptions(dragging = TRUE)) %>%
      addProviderTiles(provider = "CartoDB") %>%
      setView(lat = 60, lng = 37, zoom = 3) %>%
      setMaxBounds(lng1 = 15, lat1 = 25, lng2 = 180, lat2 = 90) %>%
      addPolygons(weight = 1,
                  label = ~paste0(NAME, ", ",
                                  names(crime_options_map[which(crime_options_map == input$map_var)]), ": ",  selected_var),
                  color = ~coloring(selected_var)) %>%

      # Add a legend in the bottom
      # Which uses the coloring set above
      # And the user-selected variable
      # And has a reactive title

      addLegend("bottomright",
                pal = coloring,
                values = ~selected_var,
                na.label = "No data",
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
      p("This application allows users to visualize crime data for 82 of the Russian Federation's federal subjects (administrative units) from 1990 through 2010."),
      p("Click through the above tabs to view the data interactively in a table, scatterplot, and map. This data exploration highlights the geographic disparity across Russia's regions in terms of social indicators.  
        The data also helps communicate changes in crime trends from the tumultuous 1990s period through President Vladimir Putin's first two terms and the first half of Dmitry Medvedev's presidency."),
      h3("Sources"), 
      p("Inter-university Consortium for Political and Social Research (ICPSR):", 
        br(),   
        "ICPSR 35355 Aggregate Data, Regions of Russia (RoR), 1990 - 2010, created by Irina Mirkina.",
         tags$a(href = "https://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/35355", 
                br(),  "Click here to learn more about this data.")),
      p("Diva GIS:", 
        br(), 
        "Polygon shapefile for Russia's first-level administrative boundaries (RUS_adm1.shp and associated files)", 
        tags$a(href = "http://www.diva-gis.org/", 
               br(),"Click here to learn more about this data"))))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)