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
# using a predefined vector. So to get nice labels, you need to have "nice" variable names already.

crime_master <- read_rds("r_4_tidy.rds") %>%
  rename(Murder = MURDER, Accidents = ROADACCIDENT, Victims = ROADVICTIM, Share = CRIMESHARE, Rape = RAPE,
         Robbery = ROBBERY, Whitecollar = ECONCRIME, Juvenile = JUVENILECRIME) %>%
  
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
# I chose this projection because it is used by most GPS navigation tools and the tech, on which Leaflet is based

rf_map <- readOGR(dsn = "RUS_adm", layer = "RUS_adm1")
rf_map <- spTransform(rf_map, CRS("+init=epsg:4326"))

# Create a vector of nice labels for the user-selected variables for the plots and the table 

crime_options <- c("Road accidents" = "Accidents", 
                   "Victims of road accidents" = "Victims",
                   "Crime share" = "Share", 
                   "Murders" = "Murder",
                   "Incidences of rape" = "Rape",
                   "Robberies" = "Robbery",
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

# Create a vector of definitions for these variables.
# The variable's definition will be displayed when the user selects a given variable.

crime_definitions <- c("the number of road accidents per 100,000 persons" = "Accidents", 
                   "the number of road accident victims per 100,000 persons" = "Victims",
                   "the number of registered crimes per 100,000 persons" = "Share", 
                   "the number of murders and murder attempts" = "Murder",
                   "the number of reported rapes" = "Rape",
                   "the number of reported robberies" = "Robbery",
                   "the number of reported white-collar crimes" = "Whitecollar",
                   "the number of reported juvenile crimes" = "Juvenile") 

# Define UI for application with a nice theme
# Use a navBar structure because the four visual compoenents each require different inputs 

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
                            # I chose to put the instructions in a wellPanel to make them distinct from the visualization
                            # Also reactively warn the user when they have selected a variable year combination for which
                            # there is no data 
                            
                            mainPanel(
                              wellPanel(h4("Instructions"),
                              p("This tool helps to answer questions such as: in 1990, which region had the highest crime share?
                                Simply select the year (for example, 1990) and the indicator (for example, crime share) and then click on the indicator column name to arrange in descending order by that indicator. 
                                Use the search bar to search for a particular region."),
                              htmlOutput("NoData"),
                              br()),
                              DT::dataTableOutput("ranking")))),
                 
                 # PLOT
                 # Third tab panel holds the plots
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
                              
                              # Allow users to select to view the data as a curve, but warn them that
                              # this will interpolate missing data, so it's not 100% accurate
                              
                              checkboxInput("line", label = "View top plot as a curve (missing data is interpolated)"),
                              
                              
                              # Let users select the regions to plot
                              # Use the options function to allow users to select a max of five regions
          
                              selectizeInput(inputId = "region", 
                                                        label = "Select up to five regions", 
                                                         choices = c(crime_plot$NAME), 
                                                         multiple = TRUE,  
                                                         options = list(maxItems = 5), 
                                                         selected = "Moscow")), 
                            # Plot outputs
                            # I chose to put the instructions in a wellPanel to make them distinct from the visualization
                            
                            mainPanel(
                             wellPanel(
                               h4("Instructions"),
                               h5(strong("Top plot")),
                              p("The top plot displays a given indicator broken down by region. 
                                Select up to five regions and an indicator to view the data in the plot. 
                                Hover over each point for more information. 
                                Tick the box to connect the points with a curve. 
                                Please note that the hover function has been disabled for the curve setting.", 
                                br()), 
                             h5(strong("Bottom plot")),
                             p("The bottom plot displays the median of the selected indicator across regions from 1990 to 2010. 
                               It serves as a tool for comparing regional data to the national trend.")),
                             
                             # Put the scatterplot first, because it's the focus of this panel
                             
                             plotlyOutput("scatterplot"),
                             br(),
                             br(),
                             
                             # Put the comparison plot (which just plots the national median for a given indicator) second
                             # as it's just meant to be used for comparison 
                             
                             plotOutput("median")))),

                 # REGRESSION
                 # Fouth tab holds a model plot
                 
                 tabPanel("Model the indicators",
                          sidebarLayout(
                            sidebarPanel( 
                              
                              # Let the user select the y variable to regress
                              
                              selectInput(inputId = "y_reg", 
                                                      label = "Select an indicator to plot on the y-axis", 
                                                      choices = crime_options, 
                                                      selected = crime_options[3]),
                              
                              htmlOutput("define_variables_y_reg"),  
                              br(),
                              
                              # Let the user select the x variable to regress
                              
                              selectInput(inputId = "x_reg", 
                                          label = "Select an indicator to plot on the x-axis", 
                                          choices = crime_options, 
                                          selected = crime_options[3]),
                              
                              htmlOutput("define_variables_x_reg"),  
                              br()),
                        
                            # Model output
                            
                            mainPanel(
                              wellPanel( h4("Instructions"),
                              p("Select a y variable to regress against an x variable. Then check out the model summary below. The model uses a simple linear regression.")),
                              plotOutput("regression_plot"),
                              br(),
                              htmlOutput("stats"))
                          )),
                 
                 
                 # MAP
                  # fifth tabPanel holds the map
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
                             wellPanel(  h4("Instructions"),
                               p("First let the map load. This may take up to one minute. 
                                 Be patient. It's worth it. You're loading 11 time zones worth of data here! 
                                 Then select a year and an indicator and the map will more quickly reload.", 
                                 br(),"Click and drag the map to pan to other areas. Hover over an individual region to display the data. Please note that regions for which there is no data (i.e. NA) are displayed in gray." )),
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
  # variables and data AND THEN joining the data to the shapefile makes the map render more quickly.
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
  # The ifelse statement allows users to check a box to plot the data as a curve instead of points.
  # The default setting is points, because it is more accurate (AKA ggplot is not interpolating missing data)
  
  output$scatterplot <- renderPlotly({
    
    if(input$line == FALSE) {
    ggplotly(ggplot(data = regions_subset(), aes_string(x = "Year", y = input$y, color = "Region")) + 
               geom_point(alpha = 0.8) + 
               labs(x = "Year", 
                    y = names(crime_options[which(crime_options == input$y)]),
                    title = paste0(names(crime_options[which(crime_options == input$y)]), " (1990 - 2010)")) +
               theme(text = element_text(size = 10), 
                     axis.text.y = element_text(angle = 90, hjust = 1)) +
               scale_color_discrete(name = "Regions")) %>% 
               config(displayModeBar = FALSE)  }
    else {
      
      # I disabled tooltip for the curve because it is messy (displaying interpolated data for the year 2000.5 for instance)
      
      ggplotly(tooltip = F, ggplot(data = regions_subset(), aes_string(x = "Year", y = input$y, color = "Region")) + 
        geom_smooth(se = F) + 
        labs(x = "Year", 
             y = names(crime_options[which(crime_options == input$y)]),
             title = paste0(names(crime_options[which(crime_options == input$y)]), " (1990 - 2010)")) +
        theme(text = element_text(size = 10), 
              axis.text.y = element_text(angle = 90, hjust = 1)) +
        scale_color_discrete(name = "Regions")) %>% 
        config(displayModeBar = FALSE) 
      
    } 
    })
  
  
  # Plot that displays median of the indicator for comparison with individual regions
  # I chose to use the median rather than the average because the median is insulated from extreme values
  
  output$median <- renderPlot({
       ggplot(data = crime_plot, aes_string(x = "YEAR", y = input$y)) +
       geom_smooth(se = F, formula = median(input$y)) +
      labs(x = "Year", 
           y = names(crime_options[which(crime_options == input$y)]),
           title = paste0("Median ", str_to_lower(names(crime_options[which(crime_options == input$y)])), " across Russia (1990 to 2010)"))
  })
  
  # Data table output
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
  
  
  # If the data selects an indicator and year for which there is no data
  # they are reactively warned
  
  output$NoData <- renderPrint({
    if (input$year_table %in% c(1990:1999) & 
        input$table_indicator %in% c(crime_options[6], crime_options[7], crime_options[5])){
      HTML(paste(strong("We do not have data for this indicator before 2000. Please select another indicator or later year.")))
    }else {
      HTML("")
    }
  })
   
  # Map output
  # See code for in-line comments

  output$map <- renderLeaflet({

    # Merge the shapefile with the subsetted map data
    # Only merge after the filtering the data
    # Merging before means that the map never loads 
    # There is too much data in that case
    
   rf_map <- merge(rf_map, map_subset(), by = "ID_1", duplicateGeoms = TRUE)
   
   # Select the user-inputted variable to map (choropleth) in the domain 
   # Color by the selected indicator using a numeric symbology 
   
    coloring <- colorNumeric(palette = "YlGnBu",
                             domain = rf_map@data$selected_var)
  
  # Set the options for the leaflet viewer
  # Allow the user to drag
  # Add a basemap to help users situate themselves 
  # Set the default view and the max bounds, so users can't drag themselves to irrelevant parts of the map
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

      # Add a legend in the bottom to maximize view of the map
      # Which uses the coloring set above
      # And the user-selected variable
      # And has a reactive title
      # I cannot move or change the NA display in the legend 
      # I know it's smushed up against the other data, but Leaflet doesn't let you alter it
      # or remove it. Even setting it to F or NA does not help.

      addLegend("bottomright",
                pal = coloring,
                values = ~selected_var,
                na.label = "NA",
                title = names(crime_options_map[which(crime_options_map == input$map_var)]),
                opacity = 1)
  })
  
  # Correlation plot output
  # Simple interactive ggplot with reactive labels and titles 
  # Using the lm model because it's a simple way for assessing the relationship (positive or negative)
  # between two indicators
  # Since it's an exploratory data analysis tool, there isn't a need to do more than that here 
  
  output$regression_plot <- renderPlot({
    ggplot(data = crime_plot, aes_string(x = input$x_reg, y = input$y_reg)) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(x = names(crime_options[which(crime_options == input$x_reg)]), 
           y = names(crime_options[which(crime_options == input$y_reg)]),
           title = paste("Regressing", str_to_lower(names(crime_options[which(crime_options == input$y_reg)])), 
                         "against", str_to_lower(names(crime_options[which(crime_options == input$x_reg)]))))
    
  })
  
  # Reactive text output for strength of correlation
  
  weak_strong <- reactive({
    
    x_var_reg <- input$x_reg
    y_var_reg <- input$y_reg
    
    # Set cutoffs based on the data and convention as specified by data camp
    
   coeff <- cor(x = crime_plot[[x_var_reg]], y = crime_plot[[y_var_reg]], use = "pairwise")
    if (round(coeff, digits = 2) > .4 ) {
      weak_strong <- "positive"
    } else if (round(coeff, digits = 2) > 0 ) {
      weak_strong <- "weak positive"
    } else if (round(coeff, digits = 2) == 0) {
      weak_strong <- "neutral"
    } else {
      weak_strong <- "weak negative"
    }
    
  })
  
  # Reactive text output for signif of correlation
  
  is_sig  <- reactive({
    my_formula <- paste0(input$y_reg, "  ~ ", input$x_reg)    
    m1 <- summary(lm(my_formula, data = crime_plot))
    fstat <- m1$fstatistic 
    pval <- pf(fstat[1], fstat[2], fstat[3], lower.tail = F)
    
    # Set a signifance level with respect to .05 as specified by data camp 
    
    if (pval < .05) { 
      is_sig <- "is"
    } else {
      is_sig <- "is not"
    }})
  
  # Define the summary text for the regression plot
  # Define the summary text output
  # Create a reactive text ouput in which the r squared, p value, and significance explanation change 
  # in response to the user selected variable 
  # Because this is an exploratory data tool, I choose this simply display rather than printing a regression table
  # I hope that this will allow users to understand the simple relationship between the indicators and
  # that this display choice will avoid overwhelming the user 
  # I use bold text to highlight the values within the text block to draw user attention to them 
  # If the user tries to regress the same variable against itself, the model summary reacts,
  # telling the user to choose two distinct variables
  
  output$stats <- renderPrint({
    
    if(input$y_reg == input$x_reg) {
      HTML(paste(h4("Model summary"), "Please select two distinct variables.")) 
      
    } else {
    
    my_formula <- paste0(input$y_reg, "  ~ ", input$x_reg)
    m0 <- (lm(my_formula, data = crime_plot))
    m1 <- summary(m0)
    fstat <- m1$fstatistic 
    pval <- pf(fstat[1], fstat[2], fstat[3], lower.tail = F)
    
    x_var_reg <- input$x_reg
    y_var_reg <- input$y_reg
    coeff <- cor(x = crime_plot[[x_var_reg]], y = crime_plot[[y_var_reg]], use = "pairwise")
    
    HTML(paste(h4("Model summary"), tags$ul(
      tags$li("The correlation coefficient is approximately ", strong(round(coeff, digits = 2)), 
              ". This is the slope of the regression line and means that the variables have a ", weak_strong(), " relationship."),
      tags$li("The multiple r-squared is approximately ", strong(round(m1$r.squared, digits = 2)), 
              ". This means that roughly ", round((m1$r.squared)*100, digits = 2), "percent of the variation is explained by this variable." ),
      tags$li("The p-value is approximately ", strong(round(pval, digits = 2)), ". This means that the result ", is_sig(), " statistically significant
              with respect to a significance level of 0.05.")))) }
    
    
  })
  
  # Create a indicator descriptor for the plots tab
  # This simply defines the selected indicator
  
  output$define_variables_y <- renderUI({
  HTML(paste("* Where ",
             str_to_lower(names(crime_options[which(crime_options == input$y)])),
             " is ",
            names(crime_definitions[which(crime_definitions == input$y)])))  
    
  })
  
  # Create a y variable descriptor for the regression plot
  
  output$define_variables_y_reg <- renderUI({
    HTML(paste("* Where ",
               str_to_lower(names(crime_options[which(crime_options == input$y_reg)])),
               " is ",
               names(crime_definitions[which(crime_definitions == input$y_reg)])))  
    
  })
  
  # Create a x variable descriptor for the regression plot
  
  output$define_variables_x_reg <- renderUI({
    HTML(paste("* Where ",
               str_to_lower(names(crime_options[which(crime_options == input$x_reg)])),
               " is ",
               names(crime_definitions[which(crime_definitions == input$x_reg)])))  
    
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
      p("Click through the above tabs to view the data interactively in a table, scatterplot, linear regression model, and map. This data exploration highlights the geographic disparity across Russia's regions in terms of social indicators.  
        The data also helps communicate changes in crime trends from the tumultuous 1990s period through President Vladimir Putin's first two terms (2000-2008) and the first half of Dmitry Medvedev's presidency (2008-2010)."),
      h3("So what?"),
      p(em("But Miranda - I don't care about Russia. Isn't that just a country full of vokda-drinkers, snow, and hackers?")),
      p("While we will have to agree to disagree on that point (By the way, Poles claim vodka is a ", 
        tags$a(href = "https://www.rferl.org/a/who-invented-vodka/28946217.html", "Polish creation"), "), I still think you'll find this app interesting. 
        One of the dominant narratives that analysts cite to explain Vladimir Putin's genuine popularity focuses on crime reduction. 
        Put simply, after the collapse of the Soviet Union in 1991, the newly democratizing state was weak and crime rates soared. Putin came to power promising to reassert the state's control 
        and end the turmoil of the chaotic and crime-ridden 90s. The majority of Russians believe he did just that. 
        But does the data hold up? Did crime increase in the 90s and drop off over Putin's first two terms (2000 - 2008)? 
        Using these tools to explore the data, you might find that the answer challenges the widely accepted story."),
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
               br(),"Click here to learn more about this data")), 
        h3("Спасибо большое"),
        p("I'd like to thank David Kane, preceptor for the GOV 1005 class, TFs Nick Short and Albert Rivero, and my classmates for all their feedback. 
        Their suggestions and troubleshooting made this app a reality.")))
  })
  
}

# Run the application 

shinyApp(ui = ui, server = server)