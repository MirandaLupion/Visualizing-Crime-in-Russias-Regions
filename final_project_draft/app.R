library(shiny)
library(tidyverse)
library(stringr)
load("Regions4.rda")
r_4 <- da35355.0004
r_4$NAME <- str_trim(r_4$NAME)



# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Crime in the Russian Regions, 1990 - 2010"),
   
   sidebarLayout( 
     sidebarPanel( 
       selectInput(inputId = "y", #internal label 
                   label = "Y-axis:", #label that user sees
                   choices = c("Road accidents" = "ROADACCIDENT", "Crime share" = "CRIMESHARE", "Murders" = "MURDER"), #vector of choices for user to pick from 
                  selected = "ROADACCIDENT"),
       
      selectizeInput(inputId = "region",
                          label = "Select regions", 
                          choices = c(r_4$NAME),
                          multiple = TRUE,
                          options = list(maxItems = 5))),
     mainPanel(
       plotOutput(outputId = "scatterplot"))))

# Server
server <- function(input, output){
  regions_subset <- reactive({
    req(input$region)
    filter(r_4, NAME %in% input$region)
  })
  output$scatterplot <- renderPlot({
    ggplot(data = regions_subset(), aes_string(x = regions_subset()$YEAR, y = input$y)) + 
      geom_point(aes(color = regions_subset()$NAME)) +
      labs(x = "Year", y = input$y)
  })}

# Run the application 
shinyApp(ui = ui, server = server)