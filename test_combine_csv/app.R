#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(leaflet)
library(RColorBrewer)
countries_data <- read_csv("../countries.csv")


ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                selectInput("colors", "Color Scheme",
                            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                ),
                checkboxInput("legend", "Show legend", TRUE),
                
                selectInput(inputId = "gender_variables",
                            label = "Choose the variable:",
                            choices = c("Adult Education", 
                                        "Enrolment Rate by Age", 
                                        "Distribution of teachers by gender", 
                                        "Graduation Rates",
                                        "Transition from school",
                                        "Graduation Field"))
  )
)

completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

server <- function(input, output, session) {
  
  adult_education <- read_csv("../education/adult_education.csv")
  enrollment_by_age <- read_csv("../education/enrollment_by_age.csv")
  enrollment_by_age <- completeFun(enrollment_by_age, "Value")
  enrollment_by_age <- enrollment_by_age %>% filter(Value > 0)
  distribution_teachers <- completeFun(read_csv("../education/distribution_teachers.csv"), "Value")
  distribution_teachers <- distribution_teachers %>% filter(Value > 0)
  graduation_rates <- completeFun(read_csv("../education/graduation_rates.csv"), "Value")
  graduation_rates <- graduation_rates %>% filter(Value > 0)
  transition <- completeFun(read_csv("../education/transition.csv"), "Value")
  transition <- transition %>% filter(Value > 0)
  graduation_field <- completeFun(read_csv("../education/graduation_field.csv"), "Value")
  graduation_field <- graduation_field %>% filter(Value > 0)
  
  variableInput <- reactive({
    switch(input$gender_variables,
           "Enrolment Rate by Age" = merge(enrollment_by_age, countries_data, by="Country"),
           "Adult Education" = merge(adult_education, countries_data, by="Country"),
           "Distribution of teachers by gender" = merge(distribution_teachers, countries_data, by="Country"),
           "Graduation Rates" = merge(graduation_rates, countries_data, by="Country"),
           "Transition from school" = merge(transition, countries_data, by="Country"),
           "Graduation Field" = merge(graduation_field, countries_data, by="Country"))
  })
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    grouped_data <- variableInput() %>%
      group_by(Country, Latitude, Longitude) %>%
      summarise(Value = mean(Value, na.rm = TRUE))
    
    return(grouped_data)
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$colors, filteredData()$Value)
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    #data_lat_long <- read_csv("../modelskiModern.csv")
    
    leaflet(filteredData()) %>% addTiles()
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    pal <- colorpal()

    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(radius = ~(Value/max(Value) * 100) ^ 3, weight = 1,
                 fillColor = ~pal(Value), fillOpacity = 0.5, popup = ~paste(Country, Value)
      )
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = filteredData())

    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~Value
      )
    }
  })
}

shinyApp(ui, server)

