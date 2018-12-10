#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(RColorBrewer)
library(shinydashboard)
library(tidyverse)

countries_data <- read_csv("countries.csv")

ui <- dashboardPage(
  dashboardHeader(title = "Gender Equality"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("Development", icon = icon("building"),
                         menuSubItem("Access to Public Space", tabName = "Access to Public Space"),
                         menuSubItem("Early Marriage", tabName = "Early Marriage"),
                         menuSubItem("Female Genital Mutilation", tabName = "Female Genital Mutilation"),
                         menuSubItem("Women Laws", tabName = "Women Laws"),
                         menuSubItem("Prevelance of Violence", tabName = "Prevelance of Violence"),
                         menuSubItem("Son Education Preference", tabName = "Son Education Preference"),
                         menuSubItem("Attitudes Towards Violence", tabName = "Attitudes Towards Violence")
                ),
                menuItem("Employment", icon = icon("dollar"),
                         menuSubItem("Female Share of Board Seats", tabName = "Female Share of Board Seats"),
                         menuSubItem("Time Spent in paid and unpaid work", tabName = "Time Spent in paid and unpaid work"),
                         menuSubItem("Employment Rate", tabName = "Employment Rate"),
                         menuSubItem("Gender Wage Gap", tabName = "Gender Wage Gap")),
                menuItem("Education", icon = icon("book"),
                         menuSubItem("Adult Education", tabName = "Adult Education"),
                         menuSubItem("Enrolment Rate by Age", tabName = "Enrolment Rate by Age"),
                         menuSubItem("Distribution of teachers by gender", tabName = "Distribution of teachers by gender"),
                         menuSubItem("Graduation Rates", tabName = "Graduation Rates"),
                         menuSubItem("Transition from school", tabName = "Transition from school"),
                         menuSubItem("Graduation Field", tabName = "Graduation Field")),
                menuItem("Government", icon = icon("balance-scale"),
                         menuSubItem("Women in Parliament", tabName = "Women in Parliament"),
                         menuSubItem("Women in Court Instance", tabName = "Women in Court Instance"),
                         menuSubItem("Women in Central Government", tabName = "Women in Central Government"),
                         menuSubItem("Women Ministers", tabName = "Women Ministers"),
                         menuSubItem("Women Judges", tabName = "Women Judges"))),
    textOutput("res")
  ),
  dashboardBody(
    tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
    checkboxInput("legend", "Show legend", TRUE),
    leafletOutput("map", width = "100%", height = "100%")
  )
)

completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

server <- function(input, output) {
 
  adult_education <- read_csv("education/adult_education.csv")
  enrollment_by_age <- read_csv("education/enrollment_by_age.csv")
  enrollment_by_age <- completeFun(enrollment_by_age, "Value")
  enrollment_by_age <- enrollment_by_age %>% filter(Value > 0)
  distribution_teachers <- completeFun(read_csv("education/distribution_teachers.csv"), "Value")
  distribution_teachers <- distribution_teachers %>% filter(Value > 0)
  graduation_rates <- completeFun(read_csv("education/graduation_rates.csv"), "Value")
  graduation_rates <- graduation_rates %>% filter(Value > 0)
  transition <- completeFun(read_csv("education/transition.csv"), "Value")
  transition <- transition %>% filter(Value > 0)
  graduation_field <- completeFun(read_csv("education/graduation_field.csv"), "Value")
  graduation_field <- graduation_field %>% filter(Value > 0)
  women_courts <- completeFun(read_csv("Government/women_courts.csv"), "Value")
  women_courts <- women_courts %>% filter(Value > 0)
  access_space <- completeFun(read_csv("Development/Access_To_Public_Space.csv"), "Value")
  access_space <- access_space %>% filter(Value > 0)
  merge_access_space <- merge(access_space, countries_data, by="Country")
  early_marriage <- completeFun(read_csv("Development/Early_Marriage.csv"), "Value")
  early_marriage$Value <- early_marriage$Value * 100
  early_marriage <- early_marriage %>% filter(Value > 0)
  female_genital_mutilation <- completeFun(read_csv("Development/Female_Genital_Mutilation.csv"), "Value")
  female_genital_mutilation$Value <- female_genital_mutilation$Value * 100
  female_genital_mutilation <- female_genital_mutilation %>% filter(Value > 0)
  women_laws <- completeFun(read_csv("Development/Laws.csv"), "Value")
  women_laws$Value <- women_laws$Value * 100
  women_laws <- women_laws %>% filter(Value > 0)
  prevelance_of_violence <- completeFun(read_csv("Development/Prevelance_of_Violence.csv"), "Value")
  prevelance_of_violence$Value <- prevelance_of_violence$Value * 100
  prevelance_of_violence <- prevelance_of_violence %>% filter(Value > 0)
  son_education_preference <- completeFun(read_csv("Development/Son_Education_Preference.csv"), "Value")
  son_education_preference$Value <- son_education_preference$Value * 100
  son_education_preference <- son_education_preference %>% filter(Value > 0)
  attitudes_towards_violence <- completeFun(read_csv("Development/Attitudes_Towards_Violence.csv"), "Value")
  attitudes_towards_violence$Value <- attitudes_towards_violence$Value * 100
  attitudes_towards_violence <- attitudes_towards_violence %>% filter(Value > 0)
  emp_seats <- completeFun(read_csv("Employment/GENDER_EMP_BOARD_SEATS.csv"), "Value")
  emp_seats <- emp_seats %>% filter(Value > 0)
  emp_paid_unpaid <- completeFun(read_csv("Employment/GENDER_EMP_PAID_UNPAID.csv"), "Value")
  emp_paid_unpaid <- emp_paid_unpaid %>% filter(Value > 0)
  emp_employed <- completeFun(read_csv("Employment/GENDER_EMP_UNEMP.csv"), "Value")
  emp_employed <- emp_employed %>% filter(Value > 0)
  emp_wage <- completeFun(read_csv("Employment/GENDER_EMP_WAGE_GAP.csv"), "Value")
  emp_wage <- emp_wage %>% filter(Value > 0)
  women_parliament <-  completeFun(read_csv("Government/women_parliament.csv"), "Value")
  women_court_instance <-  completeFun(read_csv("Government/women_court_instance.csv"), "Value")
  women_central_government <-  completeFun(read_csv("Government/women_central_government.csv"), "Value")
  women_ministers <-  completeFun(read_csv("Government/women_ministers.csv"), "Value")
  women_judges <-  completeFun(read_csv("Government/women_judges.csv"), "Value")
  
  variableInput <- reactive({
    #input$tabs
    if(input$tabs == "Access to Public Space"){
      merge(access_space, countries_data, by="Country")
    }
    else if(input$tabs == "Early Marriage"){
      return(merge(early_marriage, countries_data, by="Country"))
    }
    else if(input$tabs == "Female Genital Mutilation"){
      return(merge(female_genital_mutilation, countries_data, by="Country"))
    }
    else if(input$tabs == "Women Laws"){
      return(merge(women_laws, countries_data, by="Country"))
    }
    else if(input$tabs == "Prevelance of Violence"){
      return(merge(prevelance_of_violence, countries_data, by="Country"))
    }
    else if(input$tabs == "Son Education Preference"){
      return(merge(son_education_preference, countries_data, by="Country"))
    }
    else if(input$tabs == "Attitudes Towards Violence"){
      return(merge(attitudes_towards_violence, countries_data, by="Country"))
    }
    else if(input$tabs == "Female Share of Board Seats"){
      return(merge(emp_seats, countries_data, by="Country"))
    }
    else if(input$tabs == "Time Spent in paid and unpaid work"){
      return(merge(emp_paid_unpaid, countries_data, by="Country"))
    }
    else if(input$tabs == "Employment Rate"){
      return(merge(emp_employed, countries_data, by="Country"))
    }
    else if(input$tabs == "Gender Wage Gap"){
      return(merge(emp_wage, countries_data, by="Country"))
    }
    else if(input$tabs == "Adult Education"){
        return(merge(adult_education, countries_data, by="Country"))
    }
    else if(input$tabs == "Enrolment Rate by Age"){
      return(merge(enrollment_by_age, countries_data, by="Country"))
    }
    else if(input$tabs == "Transition from school"){
      return(merge(transition, countries_data, by="Country"))
    }
    else if(input$tabs == "Distribution of teachers by gender"){
      return(merge(distribution_teachers, countries_data, by="Country"))
    }
    else if(input$tabs == "Graduation Rates"){
      return(merge(graduation_rates, countries_data, by="Country"))
    }
    else if(input$tabs == "Graduation Field"){
      return(merge(graduation_field, countries_data, by="Country"))
    }
    else if(input$tabs == "Women in Parliament"){
      return(merge(women_parliament, countries_data, by="Country"))
    }
    else if(input$tabs == "Women in Court Instance"){
      return(merge(women_court_instance, countries_data, by="Country"))
    }
    else if(input$tabs == "Women in Central Government"){
      return(merge(women_central_government, countries_data, by="Country"))
    }
    else if(input$tabs == "Women Ministers"){
      return(merge(women_ministers, countries_data, by="Country"))
    }
    else if(input$tabs == "Women Judges"){
      return(merge(women_judges, countries_data, by="Country"))
    }
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
    colorNumeric('Reds', filteredData()$Value)
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    
    leaflet() %>%
      addTiles(options = providerTileOptions(noWrap = TRUE)) %>%
      setView(lng = -19.102055,
              lat = 52.776186,
              zoom = 2) %>%
      addProviderTiles("Esri.WorldImagery")
    
    
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    pal <- colorpal()

    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(radius = ~((Value/max(Value)) * 100) ^ 2.7, weight = 1,
                 fillColor = ~pal(Value), fillOpacity = 0.9, popup = ~paste(Country, round(Value,digits=2))
      )
  })
  # 
  # # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = filteredData())

    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (!is.null(input$legend)) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~Value
      )
    }
  })
  
  # output$res <- renderText({
  #   paste("You've selected:", variableInput())
  # })
}

shinyApp(ui, server)