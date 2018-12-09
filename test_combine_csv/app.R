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
library(tidyverse)
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
                                        "Graduation Field",
                                        "Women in Parliament",
                                        "Women in Court Instance",
                                        "Women in Central Government",
                                        "Women Ministers",
                                        "Women Judges",
                                        "Access to Public Space",
                                        "Early Marriage",
                                        "Female_Genital_Mutilation",
                                        "Women Laws",
                                        "Prevelance_of_Violence",
                                        "Son_Education_Preference",
                                        "Attitudes_Towards_Violence",
                                        "Female Share of Board Seats",
                                        "Time Spent in paid and unpaid work",
                                        "Employment Rate",
                                        "Gender Wage Gap"))
  ),
  fixedPanel(bottom = 10, left = 10,
    verbatimTextOutput("summary")
  )
  # sidebarLayout(
  #   
  #   sidebarPanel(
  #     sliderInput("obs", "Number of observations:",  
  #                 min = 1, max = 1000, value = 500)
  #   )
  # )
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
  women_courts <- completeFun(read_csv("../Government/women_courts.csv"), "Value")
  women_courts <- women_courts %>% filter(Value > 0)
  access_space <- completeFun(read_csv("../Development/Access_To_Public_Space.csv"), "Value")
  access_space <- access_space %>% filter(Value > 0)
  early_marriage <- completeFun(read_csv("../Development/Early_Marriage.csv"), "Value")
  early_marriage$Value <- early_marriage$Value * 100
  early_marriage <- early_marriage %>% filter(Value > 0)
  female_genital_mutilation <- completeFun(read_csv("../Development/Female_Genital_Mutilation.csv"), "Value")
  female_genital_mutilation$Value <- female_genital_mutilation$Value * 100
  female_genital_mutilation <- female_genital_mutilation %>% filter(Value > 0)
  women_laws <- completeFun(read_csv("../Development/Laws.csv"), "Value")
  women_laws$Value <- women_laws$Value * 100
  women_laws <- women_laws %>% filter(Value > 0)
  prevelance_of_violence <- completeFun(read_csv("../Development/Prevelance_of_Violence.csv"), "Value")
  prevelance_of_violence$Value <- prevelance_of_violence$Value * 100
  prevelance_of_violence <- prevelance_of_violence %>% filter(Value > 0)
  son_education_preference <- completeFun(read_csv("../Development/Son_Education_Preference.csv"), "Value")
  son_education_preference$Value <- son_education_preference$Value * 100
  son_education_preference <- son_education_preference %>% filter(Value > 0)
  attitudes_towards_violence <- completeFun(read_csv("../Development/Attitudes_Towards_Violence.csv"), "Value")
  attitudes_towards_violence$Value <- attitudes_towards_violence$Value * 100
  attitudes_towards_violence <- attitudes_towards_violence %>% filter(Value > 0)
  emp_seats <- completeFun(read_csv("../Employment/GENDER_EMP_BOARD_SEATS.csv"), "Value")
  emp_seats <- emp_seats %>% filter(Value > 0)
  emp_paid_unpaid <- completeFun(read_csv("../Employment/GENDER_EMP_PAID_UNPAID.csv"), "Value")
  emp_paid_unpaid <- emp_paid_unpaid %>% filter(Value > 0)
  emp_employed <- completeFun(read_csv("../Employment/GENDER_EMP_UNEMP.csv"), "Value")
  emp_employed <- emp_employed %>% filter(Value > 0)
  emp_wage <- completeFun(read_csv("../Employment/GENDER_EMP_WAGE_GAP.csv"), "Value")
  emp_wage <- emp_wage %>% filter(Value > 0)
  women_parliament <-  completeFun(read_csv("../Government/women_parliament.csv"), "Value")
  women_court_instance <-  completeFun(read_csv("../Government/women_court_instance.csv"), "Value")
  women_central_government <-  completeFun(read_csv("../Government/women_central_government.csv"), "Value")
  women_ministers <-  completeFun(read_csv("../Government/women_ministers.csv"), "Value")
  women_judges <-  completeFun(read_csv("../Government/women_judges.csv"), "Value")
  
  variableInput <- reactive({
    switch(input$gender_variables,
           "Enrolment Rate by Age" = merge(enrollment_by_age, countries_data, by="Country"),
           "Adult Education" = merge(adult_education, countries_data, by="Country"),
           "Distribution of teachers by gender" = merge(distribution_teachers, countries_data, by="Country"),
           "Graduation Rates" = merge(graduation_rates, countries_data, by="Country"),
           "Transition from school" = merge(transition, countries_data, by="Country"),
           "Graduation Field" = merge(graduation_field, countries_data, by="Country"),
           "Women in Parliament" = merge(women_parliament, countries_data, by="Country"),
           "Women in Court Instance" = merge(women_court_instance, countries_data, by="Country"),
           "Women in Central Government" = merge(women_central_government, countries_data, by="Country"),
           "Women Ministers" = merge(women_ministers, countries_data, by="Country"),
           "Women Judges" = merge(women_judges, countries_data, by="Country"),
           "Access to Public Space" = merge(access_space, countries_data, by="Country"),
           "Early Marriage" = merge(early_marriage, countries_data, by="Country"),
           "Female_Genital_Mutilation" = merge(female_genital_mutilation, countries_data, by="Country"),
           "Women Laws" = merge(women_laws, countries_data, by="Country"),
           "Prevelance_of_Violence" = merge(prevelance_of_violence, countries_data, by="Country"),
           "Son_Education_Preference" = merge(son_education_preference, countries_data, by="Country"),
           "Attitudes_Towards_Violence" = merge(attitudes_towards_violence, countries_data, by="Country"),
           "Female Share of Board Seats" = merge(emp_seats, countries_data, by="Country"),
           "Time Spent in paid and unpaid work" = merge(emp_paid_unpaid, countries_data, by="Country"),
           "Employment Rate" = merge(emp_employed, countries_data, by="Country"),
           "Gender Wage Gap" = merge(emp_wage, countries_data, by="Country")
           )
  })
  
  variableSummary <- reactive({
    switch(input$gender_variables,
           "Enrolment Rate by Age" = "Enrolment rate per age is\nthe percentage of students enrolled in each type of institution\nover the total of students.",
           "Adult Education" = "This indicator presents internationally\ncomparable data on participation\nin adult learning activities\n(formal and/or non-formal education).",
           "Distribution of teachers by gender" = "Distribution of teachers by\ngender and different age groups.",
           "Graduation Rates" = "Graduation/entry rates represent an estimated\npercentage of an age groupexpected to\ngraduate/enter a certain level of education\nat least once in their lifetime.",
           "Transition from school" = "This indicator presents internationally\ncomparable data on labour force status\nand participation in formal education,\nby educational attainment, age and gender as\nreported by the labour force survey (LFS)\nand published in OECD Education at a Glance 2018.\nFor trend data, the Education at a Glance Database\nincludes data from 1997 to 2017 (or years with available data).",
           "Graduation Field" = "Graduates/new entrants in each educational field\nas a percentage of the sum of graduates/new entrants in all fields.",
           "Women in Parliament" = "",
           "Women in Court Instance" = "",
           "Women in Central Government" = "",
           "Women Ministers" = "",
           "Women Judges" = "",
           "Access to Public Space" = "",
           "Early Marriage" = "",
           "Female_Genital_Mutilation" = "",
           "Women Laws" = "",
           "Prevelance_of_Violence" = "",
           "Son_Education_Preference" = "",
           "Attitudes_Towards_Violence" = "",
           "Female Share of Board Seats" = "Female share of seats on boards\nof the largest publicly listed companies",
           "Time Spent in paid and unpaid work" = "Time spent in paid and unpaid work, by sex",
           "Employment Rate" = "Employment and unemployment rate, by sex\nand age group, quarterly data",
           "Gender Wage Gap" = "Gender pay gaps persist and wome\nare more likely to end their lives in poverty. ")
  })
  output$summary <- renderText({ variableSummary() })
  
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
                 fillColor = ~pal(Value), fillOpacity = 0.5, popup = ~paste(Country, round(Value,digits=2))
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

