#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
load.libraries <- c('shiny', 'leaflet', 'RColorBrewer', 'shinydashboard','tidyverse')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs, dep = T)
sapply(load.libraries, require, character = TRUE)

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
                         menuSubItem("Early Marriage", tabName = "Early_Marriage"),
                         menuSubItem("Female Genital Mutilation", tabName = "Female_Genital_Mutilation"),
                         menuSubItem("Prevelance of Violence", tabName = "Prevelance_of_Violence"),
                         menuSubItem("Son Education Preference", tabName = "Son_Education_Preference"),
                         menuSubItem("Attitudes Towards Violence", tabName = "Attitudes_Towards_Violence")
                ),
                menuItem("Employment", icon = icon("dollar"),
                         menuSubItem("Female Share of Board Seats", tabName = "Female_Share_of_Board_Seats"),
                         menuSubItem("Time Spent in work", tabName = "Time_Spent_in_paid_and_unpaid_work"),
                         menuSubItem("Employment Rate", tabName = "Employment_Rate"),
                         menuSubItem("Gender Wage Gap", tabName = "Gender_Wage_Gap")),
                menuItem("Education", icon = icon("book"),
                         menuSubItem("Adult Education", tabName = "Adult_Education"),
                         menuSubItem("Enrollment Rate", tabName = "Enrolment_Rate"),
                         menuSubItem("Distribution of teachers", tabName = "Distribution_of_teachers"),
                         menuSubItem("Transition from school", tabName = "Transition_from_school"),
                         menuSubItem("Graduation Field", tabName = "Graduation_Field")),
                menuItem("Government", icon = icon("balance-scale"),
                         menuSubItem("Women in Parliament", tabName = "Women_in_Parliament"),
                         menuSubItem("Women in Court Instance", tabName = "Women_in_Court_Instance"),
                         menuSubItem("Women in Central Government", tabName = "Women_in_Central_Government"),
                         menuSubItem("Women Ministers", tabName = "Women_Ministers"),
                         menuSubItem("Women Judges", tabName = "Women_Judges")))
  ),
  dashboardBody(
    tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
    textOutput("summary"),
    leafletOutput("map", width = "100%", height = "100%"),
    checkboxInput("legend", "Show legend", TRUE)
    #imageOutput("graph1")
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
  women_court_instance <- completeFun(read_csv("Government/women_court_instance.csv"), "Value")
  women_central_government <- completeFun(read_csv("Government/women_central_government.csv"), "Value")
  women_ministers <- completeFun(read_csv("Government/women_ministers.csv"), "Value")
  women_judges <- completeFun(read_csv("Government/women_judges.csv"), "Value")
  
  # output$graph1 <- renderImage({
  #   if(input$tabs == "Access to Public Space Graph"){
  #     return(list(
  #       src = "Access_to_Public_Space.png",
  #       contentType = "image/png",
  #       alt = "Access_to_Public_Space"
  #     ))
  #     }
  #   })
  variableInput <- reactive({
    #input$tabs
    if(input$tabs == "Early_Marriage"){
      output$summary <- renderText({
        paste("Percentage of women married between 15 and 19 years")
      })
      return(merge(early_marriage, countries_data, by="Country"))
    }
    else if(input$tabs == "Female_Genital_Mutilation"){
      output$summary <- renderText({
        paste("Percentage of women who have undergone any type of female genital mutilation")
      })
      return(merge(female_genital_mutilation, countries_data, by="Country"))
    }
    else if(input$tabs == "Prevelance_of_Violence"){
      output$summary <- renderText({
        paste("Percentage of women who have experienced physical and/pr sexual violence from an intimate partner at some time in their lives.")
      })
      return(merge(prevelance_of_violence, countries_data, by="Country"))
    }
    else if(input$tabs == "Son_Education_Preference"){
      output$summary <- renderText({
        paste("Percentage of people agreeing that university is more important for boys than for girls.")
      })
      return(merge(son_education_preference, countries_data, by="Country"))
    }
    else if(input$tabs == "Attitudes_Towards_Violence"){
      output$summary <- renderText({
        paste("Percentage of women who agree that a husband/partner is justified in beating his wife/partner under certain circumstances.")
      })
      return(merge(attitudes_towards_violence, countries_data, by="Country"))
    }
    else if(input$tabs == "Female_Share_of_Board_Seats"){
      output$summary <- renderText({
        paste("Female share of seats on boards of the largest publicly listed companies")
      })
      return(merge(emp_seats, countries_data, by="Country"))
    }
    else if(input$tabs == "Time_Spent_in_paid_and_unpaid_work"){
      output$summary <- renderText({
        paste("Time spent in paid and unpaid work in different countries.")
      })
      return(merge(emp_paid_unpaid, countries_data, by="Country"))
    }
    else if(input$tabs == "Employment_Rate"){
      output$summary <- renderText({
        paste("Employment and unemployment rate, by sex and age group, quarterly data")
      })
      return(merge(emp_employed, countries_data, by="Country"))
    }
    else if(input$tabs == "Gender_Wage_Gap"){
      output$summary <- renderText({
        paste("Percentage of women who are more likely to end their lives in poverty due to gender pay gaps.")
      })
      return(merge(emp_wage, countries_data, by="Country"))
    }
    else if(input$tabs == "Adult_Education"){
      output$summary <- renderText({
        paste("This indicator presents internationally comparable data on participation of women in adult learning activities (formal and/or non-formal education).")
      })
      merged_df <- merge(adult_education, countries_data, by="Country")
      merged_df <- merged_df %>%
        select(Country, Latitude, Longitude, SEX, Value)
      merged_df <- merged_df %>% filter(SEX == "F")
      return(merged_df)
    }
    else if(input$tabs == "Enrolment_Rate"){
      output$summary <- renderText({
        paste("Percentage of students enrolled in each type of institution over the total of students.")
      })
      return(merge(enrollment_by_age, countries_data, by="Country"))
    }
    else if(input$tabs == "Transition_from_school"){
      output$summary <- renderText({
        paste("This indicator presents internationally comparable data on labour force
              status and participation in formal education, by educational attainment,
              age and gender as reported by the labour force survey (LFS) and published
              in OECD Education at a Glance 2018. For trend data, the Education at a
              Glance Database includes data from 1997 to 2017 (or years with available data).")
      })
      merged_df <- merge(transition, countries_data, by="Country")
      merged_df <- merged_df %>%
        select(Country, Latitude, Longitude, SEX, Value)
      merged_df <- merged_df %>% filter(SEX == "F")
      return(merged_df)
    }
    else if(input$tabs == "Distribution_of_teachers"){
      output$summary <- renderText({
        paste("Distribution of female teachers in different countries")
      })
      merged_df <- merge(distribution_teachers, countries_data, by="Country")
      merged_df <- merged_df %>%
        select(Country, Latitude, Longitude, SEX, Value)
      merged_df <- merged_df %>% filter(SEX == "F")
      return(merged_df)
    }
    
    else if(input$tabs == "Graduation_Field"){
      output$summary <- renderText({
        paste("Graduates/new entrants in each educational field as a percentage of the sum of graduates/new entrants in all fields.")
      })
      return(merge(graduation_field, countries_data, by="Country"))
    }
    else if(input$tabs == "Women_in_Parliament"){
      output$summary <- renderText({
        paste("Share of women parliamentarians")
      })
      return(merge(women_parliament, countries_data, by="Country"))
    }
    else if(input$tabs == "Women_in_Court_Instance"){
      output$summary <- renderText({
        paste("Share of women in courts of first instance")
      })
      return(merge(women_court_instance, countries_data, by="Country"))
    }
    else if(input$tabs == "Women_in_Central_Government"){
      output$summary <- renderText({
        paste("Share of central government employment filled by women")
      })
      return(merge(women_central_government, countries_data, by="Country"))
    }
    else if(input$tabs == "Women_Ministers"){
      output$summary <- renderText({
        paste("Share of women ministers")
      })
      return(merge(women_ministers, countries_data, by="Country"))
    }
    else if(input$tabs == "Women_Judges"){
      output$summary <- renderText({
        paste("Share of professional judges that are women")
      })
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
      addCircles(radius = ~((Value/max(Value)) * 100) ^ 2.8, weight = 1.5, color = "white",
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
  
 
}

shinyApp(ui, server)