
#Initial concept of the shiny app for a cyclical diet pattern 

#libraries

library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinipsum)
library(plotly)


# gobal ----


# ui ----
ui <- dashboardPage(
  
  dashboardHeader(title = "Data Science Diet"),
  
  dashboardSidebar(
    numericInput("weight", label = h3("Weight (lbs)"), value = 185),
    
    numericInput("bfp", label = h3("Body Fat (%)"), value = 0.235),
    
    #deficit day select
    checkboxGroupInput("d_days", 
                       label = h3("Deficit Days"), 
                       choices = list("Sunday"    = "sun", 
                                      "Monday"    = "mon", 
                                      "Tuesday"   = "tue",
                                      "Wednesday" = "wed",
                                      "Thursday"  = "thu",
                                      "Friday"    = "fri",
                                      "Saturday"  = "sat"),
                       selected = c("mon","wed")),
    
    #deficit day cals ---- 
    numericInput("deficit_cal", label = h3("Deficit Day Calories"), value = 1200),
    
    #exercise day select
    checkboxGroupInput("ex_days", 
                       label = h3("Exercise Days"), 
                       choices = list("Sunday"    = "sun", 
                                      "Monday"    = "mon", 
                                      "Tuesday"   = "tue",
                                      "Wednesday" = "wed",
                                      "Thursday"  = "thu",
                                      "Friday"    = "fri",
                                      "Saturday"  = "sat"),
                       selected = c("tue","thu", "sat")),
    
    #exercise day cals ---- 
    numericInput("deficit_cal", label = h3("Exercise Day Calories"), value = 2600)
    
    
  ),
  dashboardBody(
    fluidRow(
      infoBox("TDEE (kCals)", 2300, icon = icon("fire"), color = 'red'),
      infoBox("Lean Mass (lbs)", 140.8, icon = icon("dumbbell"), color = 'green'),
      infoBox("Fat Mass (lbs)", 42.2, icon = icon("cookie-bite"), color = 'yellow'),
    ),
    hr(),
    fluidRow(infoBox("Weekly Deficit", -500), icon = icon("balance-scale-left"),
             infoBox("Weekly Lean Change (lbs)", 0.1, icon = icon("balance-scale-right"), color = 'green'),
             infoBox("Weekly Fat Change (lbs)", -0.5, icon = icon("balance-scale-left"), color = 'yellow'),),
    hr(),
    fluidRow(
      box(title = "Weekly Pattern",
          width = 12,
          plotlyOutput('weekplot'))),
    hr(),
    fluidRow(box(title = "3 month projection",
                 width = 12,
                 plotlyOutput('monthsplot')))
  )
)



# server ----
server <- function(input, output, session){
  
  output$weekplot <- renderPlotly({print(random_ggplotly(type = "bar"))})
  
  output$monthsplot <- renderPlotly({print(random_ggplotly(type = "line"))})
  
}

# app ----
shinyApp(ui, server)
