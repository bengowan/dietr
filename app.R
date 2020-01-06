
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
    numericInput("bw", label = h3("Weight (lbs)"), value = 183),
    numericInput("ht", label = h3("Height (in)"), value = 68),
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
      infoBoxOutput('tdee'),
      infoBoxOutput('lm'),
      infoBoxOutput('fm'),
    ),
    hr(),
    fluidRow(infoBoxOutput('wk_def'),
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
  
  tdee_rx <- reactive({round((66 + 13.7*(input$bw/2.2) + 5 * 2.5 * input$ht - 6.8 *33)*1.4)})
  
  #TDEE srv ----
  output$tdee <- renderInfoBox({
    infoBox(
    title = "TDEE",
    subtitle = "Typical Daily Calories",
    tdee_rx(),
    icon = icon("fire"), 
    color = 'red')})
  
  
  #Lean Mass srv ----
  output$lm <- renderInfoBox({
    infoBox("Lean Mass (lbs)", 
            round(input$bw*(1-input$bfp)),
            icon = icon("dumbbell"), 
            color = 'green')
  })
  
  
  #Fat Mass ----
  output$fm <- renderInfoBox({
    infoBox("Fat Mass (lbs)", 
            round(input$bw*input$bfp),
            icon = icon("cookie-bite"), 
            color = 'yellow')
  })

  # Weekly deficit
  
  output$wk_def <- renderInfoBox({
    
    infoBox(
      title = "Weekly Deficit", 
      , 
      icon = icon("balance-scale-left"))
    
  })
  
  
  
  
  output$weekplot <- renderPlotly({print(random_ggplotly(type = "bar"))})
  
  output$monthsplot <- renderPlotly({print(random_ggplotly(type = "line"))})
  
}

# app ----
shinyApp(ui, server)
