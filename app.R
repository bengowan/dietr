
#Initial concept of the shiny app for a cyclical diet pattern 

#libraries

library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinipsum)
library(plotly)
library(scales)

# gobal ----


# ui ----
ui <- dashboardPage(
  
  dashboardHeader(title = "Data Science Diet"),
  
  dashboardSidebar(
    numericInput("bw", label = h3("Weight (lbs)"), value = 183),
    numericInput("ht", label = h3("Height (in)"), value = 68),
    numericInput("bfp", label = h3("Body Fat (%)"), value = 0.235),
    
    #deficit day select
    checkboxGroupInput("def_days", 
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
    numericInput("def_cal", label = h3("Deficit Day Calories"), value = 1200),
    
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
    numericInput("ex_cal", label = h3("Exercise Day Calories"), value = 2600)
    
    
  ),
  dashboardBody(
    h3("Starting Composition"),
    fluidRow(
      infoBoxOutput('tdee'),
      infoBoxOutput('lm'),
      infoBoxOutput('fm'),
    ),
    hr(),
    h3("Weekly Changes"),
    fluidRow(infoBoxOutput('wk_def'),
             infoBoxOutput('wk_lean_chg'),
             infoBoxOutput('wk_fat_chg')),
    hr(),
    fluidRow(
      box(title = "Weekly Pattern",
          width = 12,
          plotOutput('weekplot'))),
    hr(),
    fluidRow(box(title = "3 month projection",
                 width = 12,
                 plotlyOutput('monthsplot')))
  )
)



# server ----
server <- function(input, output, session){
  
  tdee_rx <- reactive({round((66 + 13.7*(input$bw/2.2) + 5 * 2.5 * input$ht - 6.8 *33)*1.4)})
  
  week_net <- reactive({
    tdee_rx()*7 - (length(input$def_days)*input$def_cal + 
                   length(input$ex_days)*input$ex_cal + 
                   7-length(c(input$def_days, input$ex_days)) * tdee_rx())
  })
  
  
  surpluses <- reactive({(input$ex_cal - tdee_rx())/700 * length(input$ex_days)})
  
  deficits <- reactive({(input$def_cal - tdee_rx())/3500 * length(input$def_days)})
  
  lean_net <- reactive({
    surpluses() * 0.75 + deficits() * 0.25
  })
  
  fat_net <- reactive({
    deficits() * 0.75 + surpluses() * 0.25
  })
  
  
  wk_days_plot <- reactive({
    
    c("sun", "mon", "tue", "wed", "thu", "fri", "sat") %>% 
    enframe() %>% 
    mutate(day_type = case_when(
      value %in% input$def_days ~ "Low",
      value %in% input$ex_days ~ "Extra",
      TRUE ~ "Normal"),
      cals = case_when(
        day_type == "Low" ~ round(input$def_cal),
        day_type == "Extra" ~ round(input$ex_cal),
        TRUE ~ round(tdee_rx())),
      day = fct_reorder(value, name)) %>% 
      ggplot(aes(x = day,
                 y = cals,
                 fill = day_type)) +
      geom_col() +
      theme_minimal() +
      expand_limits(y = 1.2 * tdee_rx()) +
      labs(x = "Day",
           y = "Calories",
           fill = "Day Calories",
           caption = "Rough calculations and chart")
  
  })
    
    
  #energy content of weight change (kcal/kg) = 1020 (ΔFFM/ΔW) + 9500 (1 - ΔFFM/ΔW)
  
  #TDEE srv ----
  output$tdee <- renderInfoBox({
    infoBox(
    title = "TDEE",
    subtitle = "Daily kCals",
    value = tdee_rx(),
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

  # Weekly deficit ----
  output$wk_def <- renderInfoBox({
    
    infoBox(
      title = "Weight Change", 
      value = round(lean_net() + fat_net(), digits = 1),
      icon  = icon("balance-scale-left"))
  })
  
  
  # Weekly Lean change ---- 
  output$wk_lean_chg <- renderInfoBox({
    infoBox(
      title = "Lean Change (lbs)", 
      value = round(lean_net(), digits = 1), 
      icon  = icon("balance-scale-right"), 
      color = 'green')
  })

  
    
  # Weekly Fat change ----
  
  output$wk_fat_chg <- renderInfoBox({
    infoBox(title = "Fat Change (lbs)",
            value = round(fat_net(), digits = 1), 
            icon  = icon("balance-scale-left"), 
            color = 'yellow')
  })
  
  
  
  output$weekplot <- renderPlot({print(wk_days_plot())})
  
  output$monthsplot <- renderPlotly({print(random_ggplotly(type = "line"))})
  
}

# app ----
shinyApp(ui, server)
