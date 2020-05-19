
#Initial concept of the shiny app for a cyclical diet pattern 

#libraries

library(tidyverse)
library(glue)
library(shiny)
library(shinydashboard)
library(scales)
library(ggthemes)

# gobal ----


# ui ----
ui <- dashboardPage(
  
  dashboardHeader(title = "Beta Data Science Diet v0.02",
                  dropdownMenu(type = "message", 
                               badgeStatus = "success",
                               messageItem("Support Team",
                                           "T.J.A.B!"))),
  
  dashboardSidebar(
  
    #Current Metrics
    h3("Starting Metrics"),
    numericInput("ht", label = h4("Height (in)"), value = 68),
    numericInput("bw", label = h4("Weight (lbs)"), value = 183),
    numericInput("bfp", label = h4("Body Fat (%)"), value = 0.235),
    
    #Goal Metrics
    h3("Goal Metrics"),
    numericInput("goal_bw", label = h4("Goal Weight (lbs)"), value = 175),
    numericInput("goal_bfp", label = h4("Goal Body Fat (%)"), value = 0.2),
    
    #deficit day select
    checkboxGroupInput("def_days", 
                       label = h4("Deficit Days"), 
                       choices = list("Sunday"    = "sun", 
                                      "Monday"    = "mon", 
                                      "Tuesday"   = "tue",
                                      "Wednesday" = "wed",
                                      "Thursday"  = "thu",
                                      "Friday"    = "fri",
                                      "Saturday"  = "sat"),
                       selected = c("mon","wed")),
    
    #deficit day cals ---- 
    numericInput("def_cal", label = h4("Deficit Day Intake Calories"), value = 1200),
    
    #exercise day select
    checkboxGroupInput("ex_days", 
                       label = h4("Exercise Days"), 
                       choices = list("Sunday"    = "sun", 
                                      "Monday"    = "mon", 
                                      "Tuesday"   = "tue",
                                      "Wednesday" = "wed",
                                      "Thursday"  = "thu",
                                      "Friday"    = "fri",
                                      "Saturday"  = "sat"),
                       selected = c("tue","thu", "sat")),
    
    #exercise day cals ---- 
    numericInput("ex_cal", label = h4("Exercise Day Intake Calories"), value = 2600)
    #numericInput("ex_out_cal", label = h3("Exercise Day Extra Burn Calories"), value = 300)
    
    
  ),
  dashboardBody(
    h3("Starting Metrics"),
    fluidRow(
      infoBoxOutput('tdee'),
      infoBoxOutput('lm'),
      infoBoxOutput('fm'),
    ),
    hr(),
    h3("Goal Metrics"),
    fluidRow(
      infoBoxOutput('goal_tdee'),
      infoBoxOutput('goal_lm'),
      infoBoxOutput('goal_fm'),
    ),
    hr(),
    h3("Weekly Changes"),
    fluidRow(infoBoxOutput('wk_def'),
             infoBoxOutput('wk_lean_chg'),
             infoBoxOutput('wk_fat_chg')),
    hr(),
    fluidRow(
    valueBoxOutput("proj_box"),
    valueBoxOutput('proj_lean'),
    valueBox("TODO Fat Chg wks", 
             subtitle = "or warn if won't achieve", 
             color = 'yellow',
             icon = icon("cookie-bite"))),
    hr(),
    fluidRow(
      box(title = "Weekly Pattern",
          width = 12,
          plotOutput('weekplot'))),
    hr(),
    fluidRow(
      box(title = "Projection",
          width = 12,
          plotOutput('proj_plot')))
 )
)



# server ----
server <- function(input, output, session){
  
  tdee_rx <- reactive({round((66 + 13.7*(input$bw/2.2) + 5 * 2.5 * input$ht - 6.8 *33)*1.4)})
  
  goal_tdee_rx <- reactive({
    round((66 + 13.7*(input$goal_bw/2.2) + 5 * 2.5 * input$ht - 6.8 *33)*1.4)
  })
  
  week_net <- reactive({
    tdee_rx()*7 - (length(input$def_days)*input$def_cal + 
                   length(input$ex_days)*input$ex_cal + 
                   7-length(c(input$def_days, input$ex_days)) * tdee_rx())
  })
  
  
  surpluses <- reactive({(input$ex_cal - tdee_rx())/700 * length(input$ex_days)})
  deficits <- reactive({(input$def_cal - tdee_rx())/3500 * length(input$def_days)})
  
  lean_net <- reactive({surpluses() * 0.75 + deficits() * 0.25})
  fat_net <- reactive({deficits() * 0.75 + surpluses() * 0.25})
  
  # weekly plot
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
      theme_few() +
      scale_fill_few() +
      expand_limits(y = 1.2 * tdee_rx()) +
      labs(title = "Weekly Calories Pattern",
           subtitle = "draft",
           x = "Day",
           y = "Calories",
           fill = "Day Calories",
           caption = "Rough calculations and chart")
  
  })
    
    
  #energy content of weight change (kcal/kg) = 1020 (ΔFFM/ΔW) + 9500 (1 - ΔFFM/ΔW)
  
  #Baseline composition plots ----
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

  # Goal Composition Outputs
  #TDEE srv ----
  output$goal_tdee <- renderInfoBox({
    infoBox(
      title = "Goal TDEE",
      subtitle = "Daily kCals",
      value = goal_tdee_rx(),
      icon = icon("fire"), 
      color = 'red')})
  
  
  #Lean Mass srv ----
  output$goal_lm <- renderInfoBox({
    infoBox("Goal Lean Mass (lbs)", 
            round(input$goal_bw*(1-input$goal_bfp)),
            icon = icon("dumbbell"), 
            color = 'green')
  })
  
  
  #Fat Mass ----
  output$goal_fm <- renderInfoBox({
    infoBox("Goal Fat Mass (lbs)", 
            round(input$goal_bw*input$goal_bfp),
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
      icon  = icon("dumbbell"), 
      color = 'green')
  })

  
    
  # Weekly Fat change ----
  
  output$wk_fat_chg <- renderInfoBox({
    infoBox(title = "Fat Change (lbs)",
            value = round(fat_net(), digits = 1), 
            icon  = icon("cookie"), 
            color = 'yellow')
  })
  
  
  
  output$weekplot <- renderPlot({print(wk_days_plot())})
  
  
  # projection
  
  bw_chg <- reactive({lean_net() + fat_net()})
  
  bw_weeks_proj <- reactive({ceiling((input$bw - input$goal_bw)/-bw_chg())})
  
  lean_weeks_proj <- reactive({
    
    (round(input$bw*(1-input$bfp)) - round(input$goal_bw*(1-input$goal_bfp))) / lean_net()
    
    
    })
  
  proj_df <- reactive({
    tibble(week = 1:(bw_weeks_proj()),
           bw   = input$bw) %>% 
      mutate(new_bw = bw + week*bw_chg(),
             new_bw = round(new_bw, digits = 1))
  })
  
  
  proj_plot <- reactive({
    
    proj_df() %>% 
      ggplot(aes(x = as.factor(week),
                 y = new_bw)) +
      geom_col(fill = "lightblue") +
      geom_text(aes(label = new_bw), vjust = 1) +
      theme_few() +
      expand_limits(y = 0) +
      labs(title = "Projected Changes",
           subtitle = "TODO: Separate Lean / Fat Mass fill",
           caption = "Reference Kevin Hall modeling study")
    
  })
  
  output$proj_box <- renderValueBox({
    valueBox(
    value = glue("{bw_weeks_proj()} weeks"),
    subtitle = "to reach goal body weight", 
    color = 'aqua',
    icon = icon("weight"))
  })
  
  
  output$proj_lean <- renderValueBox({
    valueBox(value = glue("{lean_weeks_proj()} weeks"),
             subtitle = "to reach goal lean lbs",
             color = 'green',
             icon = icon("dumbbell"))
    
    
  })
  
  
  
  output$proj_plot <- renderPlot({print(proj_plot())})
  
  
}

# app ----
shinyApp(ui, server)
