library(dplyr)
library(ggplot2)
library(kableExtra)
library(knitr)
library(lubridate)
library(rlang)
library(shiny)
library(shinydashboard)
library(stringr)
library(tidyverse)

percent = function(x, digits = 2, format = "f", ...) {
  t = formatC(x * 100, format = format, digits = digits, ...) %>% paste("%", sep = "")
  return(t)
}

uix = dashboardPage(
  
  skin = "blue",
  dashboardHeader(
    title = "Word Replacement - Kevin Wong",
    disable = FALSE),  
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        text = "Word Replacement",
        icon = icon(name = "keyboard"),
        tabName = "Word",
        badgeLabel = "NEW",
        selected = TRUE
      )
    )
  ),
  
  dashboardBody(
    tags$script(HTML("$('body').addClass('fixed');")),
    
    tabItems(
      
      tabItem(tabName = "Word Replacement",
              fluidRow(
                box(
                  width = 6,
                  textAreaInput(inputId = "Input_Area", label = "Input", value = "ENTER TEXT here.", placeholder = "ENTER TEXT HERE.")
                ),
                box(
                  width = 6,
                  verbatimTextOutput(outputId = "TTend")
                )
              ),
              fluidRow(
                width = 12,
                box(
                  width = 6,
                textInput(inputId = "FindText", label = "Find", value = ".", placeholder = "Find"),
                textInput(inputId = "ReplaceText", label = "Replace", value = "", placeholder = "Replace"),
                actionButton(inputId = "Replace_Button", label = "Replace", width = "100%"),
                actionButton(inputId = "Upper_Button", label = "UPPER", width = "100%"),
                actionButton(inputId = "Lower_Button", label = "lower", width = "100%")
              )
              )
      ),
      
      tabItem(tabName = "Geometry Replacement",
              fluidRow(
                box(
                  title = "Triangle",
                  width = 12,
                  
                )
              ),
                box(
                  width = 6,
                  verbatimTextOutput(outputId = "TTend")
                )
              ),
              fluidRow(
                width = 12,
                box(
                  width = 6,
                  textInput(inputId = "FindText", label = "Find", value = ".", placeholder = "Find"),
                  textInput(inputId = "ReplaceText", label = "Replace", value = "", placeholder = "Replace"),
                  actionButton(inputId = "Replace_Button", label = "Replace", width = "100%"),
                  actionButton(inputId = "Upper_Button", label = "UPPER", width = "100%"),
                  actionButton(inputId = "Lower_Button", label = "lower", width = "100%")
                )
              )
      ),
      
      tabItem(tabName = "Quiz",
              fluidRow(
                box(
                  column(width = 12,
                         h1("Quiz")
                  )
                )
              )
      )
    )#tabItems
  )#dashboardBody
) #dashboardPage

serverx = function(input, output, session) {
  
  comment_value = reactiveValues(outputt = "", find = ".", replace = "")

  
  observeEvent(input$Replace_Button, {
    comment_value$find = input$FindText
  })
  
  observeEvent(input$Replace_Button, {
    comment_value$replace = input$ReplaceText
  })
  
  observeEvent(input$Replace_Button, {
    comment_value$outputt = str_replace_all(string = input$Input_Area, pattern = comment_value$find, replacement = comment_value$replace)
  })
  
  output$TTend = renderText({
    comment_value$outputt
  })

  
}

#eventRactive = update value
#observeEvent = make something happen



shinyApp(
  ui = uix,
  server = serverx
)