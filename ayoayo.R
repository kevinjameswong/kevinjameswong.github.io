library(dplyr)
library(ggplot2)
library(kableExtra)
library(knitr)
library(lubridate)
library(rlang)
library(scales)
library(shiny)
library(shinydashboard)
library(stringr)
library(tidyverse)

korean = read.csv(file = "files/koreanphrases.csv")
korean$Index = korean %>% nrow() %>% seq.int()
korean = korean %>%
  group_by(English, Korean, Romanization) %>%
  mutate(count = n()) %>%
  #filter(Romanization == "hwajangsil") %>%
  mutate(TT = min(Index)) %>% #the min Index per row
  mutate(Duplicate = ifelse(TT == Index, 0, 1) %>% as.integer()) %>%
  select(English, Korean, Romanization, Index, Duplicate) %>%
  filter(Index %in% c(1, 3, 5, 7, 9))




uix = dashboardPage(
  skin = "green",
  dashboardHeader(
    title = "Learn Korean - Kevin Wong",
    disable = FALSE),  
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        text = "Learn",
        icon = icon(name = "lightbulb"),
        tabName = "Learn",
        badgeLabel = "NEW",
        selected = TRUE),
      menuItem(
        text = "Quiz",
        icon = icon(name = "check"),
        tabName = "Quiz",
        badgeLabel = "NEW",
        selected = FALSE)
    )
  ),
  
  dashboardBody(
    
    tabItems(
      
      tabItem(tabName = "Learn",
              
              fluidRow(
                box(
                  width = 12,
                  
                  h4("Question 1"),
                  textOutput(outputId = "question_question01"),
                  uiOutput(outputId = "buttons_question01"),
                  br()
                )
              )
              
              
      ),
      tabItem(tabName = "Quiz",
              h1("TT"))
      
      
    )))

serverx = function(input, output, session) {
    
    output$buttons_question01 = renderUI({
      radioButtons(
        inputId = "submission_question01",
        label = textOutput(outputId = "question_question01"),
        choiceNames = list(
          HTML("<font color='red'>Normal</font>"), 
          tags$span(style = "color:red", "Uniform"), 
          "Log-normal", "Exponential"
        ),
        choiceValues = c("norm", "unif", "lnorm", "exp"),
        selected = character(0)
        #) %>% div(style = "color:black; font-size:120%; background-color: white;")
      ) %>% div(style = "font-size:120%;")

  })
  
  
  
  
  
}







shinyApp(
  ui = uix,
  server = serverx
)