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

korean = read.csv(file = "files/koreanphrases.csv")
korean$Index = korean %>% nrow() %>% seq.int()
korean = korean %>%
  group_by(English, Korean, Romanization) %>%
  mutate(count = n()) %>%
  #filter(Romanization == "hwajangsil") %>%
  mutate(TT = min(Index)) %>% #the min Index per row
  mutate(Duplicate = ifelse(TT == Index, 0, 1) %>% as.integer()) %>%
  select(English, Korean, Romanization, Index, Duplicate) %>%
  filter(Index <= 5)




uix = dashboardPage(
  skin = "green",
  dashboardHeader(
    title = "Learn Korean - Kevin Wong",
    disable = FALSE),  
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        text = "Learn",
        icon = icon(name = "calendar"),
        tabName = "Learn",
        badgeLabel = "NEW",
        selected = TRUE),
      menuItem(
        text = "Quiz",
        icon = icon(name = "th"),
        tabName = "Quiz",
        badgeLabel = "NEW",
        selected = FALSE)
    )
  ),
  
  dashboardBody(
    
    tabItems(
      
      tabItem(tabName = "Learn",
              

                    actionButton(
                      inputId = "quizStart",
                      label = "Start Quiz!"
                    ),
              textOutput(outputId = "A"),
              textOutput(outputId = "B")
              ))))


serverx = function(input, output, session) {
  

  
  observeEvent(input$quizStart, {
    output$A = renderText({
      "2"
    })
    output$B = renderText({
      "3"
    })
  })
  
  
  
  
}







shinyApp(
  ui = uix,
  server = serverx
)