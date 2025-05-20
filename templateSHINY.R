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

spotify = read.csv(file = "files/SpotifyWrapped.csv")

percent = function(x, digits = 2, format = "f", ...) {
  t = formatC(x * 100, format = format, digits = digits, ...) %>% paste("%", sep = "")
  return(t)
}

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
        selected = TRUE #FALSE
      ),
      menuItem(
        text = "Quiz",
        icon = icon(name = "check"),
        tabName = "Quiz",
        badgeLabel = "NEW",
        selected = FALSE
      ),
      menuItem(
        text = "Dictionary",
        icon = icon(name = "book"),
        tabName = "Dictionary",
        badgeLabel = "NEW",
        selected = FALSE
      ),
      menuItem(
        text = "Romanization",
        icon = icon(name = "keyboard"),
        tabName = "Romanization",
        badgeLabel = "NEW",
        selected = FALSE
      )
    )
  ),
  
  dashboardBody(
    tags$script(HTML("$('body').addClass('fixed');")),

    tabItems(
      
      tabItem(tabName = "Learn",
              fluidRow(
                box(
                  column(width = 12,
                  h1("Learn")
                  )
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
      ),
      
      tabItem(tabName = "Dictionary",
              fluidRow(
                box(
                  column(width = 12,
                         h1("Dictionary")
                  )
                )
              )
      ),
      
      tabItem(tabName = "Romanization",
              fluidRow(
                box(
                  column(width = 12,
                         h1("Romanization")
                  )
                )
              )
      )
    )#tabItems
  )#dashboardBody
) #dashboardPage

serverx = function(input, output, session) {

  observeEvent(input$R_FR_Submit, {
    R_submission_written_start$R_NEW_INTMORE = sample(x = korean_duo %>% group_by(Index) %>% select(Index) %>% unlist() %>% unname(), size = 1, replace = TRUE)
  })

  
  R_FR_Korean = eventReactive(
    eventExpr = input$R_FR_Submit,
    valueExpr = {paste("Korean: ", korean_duo[R_submission_written_start$R_NEW_INTMORE, ]$Korean, sep = "")},
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )
  
  
}

#eventRactive = update value
#observeEvent = make something happen



shinyApp(
  ui = uix,
  server = serverx
)