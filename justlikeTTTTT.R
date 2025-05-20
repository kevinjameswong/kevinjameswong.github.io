library(dplyr)
library(gghighlight)
library(ggplot2)
library(kableExtra)
library(knitr)
library(lubridate)
library(rlang)
library(shiny)
library(shinydashboard)
library(stringr)
library(tidyverse)


subjects = read.csv(file = "files/korean_subjects.csv") %>%
  filter(English != '')
subjects$Index = subjects %>% nrow() %>% seq.int()


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
    #shinyjs::useShinyjs(), # required to enable Shinyjs
    tabItems(
      
      tabItem(tabName = "Learn",
              
              fluidRow(
                box(width = 4,
                    checkboxGroupInput(
                      inputId = "E_QuestionDifficulty",
                      label = "Question Difficulty:",
                      choiceNames = c("Easy", "Hard"),
                      choiceValues = c(1, 2),
                      #selected = c(1, 2)
                      selected = c(1)
                    )
                )
              ),
              h1("English to Korean"),
              br(),
              fluidRow(
                box(width = 4,
                    actionButton(inputId = "E_KoreanReveal", label = "Reveal Korean", width = "100%", style = "color: #FFFFFF; background-color: #8FC8DC; border-color: #000000; font-size: 120%"),
                    br(),
                    br(),
                    actionButton(inputId = "E_RomanizationReveal", label = "Reveal Romanization", width = "100%", style = "color: #FFFFFF; background-color: #8FC8DC; border-color: #000000; font-size: 120%"),
                    br(),
                    br(),
                    actionButton(inputId = "E_NewWord", label = "New Word", width = "100%", style = "color: #FFFFFF; background-color: #000000; border-color: #000000; font-size: 120%")
                ),
                box(width = 8,
                    textOutput(outputId = "E_English") %>% span(style = "font-size:20px;"),
                    textOutput(outputId = "E_Korean") %>% span(style = "font-size:20px;"),
                    textOutput(outputId = "E_Romanization") %>% span(style = "font-size:20px;")
                )
              ),
              hr(),
              h1("Korean to English"),
              br(),
              fluidRow(
                box(width = 4,
                    actionButton(inputId = "K_EnglishReveal", label = "Reveal English", width = "100%", style = "color: #FFFFFF; background-color: #027A54; border-color: #000000; font-size: 120%"),
                    br(),
                    br(),
                    actionButton(inputId = "K_RomanizationReveal", label = "Reveal Romanization", width = "100%", style = "color: #FFFFFF; background-color: #027A54; border-color: #000000; font-size: 120%"),
                    br(),
                    br(),
                    actionButton(inputId = "K_NewWord", label = "New Word", width = "100%", style = "color: #FFFFFF; background-color: #000000; border-color: #000000; font-size: 120%")
                ),
                box(width = 8,
                    textOutput(outputId = "K_Korean") %>% span(style = "font-size:20px;"),
                    textOutput(outputId = "K_English") %>% span(style = "font-size:20px;"),
                    textOutput(outputId = "K_Romanization") %>% span(style = "font-size:20px;")
                )
              ),
              hr(),
              h1("Romanization to English"),
              br(),
              fluidRow(
                box(width = 4,
                    actionButton(inputId = "R_EnglishReveal", label = "Reveal English", width = "100%", style = "color: #FFFFFF; background-color: #DEC371; border-color: #000000; font-size: 120%"),
                    br(),
                    br(),
                    actionButton(inputId = "R_KoreanReveal", label = "Reveal Korean", width = "100%", style = "color: #FFFFFF; background-color: #DEC371; border-color: #000000; font-size: 120%"),
                    br(),
                    br(),
                    actionButton(inputId = "R_NewWord", label = "New Word", width = "100%", style = "color: #FFFFFF; background-color: #000000; border-color: #000000; font-size: 120%")
                ),
                box(width = 8,
                    textOutput(outputId = "R_Romanization") %>% span(style = "font-size:20px;"),
                    textOutput(outputId = "R_English") %>% span(style = "font-size:20px;"),
                    textOutput(outputId = "R_Korean") %>% span(style = "font-size:20px;")
                )
              )
      )
      
    )
  )
) #dashboardPage

serverx = function(input, output, session) {
  
  E_start = reactiveValues(
    Subject_Int = 1,
    Object_Int = 325,
    Verb_Int = 325,
    Tense_Int = 325,
    Formality_Int = 325,
    Context_Int = 325,
    SubjectMarker_Int = 1
  )
  
  
  observeEvent(input$E_NewWord, {
    E_start$Subject_Int =
      sample(x = subjects %>% group_by(Index) %>% select(Index) %>% unlist() %>% unname(), size = 1, replace = TRUE)
  })
  
  
  observeEvent(input$E_NewWord, {
    output$E_Korean = NULL
  })
  
  observeEvent(input$E_NewWord, {
    output$E_Romanization = NULL
  })
  
  output$E_English = renderText({
    E_start$E_NEW_Int
    #paste("English: ", korean[E_start$E_NEW_Int, ]$English, sep = "")
  })
  
  observeEvent(input$E_KoreanReveal, {
    output$E_Korean = renderText({
      paste("Korean: ", korean[E_start$E_NEW_Int, ]$Korean, sep = "")
    })
  })
  
  observeEvent(input$E_RomanizationReveal, {
    output$E_Romanization = renderText({
      paste("Romanization: ", korean[E_start$E_NEW_Int, ]$Romanization, sep = "")
    })
  })
  
  
  
  
  #Korean
  K_start = reactiveValues(K_NEW_Int = 326)
  
  observeEvent(input$K_NewWord, {
    K_start$K_NEW_Int =
      if (input$E_QuestionDifficulty == 1 && input$E_QuestionDifficulty == 2) {
        sample(x = korean %>% group_by(Index) %>% select(Index) %>% unlist() %>% unname(), size = 1, replace = TRUE)
      } else if (input$E_QuestionDifficulty == 1 && input$E_QuestionDifficulty != 2) {
        sample(x = korean %>% filter(Easy == 1) %>% group_by(Index) %>% select(Index) %>% unlist() %>% unname(), size = 1, replace = TRUE)
      } else if (input$E_QuestionDifficulty != 1 && input$E_QuestionDifficulty == 2) {
        sample(x = korean %>% filter(Easy == 0) %>% group_by(Index) %>% select(Index) %>% unlist() %>% unname(), size = 1, replace = TRUE)
      } else {
        sample(x = korean %>% group_by(Index) %>% select(Index) %>% unlist() %>% unname(), size = 1, replace = TRUE)
      }
  })
  
  observeEvent(input$K_NewWord, {
    output$K_English = NULL
  })
  
  observeEvent(input$K_NewWord, {
    output$K_Romanization = NULL
  })
  
  observeEvent(input$K_EnglishReveal, {
    output$K_English = renderText({
      E_start$K_NEW_Int
      #paste("English: ", korean[K_start$K_NEW_Int, ]$English, sep = "")
    })
  })
  
  output$K_Korean = renderText({
    paste("Korean: ", korean[K_start$K_NEW_Int, ]$Korean, sep = "")
  })
  
  observeEvent(input$K_RomanizationReveal, {
    output$K_Romanization = renderText({
      paste("Romanization: ", korean[K_start$K_NEW_Int, ]$Romanization, sep = "")
    })
  })
  
  
  
  
  
  
  #Romanization
  R_start = reactiveValues(R_NEW_Int = 1220)
  
  observeEvent(input$R_NewWord, {
    R_start$R_NEW_Int =
      if (input$E_QuestionDifficulty == 1 && input$E_QuestionDifficulty == 2) {
        sample(x = korean %>% group_by(Index) %>% select(Index) %>% unlist() %>% unname(), size = 1, replace = TRUE)
      } else if (input$E_QuestionDifficulty == 1 && input$E_QuestionDifficulty != 2) {
        sample(x = korean %>% filter(Easy == 1) %>% group_by(Index) %>% select(Index) %>% unlist() %>% unname(), size = 1, replace = TRUE)
      } else if (input$E_QuestionDifficulty != 1 && input$E_QuestionDifficulty == 2) {
        sample(x = korean %>% filter(Easy == 0) %>% group_by(Index) %>% select(Index) %>% unlist() %>% unname(), size = 1, replace = TRUE)
      } else {
        sample(x = korean %>% group_by(Index) %>% select(Index) %>% unlist() %>% unname(), size = 1, replace = TRUE)
      }
  })
  
  observeEvent(input$R_NewWord, {
    output$R_English = NULL
  })
  
  observeEvent(input$R_NewWord, {
    output$R_Korean = NULL
  })
  
  observeEvent(input$R_EnglishReveal, {
    output$R_English = renderText({
      paste("English: ", korean[R_start$R_NEW_Int, ]$English, sep = "")
    })
  })
  
  observeEvent(input$R_KoreanReveal, {
    output$R_Korean = renderText({
      paste("Korean: ", korean[R_start$R_NEW_Int, ]$Korean, sep = "")
    })
  })
  
  output$R_Romanization = renderText({
    paste("Romanization: ", korean[R_start$R_NEW_Int, ]$Romanization, sep = "")
  })
  
  
  
  
}



shinyApp(
  ui = uix,
  server = serverx
)