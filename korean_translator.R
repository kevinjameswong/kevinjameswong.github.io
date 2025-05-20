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

korean = read.csv(file = "files/koreanphrases.csv")
korean$Index = korean %>% nrow() %>% seq.int()
korean$Easy = ifelse(
  test = is.na(korean$Easy),
  yes = 0,
  no = 1
)
verbs = korean %>%
  group_by(English, Korean, Romanization, Easy) %>%
  mutate(count = n()) %>%
  #filter(Romanization == "hwajangsil") %>%
  mutate(TT = min(Index)) %>%
  mutate(Duplicate = ifelse(TT == Index, 0, 1) %>% as.integer()) %>%
  select(English, Korean, Romanization, Easy, Index, Duplicate) %>%
  ungroup() %>%
  filter(
    English %>% str_to_lower() %>% str_sub(start = 1, end = 3) == "to ",
    Korean %>% str_sub(start = Korean %>% str_length()) == "다",
    Easy == 1)


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
                    actionButton(inputId = "E_KoreanReveal", label = "Reveal Korean", width = "100%", style = "color: #FFFFFF; background-color: #10263C; border-color: #000000; font-size: 120%"),
                    br(),
                    br(),
                    actionButton(inputId = "E_RomanizationReveal", label = "Reveal Romanization", width = "100%", style = "color: #FFFFFF; background-color: #E2C69A; border-color: #000000; font-size: 120%"),
                    br(),
                    br(),
                    actionButton(inputId = "E_NewWord", label = "New Sentence", width = "100%", style = "color: #FFFFFF; background-color: #000000; border-color: #000000; font-size: 120%")
                ),
                box(width = 8,
                    uiOutput(outputId = "English") %>% span(style = "font-size:20px;"),
                    uiOutput(outputId = "E_Korean") %>% span(style = "font-size:20px;"),
                    textOutput(outputId = "E_Romanization") %>% span(style = "font-size:20px;")
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
  
  
  #observeEvent(input$E_NewWord, {
    #output$E_Subject_SampleInt = sample(),
    #output$E_Object_SampleInt = sample(),
    #output$E_Formality_SampleInt = sample(),
    #output$E_Verb_SampleInt = sample()
  #})

  
  output$English = renderUI({
    HTML(text = paste(
    paste("Subject: ", subjects[E_start$Subject_Int, ]$English, sep = ""),
    br(),
    paste("Object: ", subjects[E_start$Subject_Int, ]$English, sep = ""),
    br(),
    paste("Verb: ", subjects[E_start$Subject_Int, ]$English, sep = ""),
    br(),
    paste("Tense: ", subjects[E_start$Subject_Int, ]$English, sep = ""),
    br(),
    paste("Formality: ", subjects[E_start$Subject_Int, ]$English, sep = ""),
    br(),
    paste("Context: ", subjects[E_start$Subject_Int, ]$English, sep = ""),
    br(),
    paste("Marker: ", subjects[E_start$Subject_Int, ]$English, sep = "")
    ))
  })
  
  observeEvent(input$E_NewWord, {
    output$E_Korean = NULL
  })
  
  observeEvent(input$E_NewWord, {
    output$E_Romanization = NULL
  })
  
  observeEvent(input$E_KoreanReveal, {
    output$E_Korean = renderUI({
      HTML(text = paste(
        br(),
        paste("Korean: ", subjects[E_start$Subject_Int, ]$Korean, sep = "")
      ))
    })
  })
  
  observeEvent(input$E_RomanizationReveal, {
    output$E_Romanization = renderText({
      paste("Romanization: ", sep = "")
    })
  })
  
  
  
  
  
}


shinyApp(
  ui = uix,
  server = serverx
)