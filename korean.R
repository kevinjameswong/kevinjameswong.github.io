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
korean$Easy = ifelse(
  test = is.na(korean$Easy),
  yes = 0,
  no = 1
)
korean = korean %>%
  group_by(English, Korean, Romanization, Easy) %>%
  mutate(count = n()) %>%
  #filter(Romanization == "hwajangsil") %>%
  mutate(TT = min(Index)) %>%
  mutate(Duplicate = ifelse(TT == Index, 0, 1) %>% as.integer()) %>%
  select(English, Korean, Romanization, Easy, Index, Duplicate)# %>%
  #filter(Index %in% c(1, 3, 5, 7, 9))



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
      ),

      
      
      tabItem(tabName = "Quiz",
              
              fluidRow(
                box(width = 6,
                    selectInput(
                      inputId = "numberofquizQuestions",
                      label = "Number of Questions:",
                      choices = 1:13 %>% sort(),
                      #choices = c("2020", "2021", "2022", "2023", "2024"),
                      #selected = TopArtists_ByYear$Year %>% unique() %>% sort() %>% tail()
                      selected = 3
                    ),
                    checkboxGroupInput(
                      inputId = "questionType",
                      label = "Question Types:",
                      choiceNames = c("English to Korean", "Korean to English", "Korean to Romainzation", "Romanization to Korean"),
                      choiceValues = c(1, 2, 3, 4),
                      #selected = c(1, 2)
                      selected = c(1, 2, 3, 4)
                    ),
                    actionButton(
                      inputId = "quizStart",
                      label = "Start Quiz!"
                    ),
                    
                    p("Please wait about 5 seconds for quiz to generate. Thank you.")
                    ),
                box(width = 6,
                    br(),
                    #valueBoxOutput(outputId = "numberCorrect", width = 6))
                    textOutput(outputId = "numberCorrect"))
                #textOutput(outputId = "question_question_01x"))
              ),
              
              
              fluidRow(
                box(
                  width = 12,
                  collapsible = FALSE,
                  solidHeader = FALSE,
                  #title = "Quiz",
                  h2("Quiz"),


                  htmlOutput(outputId = "header_question_01"),
                  textOutput(outputId = "question_question_01"),
                  uiOutput(outputId = "buttons_question_01"),
                  
                  htmlOutput(outputId = "header_question_02"),
                  textOutput(outputId = "question_question_02"),
                  uiOutput(outputId = "buttons_question_02"),
                  
                  htmlOutput(outputId = "header_question_03"),
                  textOutput(outputId = "question_question_03"),
                  uiOutput(outputId = "buttons_question_03"),
                  
                  htmlOutput(outputId = "header_question_04"),
                  textOutput(outputId = "question_question_04"),
                  uiOutput(outputId = "buttons_question_04"),
                  
                  htmlOutput(outputId = "header_question_05"),
                  textOutput(outputId = "question_question_05"),
                  uiOutput(outputId = "buttons_question_05"),
                  
                  htmlOutput(outputId = "header_question_06"),
                  textOutput(outputId = "question_question_06"),
                  uiOutput(outputId = "buttons_question_06"),
                  
                  htmlOutput(outputId = "header_question_07"),
                  textOutput(outputId = "question_question_07"),
                  uiOutput(outputId = "buttons_question_07"),
                  
                  htmlOutput(outputId = "header_question_08"),
                  textOutput(outputId = "question_question_08"),
                  uiOutput(outputId = "buttons_question_08"),
                  
                  htmlOutput(outputId = "header_question_09"),
                  textOutput(outputId = "question_question_09"),
                  uiOutput(outputId = "buttons_question_09"),
                  
                  htmlOutput(outputId = "header_question_10"),
                  textOutput(outputId = "question_question_10"),
                  uiOutput(outputId = "buttons_question_10"),
                  
                  br(),
                  #h4("TTT"),
                  
                  #span(textOutput(outputId = "question_question_01x"), style = "color:red;"),
                  #h1("a"),
                  #verbatimTextOutput(outputId = "a"),
                  #h1("b"),
                  #verbatimTextOutput(outputId = "b"),
                  #h1("c"),
                  #textOutput(outputId = "c"),
                  #h1("d"),
                  #textOutput(outputId = "d"),
                  #h1("e"),
                  #textOutput(outputId = "e"),
                  #h1("f"),
                  #textOutput(outputId = "f"),
                  #h1("g"),
                  #textOutput(outputId = "g"),
                  #h1("h"),
                  #verbatimTextOutput(outputId = "numberCorrect"),
                  #h1("i"),
                  #verbatimTextOutput(outputId = "i"),
                  #paste("TTxx"),
                  
                  
                  actionButton(
                    inputId = "quizSubmit",
                    label = "Submit Quiz!"
                  )
                  
                )
              )
              
              
      )
      
      
    )))

serverx = function(input, output, session) {
  
  #English
  E_start = reactiveValues(E_NEW_Int = 325)
  
  observeEvent(input$E_NewWord, {
    E_start$E_NEW_Int =
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
  
  observeEvent(input$E_NewWord, {
    output$E_Korean = NULL
  })
  
  observeEvent(input$E_NewWord, {
    output$E_Romanization = NULL
  })
  
  output$E_English = renderText({
    paste("English: ", korean[E_start$E_NEW_Int, ]$English, sep = "")
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
      paste("English: ", korean[K_start$K_NEW_Int, ]$English, sep = "")
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
  
  
  
  
  
  
  
  
  #Question 01
  correct_answer_int_question_01 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {sample(x = korean %>% filter(Duplicate == 0) %>% group_by(Index) %>% select(Index) %>% unlist() %>% unname(), size = 1, replace = FALSE)},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  wrong_answer_int_question_01 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {sample(x = korean %>% filter(Index != correct_answer_int_question_01(), Duplicate == 0) %>% group_by(Index) %>% select(Index) %>% unlist() %>% unname(), size = 3, replace = FALSE)},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  questionType_question_01 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {sample(x = input$questionType, size = 1, replace = TRUE)[1] %>% as.integer()},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  EtoK_question_01 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {korean %>% filter(Index %in% c(correct_answer_int_question_01())) %>% group_by(English) %>% select(English) %>% unlist() %>% unname()},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  KtoE_question_01 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {korean %>% filter(Index %in% c(correct_answer_int_question_01())) %>% group_by(Korean) %>% select(Korean) %>% unlist() %>% unname()},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  KtoR_question_01 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {korean %>% filter(Index %in% c(correct_answer_int_question_01())) %>% group_by(Korean) %>% select(Korean) %>% unlist() %>% unname()},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  RtoK_question_01 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {korean %>% filter(Index %in% c(correct_answer_int_question_01())) %>% group_by(Romanization) %>% select(Romanization) %>% unlist() %>% unname()},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  EtoK_correct_answer_question_01 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {korean %>% filter(Index %in% c(correct_answer_int_question_01())) %>% group_by(Korean) %>% select(Korean) %>% unlist() %>% unname()},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  KtoE_correct_answer_question_01 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {korean %>% filter(Index %in% c(correct_answer_int_question_01())) %>% group_by(English) %>% select(English) %>% unlist() %>% unname()},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  KtoR_correct_answer_question_01 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {korean %>% filter(Index %in% c(correct_answer_int_question_01())) %>% group_by(Romanization) %>% select(Romanization) %>% unlist() %>% unname()},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  RtoK_correct_answer_question_01 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {korean %>% filter(Index %in% c(correct_answer_int_question_01())) %>% group_by(Korean) %>% select(Korean) %>% unlist() %>% unname()},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  EtoK_wrong_answer_question_01 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {korean %>% filter(Index %in% c(wrong_answer_int_question_01())) %>% group_by(Korean) %>% select(Korean) %>% unlist() %>% unname()},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  KtoE_wrong_answer_question_01 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {korean %>% filter(Index %in% c(wrong_answer_int_question_01())) %>% group_by(English) %>% select(English) %>% unlist() %>% unname()},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  KtoR_wrong_answer_question_01 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {korean %>% filter(Index %in% c(wrong_answer_int_question_01())) %>% group_by(Romanization) %>% select(Romanization) %>% unlist() %>% unname()},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  RtoK_wrong_answer_question_01 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {korean %>% filter(Index %in% c(wrong_answer_int_question_01())) %>% group_by(Korean) %>% select(Korean) %>% unlist() %>% unname()},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  all_questions_question_01 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {c(EtoK_question_01(), KtoE_question_01(), KtoR_question_01(), RtoK_question_01())},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  all_correct_answers_question_01 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {c(EtoK_correct_answer_question_01(), KtoE_correct_answer_question_01(), KtoR_correct_answer_question_01(), RtoK_correct_answer_question_01())},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  all_wrong_answers_question_01 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {list(EtoK_wrong_answer_question_01(), KtoE_wrong_answer_question_01(), KtoR_wrong_answer_question_01(), RtoK_wrong_answer_question_01())},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  choiceNames_question_01 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {c(all_correct_answers_question_01()[questionType_question_01()], all_wrong_answers_question_01()[questionType_question_01()] %>% unlist() %>% unname())},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  choiceValues_question_01 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {c(correct_answer_int_question_01(), wrong_answer_int_question_01())},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  df_question_01 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {
      data.frame(
        choiceN = choiceNames_question_01(),
        choiceV = choiceValues_question_01(),
        orig_sort = 1:4,
        random = sample(x = 1:4, size = 4, replace = FALSE),
        isRight = ifelse(test = choiceValues_question_01() == correct_answer_int_question_01(), yes = 1, no = 0)
      )
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  df_question_01_randomsort = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {df_question_01()[order(df_question_01()$random, decreasing = FALSE), ]},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  question_question_01 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {all_questions_question_01()[questionType_question_01()]},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  observeEvent(input$quizStart, {
    output$buttons_question_01 = renderUI({
      radioButtons(
        inputId = "submission_question_01",
        label = question_question_01(),
        choiceNames = df_question_01_randomsort()$choiceN,
        choiceValues = df_question_01_randomsort()$choiceV,
        selected = character(0)
        #) %>% div(style = "color:black; font-size:120%; background-color: white;")
      ) %>% div(style = "font-size:120%;")
    })
  })
  
  E_start_01 = reactiveValues(submission_question_01 = NULL)
  observeEvent(input$quizStart, {
    E_start_01$submission_question_01 = NULL
  })
  observeEvent(input$quizSubmit, {
    E_start_01$submission_question_01 = input$submission_question_01
  })

  header_question_01 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {ifelse(
      test = input$numberofquizQuestions >= 1,
      yes = "Question #1",
      no = ""
    )},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )

  output$header_question_01 = renderText({
    ifelse(
      test = is.null(x = E_start_01$submission_question_01),
      yes = paste("</br><h4 style=\"background-color:bluex;\"><h4>", header_question_01(), "</h4>", sep = ""),
      no = ifelse(
        test = E_start_01$submission_question_01 == correct_answer_int_question_01(),
        yes = paste("</br><h4><span style=\"background-color:#027A54\";>", header_question_01(), "</span></h4>", sep = ""),
        no = paste("</br><h4><span style=\"background-color:#C82020;\">", header_question_01(), "</span></h4>", sep = "")
      )
    )
  })
  
  
  #Question 02
  correct_answer_int_question_02 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {sample(x = korean %>% filter(Duplicate == 0) %>% group_by(Index) %>% select(Index) %>% unlist() %>% unname(), size = 1, replace = FALSE)},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  wrong_answer_int_question_02 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {sample(x = korean %>% filter(Index != correct_answer_int_question_02(), Duplicate == 0) %>% group_by(Index) %>% select(Index) %>% unlist() %>% unname(), size = 3, replace = FALSE)},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  questionType_question_02 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {sample(x = input$questionType, size = 1, replace = TRUE)[1] %>% as.integer()},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  EtoK_question_02 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {korean %>% filter(Index %in% c(correct_answer_int_question_02())) %>% group_by(English) %>% select(English) %>% unlist() %>% unname()},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  KtoE_question_02 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {korean %>% filter(Index %in% c(correct_answer_int_question_02())) %>% group_by(Korean) %>% select(Korean) %>% unlist() %>% unname()},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  KtoR_question_02 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {korean %>% filter(Index %in% c(correct_answer_int_question_02())) %>% group_by(Korean) %>% select(Korean) %>% unlist() %>% unname()},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  RtoK_question_02 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {korean %>% filter(Index %in% c(correct_answer_int_question_02())) %>% group_by(Romanization) %>% select(Romanization) %>% unlist() %>% unname()},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  EtoK_correct_answer_question_02 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {korean %>% filter(Index %in% c(correct_answer_int_question_02())) %>% group_by(Korean) %>% select(Korean) %>% unlist() %>% unname()},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  KtoE_correct_answer_question_02 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {korean %>% filter(Index %in% c(correct_answer_int_question_02())) %>% group_by(English) %>% select(English) %>% unlist() %>% unname()},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  KtoR_correct_answer_question_02 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {korean %>% filter(Index %in% c(correct_answer_int_question_02())) %>% group_by(Romanization) %>% select(Romanization) %>% unlist() %>% unname()},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  RtoK_correct_answer_question_02 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {korean %>% filter(Index %in% c(correct_answer_int_question_02())) %>% group_by(Korean) %>% select(Korean) %>% unlist() %>% unname()},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  EtoK_wrong_answer_question_02 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {korean %>% filter(Index %in% c(wrong_answer_int_question_02())) %>% group_by(Korean) %>% select(Korean) %>% unlist() %>% unname()},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  KtoE_wrong_answer_question_02 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {korean %>% filter(Index %in% c(wrong_answer_int_question_02())) %>% group_by(English) %>% select(English) %>% unlist() %>% unname()},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  KtoR_wrong_answer_question_02 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {korean %>% filter(Index %in% c(wrong_answer_int_question_02())) %>% group_by(Romanization) %>% select(Romanization) %>% unlist() %>% unname()},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  RtoK_wrong_answer_question_02 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {korean %>% filter(Index %in% c(wrong_answer_int_question_02())) %>% group_by(Korean) %>% select(Korean) %>% unlist() %>% unname()},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  all_questions_question_02 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {c(EtoK_question_02(), KtoE_question_02(), KtoR_question_02(), RtoK_question_02())},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  all_correct_answers_question_02 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {c(EtoK_correct_answer_question_02(), KtoE_correct_answer_question_02(), KtoR_correct_answer_question_02(), RtoK_correct_answer_question_02())},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  all_wrong_answers_question_02 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {list(EtoK_wrong_answer_question_02(), KtoE_wrong_answer_question_02(), KtoR_wrong_answer_question_02(), RtoK_wrong_answer_question_02())},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  choiceNames_question_02 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {c(all_correct_answers_question_02()[questionType_question_02()], all_wrong_answers_question_02()[questionType_question_02()] %>% unlist() %>% unname())},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  choiceValues_question_02 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {c(correct_answer_int_question_02(), wrong_answer_int_question_02())},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  df_question_02 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {
      data.frame(
        choiceN = choiceNames_question_02(),
        choiceV = choiceValues_question_02(),
        orig_sort = 1:4,
        random = sample(x = 1:4, size = 4, replace = FALSE),
        isRight = ifelse(test = choiceValues_question_02() == correct_answer_int_question_02(), yes = 1, no = 0)
      )
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  df_question_02_randomsort = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {df_question_02()[order(df_question_02()$random, decreasing = FALSE), ]},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  question_question_02 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {all_questions_question_02()[questionType_question_02()]},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  observeEvent(input$quizStart, {
    output$buttons_question_02 = renderUI({
      radioButtons(
        inputId = "submission_question_02",
        label = question_question_02(),
        choiceNames = df_question_02_randomsort()$choiceN,
        choiceValues = df_question_02_randomsort()$choiceV,
        selected = character(0)
        #) %>% div(style = "color:black; font-size:120%; background-color: white;")
      ) %>% div(style = "font-size:120%;")
    })
  })
  
  E_start_02 = reactiveValues(submission_question_02 = NULL)
  observeEvent(input$quizStart, {
    E_start_02$submission_question_02 = NULL
  })
  observeEvent(input$quizSubmit, {
    E_start_02$submission_question_02 = input$submission_question_02
  })
  
  header_question_02 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {ifelse(
      test = input$numberofquizQuestions >= 2,
      yes = "Question #2",
      no = ""
    )},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  output$header_question_02 = renderText({
    ifelse(
      test = is.null(x = E_start_02$submission_question_02),
      yes = paste("</br><h4 style=\"background-color:bluex;\"><h4>", header_question_02(), "</h4>", sep = ""),
      no = ifelse(
        test = E_start_02$submission_question_02 == correct_answer_int_question_02(),
        yes = paste("</br><h4><span style=\"background-color:#027A54\";>", header_question_02(), "</span></h4>", sep = ""),
        no = paste("</br><h4><span style=\"background-color:#C82020;\">", header_question_02(), "</span></h4>", sep = "")
      )
    )
  })
  
  
  
  #Question 03
  correct_answer_int_question_03 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {sample(x = korean %>% filter(Duplicate == 0) %>% group_by(Index) %>% select(Index) %>% unlist() %>% unname(), size = 1, replace = FALSE)},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  wrong_answer_int_question_03 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {sample(x = korean %>% filter(Index != correct_answer_int_question_03(), Duplicate == 0) %>% group_by(Index) %>% select(Index) %>% unlist() %>% unname(), size = 3, replace = FALSE)},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  questionType_question_03 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {sample(x = input$questionType, size = 1, replace = TRUE)[1] %>% as.integer()},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  EtoK_question_03 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {korean %>% filter(Index %in% c(correct_answer_int_question_03())) %>% group_by(English) %>% select(English) %>% unlist() %>% unname()},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  KtoE_question_03 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {korean %>% filter(Index %in% c(correct_answer_int_question_03())) %>% group_by(Korean) %>% select(Korean) %>% unlist() %>% unname()},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  KtoR_question_03 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {korean %>% filter(Index %in% c(correct_answer_int_question_03())) %>% group_by(Korean) %>% select(Korean) %>% unlist() %>% unname()},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  RtoK_question_03 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {korean %>% filter(Index %in% c(correct_answer_int_question_03())) %>% group_by(Romanization) %>% select(Romanization) %>% unlist() %>% unname()},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  EtoK_correct_answer_question_03 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {korean %>% filter(Index %in% c(correct_answer_int_question_03())) %>% group_by(Korean) %>% select(Korean) %>% unlist() %>% unname()},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  KtoE_correct_answer_question_03 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {korean %>% filter(Index %in% c(correct_answer_int_question_03())) %>% group_by(English) %>% select(English) %>% unlist() %>% unname()},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  KtoR_correct_answer_question_03 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {korean %>% filter(Index %in% c(correct_answer_int_question_03())) %>% group_by(Romanization) %>% select(Romanization) %>% unlist() %>% unname()},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  RtoK_correct_answer_question_03 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {korean %>% filter(Index %in% c(correct_answer_int_question_03())) %>% group_by(Korean) %>% select(Korean) %>% unlist() %>% unname()},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  EtoK_wrong_answer_question_03 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {korean %>% filter(Index %in% c(wrong_answer_int_question_03())) %>% group_by(Korean) %>% select(Korean) %>% unlist() %>% unname()},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  KtoE_wrong_answer_question_03 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {korean %>% filter(Index %in% c(wrong_answer_int_question_03())) %>% group_by(English) %>% select(English) %>% unlist() %>% unname()},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  KtoR_wrong_answer_question_03 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {korean %>% filter(Index %in% c(wrong_answer_int_question_03())) %>% group_by(Romanization) %>% select(Romanization) %>% unlist() %>% unname()},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  RtoK_wrong_answer_question_03 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {korean %>% filter(Index %in% c(wrong_answer_int_question_03())) %>% group_by(Korean) %>% select(Korean) %>% unlist() %>% unname()},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  all_questions_question_03 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {c(EtoK_question_03(), KtoE_question_03(), KtoR_question_03(), RtoK_question_03())},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  all_correct_answers_question_03 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {c(EtoK_correct_answer_question_03(), KtoE_correct_answer_question_03(), KtoR_correct_answer_question_03(), RtoK_correct_answer_question_03())},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  all_wrong_answers_question_03 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {list(EtoK_wrong_answer_question_03(), KtoE_wrong_answer_question_03(), KtoR_wrong_answer_question_03(), RtoK_wrong_answer_question_03())},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  choiceNames_question_03 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {c(all_correct_answers_question_03()[questionType_question_03()], all_wrong_answers_question_03()[questionType_question_03()] %>% unlist() %>% unname())},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  choiceValues_question_03 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {c(correct_answer_int_question_03(), wrong_answer_int_question_03())},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  df_question_03 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {
      data.frame(
        choiceN = choiceNames_question_03(),
        choiceV = choiceValues_question_03(),
        orig_sort = 1:4,
        random = sample(x = 1:4, size = 4, replace = FALSE),
        isRight = ifelse(test = choiceValues_question_03() == correct_answer_int_question_03(), yes = 1, no = 0)
      )
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  df_question_03_randomsort = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {df_question_03()[order(df_question_03()$random, decreasing = FALSE), ]},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  question_question_03 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {all_questions_question_03()[questionType_question_03()]},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  observeEvent(input$quizStart, {
    output$buttons_question_03 = renderUI({
      radioButtons(
        inputId = "submission_question_03",
        label = question_question_03(),
        choiceNames = df_question_03_randomsort()$choiceN,
        choiceValues = df_question_03_randomsort()$choiceV,
        selected = character(0)
        #) %>% div(style = "color:black; font-size:120%; background-color: white;")
      ) %>% div(style = "font-size:120%;")
    })
  })
  
  E_start_03 = reactiveValues(submission_question_03 = NULL)
  observeEvent(input$quizStart, {
    E_start_03$submission_question_03 = NULL
  })
  observeEvent(input$quizSubmit, {
    E_start_03$submission_question_03 = input$submission_question_03
  })
  
  header_question_03 = eventReactive(
    eventExpr = input$quizStart,
    valueExpr = {ifelse(
      test = input$numberofquizQuestions >= 3,
      yes = "Question #3",
      no = ""
    )},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  output$header_question_03 = renderText({
    ifelse(
      test = is.null(x = E_start_03$submission_question_03),
      yes = paste("</br><h4 style=\"background-color:bluex;\"><h4>", header_question_03(), "</h4>", sep = ""),
      no = ifelse(
        test = E_start_03$submission_question_03 == correct_answer_int_question_03(),
        yes = paste("</br><h4><span style=\"background-color:#027A54\";>", header_question_03(), "</span></h4>", sep = ""),
        no = paste("</br><h4><span style=\"background-color:#C82020;\">", header_question_03(), "</span></h4>", sep = "")
      )
    )
  })

  outcome_question_01 = eventReactive(
    eventExpr = input$quizSubmit,
    valueExpr = {ifelse(test = E_start_01$submission_question_01 == correct_answer_int_question_01(), yes = 1, no = 0)},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  outcome_question_02 = eventReactive(
    eventExpr = input$quizSubmit,
    valueExpr = {ifelse(test = E_start_02$submission_question_02 == correct_answer_int_question_02(), yes = 1, no = 0)},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  outcome_question_03 = eventReactive(
    eventExpr = input$quizSubmit,
    valueExpr = {ifelse(test = E_start_03$submission_question_03 == correct_answer_int_question_03(), yes = 1, no = 0)},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  output$outcome_question_01 = renderPrint({
    outcome_question_01()
  })
  output$outcome_question_02 = renderPrint({
    outcome_question_02()
  })
  output$outcome_question_03 = renderPrint({
    outcome_question_03()
  })
  
  numberCorrect = eventReactive(
    eventExpr = input$quizSubmit,
    valueExpr = {c(outcome_question_01(), outcome_question_02(), outcome_question_03()) %>% sum()},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  observeEvent(input$quizSubmit, {
    
    showModal(ui = modalDialog(
      title = "Quiz Results",
      footer = modalButton(label = "Dismiss"),
      size = c("xl"),
      paste("You scored ", numberCorrect(), "/", input$numberofquizQuestions, ", ", round(x = numberCorrect() * 100 / as.numeric(input$numberofquizQuestions), digits = 2), "%. Great job!", sep = ""),
      easyClose = TRUE
    ))
    
  })

  

  

}













shinyApp(
  ui = uix,
  server = serverx
)