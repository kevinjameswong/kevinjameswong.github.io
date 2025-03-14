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
  select(English, Korean, Romanization, Easy, Index, Duplicate) %>%
  ungroup()# %>%
  #filter(Index %in% c(1, 3, 5, 7, 9))


percent = function(x, digits = 2, format = "f", ...) {
  t = formatC(x * 100, format = format, digits = digits, ...) %>% paste("%", sep = "")
  return(t)
}


korean_duo = read.csv(file = "files/duolingo_korean.csv") %>%
  select(Korean, Confirmed.Duolingo.Romanization) %>%
  unique() %>%
  filter(Korean != "")
korean_duo$Index = korean_duo %>% nrow() %>% seq.int()
names(korean_duo) = c("Korean", "Romanization", "Index")




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
                    #fluidRow(
                    checkboxGroupInput(
                      inputId = "questionType",
                      label = "Question Types:",
                      choiceNames = c("English to Korean", "Korean to English", "Korean to Romainzation", "Romanization to Korean"),
                      choiceValues = c(1, 2, 3, 4),
                      #selected = c(1, 2)
                      selected = c(1, 2, 3, 4)
                    ),
                    #checkboxGroupInput(
                      #inputId = "questionTypeX",
                      #label = "Question Difficulty:",
                      #choiceNames = c("Easy", "Hard"),
                      #choiceValues = c(1, 2),
                      #selected = c(1, 2)
                      #selected = c(1, 2)
                    #)
                    #),
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
              
              
      ),
      
      tabItem(
        tabName = "Dictionary",
        fluidRow(
          box(width = 12,
              dataTableOutput(outputId = "Diction")
            )
          )
      ),
      tabItem(
        tabName = "Romanization",
        fluidRow(
          box(width = 6,
              
             column(width = 6,
                    textOutput(outputId = "R_FR_Korean") %>% span(style = "font-size:30px;"),
                    textInput(inputId = "R_FR_TextInput",
                              label = "Romanization:",
                              value = "",
                              width = "100%",
                              placeholder = "Answer"),
             actionButton(inputId = "R_FR_Submit", label = "Submit", style = "color: #FFFFFF; background-color: #8FC8DC; border-color: #000000; font-size: 100%")
             #actionButton(inputId = "R_FR_Next", label = "Next", style = "color: #FFFFFF; background-color: #8FC8DC; border-color: #000000; font-size: 100%")
        )),
          box(width = 6,
              h4("Total Score"),
              column(width = 12,
                    valueBoxOutput(outputId = "R_submission_written_score"),
                    valueBoxOutput(outputId = "R_submission_written_testnumber"),
                    actionButton(inputId = "R_FR_ClearScore", label = "Reset Score", style = "color: #FFFFFF; background-color: #8FC8DC; border-color: #000000; font-size: 100%")
                ))
        ),
        fluidRow(
          box(width = 12,
              h4("Romanization History"),
              tableOutput(outputId = "all_data_XX")
              #textOutput(outputId = "R_submission_written_start"),
              #textOutput(outputId = "R_submission_written_correctanswer")
          )
        )
      )
      
      
    )
    )
  ) #dashboardPage

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
      yes = paste("</br><h4 style=\"background-color:bluex;\">", header_question_01(), "</h4>", sep = ""),
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
  
  
  
  
  
  
  
  
  #Romanization Free Response
  #R_submission_written_start = reactiveValues(R_NEW_INTMORE = sample(x = korean_duo %>% group_by(Index) %>% select(Index) %>% unlist() %>% unname(), size = 1, replace = TRUE))
  R_submission_written_start = reactiveValues(R_NEW_INTMORE = 1)
  CCC = reactiveValues(CC = "Korean: 시작")
  R_submission_written_testnumber = reactiveValues(start = 0)
  R_submission_written_score = reactiveValues(start = 0)
  R_submission_written_correctanswer = reactiveValues(start = "sijag")
  
  rv = reactiveValues(
    df = data.frame(
      QuestionNumber  = as.integer(character()),
      Korean          = as.character(character()),
      CorrectAnswer   = as.character(character()),
      SubmittedAnswer = as.character(character()),
      TotalCorrect    = as.integer(character()),
      OverallPercentage = as.integer(character())
    )
  )
  
  #R_submission_written_start$R_NEW_INTMORE = eventReactive(
  #eventExpr = input$R_FR_Submit,
  #valueExpr = {sample(x = korean_duo %>% group_by(Index) %>% select(Index) %>% unlist() %>% unname(), size = 1, replace = TRUE)},
  #ignoreNULL = TRUE,
  #ignoreInit = TRUE
  #)
  


  observeEvent(input$R_FR_Submit, {
    R_submission_written_testnumber$start = R_submission_written_testnumber$start + 1
  })
  
  observeEvent(input$R_FR_Submit, {
    R_submission_written_score$start = R_submission_written_score$start + ifelse(test = tolower(input$R_FR_TextInput) == tolower(R_submission_written_correctanswer$start), yes = 1, no = 0)
  })
  


 
  #R_submission_written_correctanswer$start = eventReactive(
    #eventExpr = input$R_FR_Submit,
    #valueExpr = {korean_duo[R_submission_written_start$R_NEW_INTMORE(), ]$Romanization},
    #ignoreNULL = TRUE,
    #ignoreInit = TRUE
  #)
  
  #R_submission_written_score$start = eventReactive(
    #eventExpr = input$R_FR_Submit,
    #valueExpr = {R_submission_written_score$start + ifelse(test = input$R_FR_TextInput == R_submission_written_correctanswer$start(), yes = 1, no = 0)},
    #ignoreNULL = TRUE,
    #ignoreInit = TRUE
  #)

  #R_submission_written_testnumber = eventReactive(
    #eventExpr = input$R_FR_Submit,
    #valueExpr = {R_submission_written_testnumber() + 1},
    #ignoreNULL = TRUE,
    #ignoreInit = TRUE
  #)

  
  observeEvent(input$R_FR_Submit, {
    updateTextInput(session = session, inputId = "R_FR_TextInput", value = "")
  })
  
  #observeEvent(input$R_FR_Submit, {
    #input$R_FR_TextInput = NULL
  #})
  

  #mydata = eventReactive(
    #eventExpr = input$R_FR_Submit,
    #valueExpr = {rbind(mydata, TT = 1)},
    #ignoreNULL = TRUE,
    #ignoreInit = TRUE
  #)
  
  #E_start_01 = reactiveValues(submission_question_01 = NULL)
  #observeEvent(input$quizStart, {
    #E_start_01$submission_question_01 = NULL
  #})
  
  observeEvent(input$R_FR_Submit, {
    rv$df = rbind(rv$df,
                  data.frame(
                    QuestionNumber = R_submission_written_testnumber$start,
                    Korean         = korean_duo[R_submission_written_start$R_NEW_INTMORE, ]$Korean,
                    CorrectAnswer  = R_submission_written_correctanswer$start,
                    SubmittedAnswer = input$R_FR_TextInput,
                    TotalCorrect   = R_submission_written_score$start,
                    OverallPercentage = (R_submission_written_score$start / R_submission_written_testnumber$start) %>% percent()
                  )
    )
  })
  

  
  observeEvent(input$R_FR_Submit, {
    R_submission_written_start$R_NEW_INTMORE = sample(x = korean_duo %>% group_by(Index) %>% select(Index) %>% unlist() %>% unname(), size = 1, replace = TRUE)
  })
  
  observeEvent(input$R_FR_Submit, {
    R_submission_written_correctanswer$start = korean_duo[R_submission_written_start$R_NEW_INTMORE, ]$Romanization
  })
  
  #R_FR_Korean = eventReactive(
    #eventExpr = input$R_FR_Submit,
    #valueExpr = {paste("Korean: ", korean_duo[R_submission_written_start$R_NEW_INTMORE, ]$Korean, sep = "")},
    #ignoreNULL = TRUE,
    #ignoreInit = TRUE
  #)
  
  observeEvent(input$R_FR_Submit, {
    CCC$CC = paste("Korean: ", korean_duo[R_submission_written_start$R_NEW_INTMORE, ]$Korean, sep = "")
  })
  
  
  output$all_data_XX = renderText(
    rv$df %>%
      arrange(QuestionNumber %>% desc()) %>%
      kable(format = "html", align = "llllll", col.names = c("Question", "Korean", "Correct Answer", "Submitted Answer", "Total Correct", "% Correct")) %>%
      kable_styling(bootstrap_options = c("hover", "responsive", "striped")) %>%
      scroll_box(width = "100%", height = "100%")
  )
  
  output$R_FR_Korean = renderText({
    CCC$CC
  })
  
  output$R_submission_written_score = renderValueBox({
    R_submission_written_score$start %>%
      format(nsmall = 0, big.mark = ",") %>%
      valueBox(subtitle = "Total Correct", color = "green", icon = icon(name = "check"), width = 3)
  })
  
  output$R_submission_written_testnumber = renderValueBox({
    R_submission_written_testnumber$start %>%
      format(nsmall = 0, big.mark = ",") %>%
      valueBox(subtitle = "Total Questions", color = "blue", icon = icon(name = "fab fa-question"), width = 3)
  })
  
  output$R_submission_written_start = renderText({
    R_submission_written_start$R_NEW_INTMORE
  })
  
  output$R_submission_written_correctanswer = renderText({
    R_submission_written_correctanswer$start
  })
  
  
  observeEvent(input$R_FR_ClearScore, {
    R_submission_written_score$start = 0
  })
  
  observeEvent(input$R_FR_ClearScore, {
    R_submission_written_testnumber$start = 0
  })
  
  observeEvent(input$R_FR_ClearScore, {
    rv$df = data.frame(
      QuestionNumber  = as.integer(character()),
      Korean          = as.character(character()),
      CorrectAnswer   = as.character(character()),
      SubmittedAnswer = as.character(character()),
      TotalCorrect    = as.integer(character()),
      OverallPercentage = as.integer(character())
    )
  })
  

  
  
  
  #Dictionary
  output$Diction = renderDataTable({
    korean %>%
      #filter(Index <= 30) %>%
      select(English, Korean, Romanization)
  })

  #R_FR_ClearScore
  #R_FR_Submit
  #R_FR_Next
  #R_FR_TextInput
  
}

  #eventRactive = update value
  #observeEvent = make something happen



shinyApp(
  ui = uix,
  server = serverx
)