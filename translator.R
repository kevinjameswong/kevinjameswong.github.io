library(dplyr)
library(gghighlight)
library(ggplot2)
library(googlesheets4)
library(kableExtra)
library(knitr)
library(lubridate)
library(rlang)
library(scales)
library(shiny)
library(shinydashboard)
library(stringr)
library(tidyverse)

library(randomForest)
library(generics)
library(caret)

gs4_deauth()

percent = function(x, digits = 2, format = "f", ...) {
  t = formatC(x * 100, format = format, digits = digits, ...) %>% paste("%", sep = "")
  return(t)
}

decimal = function(x) {
  t = sub(x = x, pattern = "%", replacement = "") %>% as.numeric() / 100
  return(t)
}



subjects = read.csv(file = "files/korean_subjects.csv") %>%
  filter(English != '')
subjects$Index = subjects %>% nrow() %>% seq.int()



gs4_deauth()

Subjects = read_sheet(ss = "https://docs.google.com/spreadsheets/d/1V9h6qJsIR-VJIBoznJSYKMmJxKG1Swd_ovl6v9LPS9Y", sheet = "Subjects")


set.seed(seed = seed)





#https://mastering-shiny.org/basic-ui.html

#selectInput = dropdown
#sliderInput = slider
#textInput
#passwordInput
#textAreaInput
#numericInput("num", "Number one", value = 0, min = 0, max = 100),
#dateInput("dob", "When were you born?"),
#dateRangeInput

#navbarMenu is a navbar, except with multiple options in a dropdown. example is "Data Analysis Projects"

uix = dashboardPage(
  skin = "black",
  dashboardHeader(
    title = "Korean Translator",
    disable = FALSE),  
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        text = "Game",
        #icon = icon(name = "brain"),
        tabName = "Game",
        badgeLabel = "NEW",
        selected = TRUE)
      #menuItem(
        #text = "Question List",
        #icon = icon(name = "music", lib = "font-awesome"),
        #tabName = "Questions",
        #badgeLabel = "NEW",
        #selected = FALSE)#,
      
      
      #menuItem(
      #text = "Listening Statistics", #view most songs by artist in single year (top 5), highest score in a single year (top 5), most songs all time, highest score all time, most years appearing in spotify wrapped
      #icon = icon(name = "th"),
      #tabName = "Records",
      #badgeLabel = "NEW")
    )
  ),
  
  dashboardBody(
    #tags$head(
    #tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    #),
    tags$script(HTML("$('body').addClass('fixed');")),
    
    tabItems(
      # First tab content
      tabItem(tabName = "Game",
              
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
            
              
      ),
      
      
      
      
      )
    )
)

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