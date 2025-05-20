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

mm_picks = read.csv(file = "files/marchmadness_picks.csv")
mm_matchups = read.csv(file = "files/marchmadness_matchups.csv")

percent = function(x, digits = 2, format = "f", ...) {
  t = formatC(x * 100, format = format, digits = digits, ...) %>% paste("%", sep = "")
  return(t)
}

Points_S16 = 40
Points_E8 = 80
Points_F4 = 160
Points_Champ = 320

MAX_Year = 2025

uix = dashboardPage(
  
  skin = "blue",
  dashboardHeader(
    title = "March Madness - Kevin Wong",
    disable = FALSE),  
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        text = "Standings",
        icon = icon(name = "ranking-star"),
        tabName = "Standings",
        badgeLabel = "NEW",
        selected = TRUE
      ),
      menuItem(
        text = "Scenarios",
        icon = icon(name = "check-square"),
        tabName = "Scenarios",
        badgeLabel = "NEW",
        selected = FALSE
      ),
      menuItem(
        text = "Forecast",
        icon = icon(name = "chart-line"),
        tabName = "Forecast",
        badgeLabel = "NEW",
        selected = FALSE
      )
    )
  ),
  
  dashboardBody(
    tags$script(HTML("$('body').addClass('fixed');")),
    
    tabItems(
      
      tabItem(tabName = "Standings",
              fluidRow(
                box(
                  column(width = 12,
                         h1("Standings")
                  )
                )
              )
      ),
      
      tabItem(tabName = "Scenarios",
              fluidRow(
                box(
                  column(width = 12,
                         h2("Scenarios"),
                         selectInput(
                           inputId = "YearSelect",
                           label = "Select Year:",
                           choices = mm_matchups$Year %>% unique(),
                           selected = mm_matchups$Year %>% max()
                        ),
                        h4("Sweet 16"),
                        selectInput(
                          inputId = "Input_G01",
                          label = "Game 1 (Sweet 16)",
                          choices = c(NA, mm_matchups %>% filter(Year == MAX_Year, Game == "S16_1") %>% select(Team)),
                          selected = NA
                        ),
                        selectInput(
                          inputId = "Input_G02",
                          label = "Game 2 (Sweet 16)",
                          choices = c(NA, mm_matchups %>% filter(Year == MAX_Year, Game == "S16_2") %>% select(Team)),
                          selected = NA
                        ),
                        selectInput(
                          inputId = "Input_G03",
                          label = "Game 3 (Sweet 16)",
                          choices = c(NA, mm_matchups %>% filter(Year == MAX_Year, Game == "S16_3") %>% select(Team)),
                          selected = NA
                        ),
                        selectInput(
                          inputId = "Input_G04",
                          label = "Game 4 (Sweet 16)",
                          choices = c(NA, mm_matchups %>% filter(Year == MAX_Year, Game == "S16_4") %>% select(Team)),
                          selected = NA
                        ),
                        selectInput(
                          inputId = "Input_G05",
                          label = "Game 5 (Sweet 16)",
                          choices = c(NA, mm_matchups %>% filter(Year == MAX_Year, Game == "S16_5") %>% select(Team)),
                          selected = NA
                        ),
                        selectInput(
                          inputId = "Input_G06",
                          label = "Game 6 (Sweet 16)",
                          choices = c(NA, mm_matchups %>% filter(Year == MAX_Year, Game == "S16_6") %>% select(Team)),
                          selected = NA
                        ),
                        selectInput(
                          inputId = "Input_G07",
                          label = "Game 7 (Sweet 16)",
                          choices = c(NA, mm_matchups %>% filter(Year == MAX_Year, Game == "S16_7") %>% select(Team)),
                          selected = NA
                        ),
                        selectInput(
                          inputId = "Input_G08",
                          label = "Game 8 (Sweet 16)",
                          choices = c(NA, mm_matchups %>% filter(Year == MAX_Year, Game == "S16_8") %>% select(Team)),
                          selected = NA
                        ),
                        h4("Elite 8"),
                        selectInput(
                          inputId = "Input_G09",
                          label = "Game 9 (Elite 8)",
                          choices = c(NA, mm_matchups %>% filter(Year == MAX_Year, Game %in% c("S16_1", "S16_2")) %>% select(Team)),
                          selected = NA
                        ),
                        selectInput(
                          inputId = "Input_G10",
                          label = "Game 10 (Elite 8)",
                          choices = c(NA, mm_matchups %>% filter(Year == MAX_Year, Game %in% c("S16_3", "S16_4")) %>% select(Team)),
                          selected = NA
                        ),
                        selectInput(
                          inputId = "Input_G11",
                          label = "Game 11 (Elite 8)",
                          choices = c(NA, mm_matchups %>% filter(Year == MAX_Year, Game %in% c("S16_5", "S16_6")) %>% select(Team)),
                          selected = NA
                        ),
                        selectInput(
                          inputId = "Input_G12",
                          label = "Game 12 (Elite 8)",
                          choices = c(NA, mm_matchups %>% filter(Year == MAX_Year, Game %in% c("S16_7", "S16_8")) %>% select(Team)),
                          selected = NA
                        ),
                        h4("Final 4"),
                        selectInput(
                          inputId = "Input_G13",
                          label = "Game 13 (Final 4)",
                          choices = c(NA, mm_matchups %>% filter(Year == MAX_Year, Game %in% c("S16_1", "S16_2", "S16_3", "S16_4")) %>% select(Team)),
                          selected = NA
                        ),
                        selectInput(
                          inputId = "Input_G14",
                          label = "Game 14 (Final 4)",
                          choices = c(NA, mm_matchups %>% filter(Year == MAX_Year, Game %in% c("S16_5", "S16_6", "S16_7", "S16_8")) %>% select(Team)),
                          selected = NA
                        ),
                        h4("Championship"),
                        selectInput(
                          inputId = "Input_G15",
                          label = "Game 15 (Championship)",
                          choices = c(NA, mm_matchups %>% filter(Year == MAX_Year, Game %in% c("S16_1", "S16_2", "S16_3", "S16_4", "S16_5", "S16_6", "S16_7", "S16_8")) %>% select(Team)),
                          selected = NA
                        )
                  )
                )
              ),
              tableOutput(outputId = "all_data_XX"),
              textOutput(outputId = "Points")
              
              
      ),
      
      tabItem(tabName = "Forecast",
              fluidRow(
                box(
                  column(width = 12,
                         h2("Forecast"),
                         selectInput(
                            inputId = "YearSelect_Scenarios",
                            label = "Select Year:",
                            choices = mm_matchups$Year %>% unique(),
                            selected = mm_matchups$Year %>% max()
                          ),
                         h4("Sweet 16"),
                         selectInput(
                           inputId = "Input_G01_Scenarios",
                           label = "Game 1 (Sweet 16)",
                           choices = c(NA, mm_matchups %>% filter(Year == MAX_Year, Game == "S16_1") %>% select(Team)),
                           selected = NA
                         ),
                         selectInput(
                           inputId = "Input_G02_Scenarios",
                           label = "Game 2 (Sweet 16)",
                           choices = c(NA, mm_matchups %>% filter(Year == MAX_Year, Game == "S16_2") %>% select(Team)),
                           selected = NA
                         ),
                         selectInput(
                           inputId = "Input_G03_Scenarios",
                           label = "Game 3 (Sweet 16)",
                           choices = c(NA, mm_matchups %>% filter(Year == MAX_Year, Game == "S16_3") %>% select(Team)),
                           selected = NA
                         ),
                         selectInput(
                           inputId = "Input_G04_Scenarios",
                           label = "Game 4 (Sweet 16)",
                           choices = c(NA, mm_matchups %>% filter(Year == MAX_Year, Game == "S16_4") %>% select(Team)),
                           selected = NA
                         ),
                         selectInput(
                           inputId = "Input_G05_Scenarios",
                           label = "Game 5 (Sweet 16)",
                           choices = c(NA, mm_matchups %>% filter(Year == MAX_Year, Game == "S16_5") %>% select(Team)),
                           selected = NA
                         ),
                         selectInput(
                           inputId = "Input_G06_Scenarios",
                           label = "Game 6 (Sweet 16)",
                           choices = c(NA, mm_matchups %>% filter(Year == MAX_Year, Game == "S16_6") %>% select(Team)),
                           selected = NA
                         ),
                         selectInput(
                           inputId = "Input_G07_Scenarios",
                           label = "Game 7 (Sweet 16)",
                           choices = c(NA, mm_matchups %>% filter(Year == MAX_Year, Game == "S16_7") %>% select(Team)),
                           selected = NA
                         ),
                         selectInput(
                           inputId = "Input_G08_Scenarios",
                           label = "Game 8 (Sweet 16)",
                           choices = c(NA, mm_matchups %>% filter(Year == MAX_Year, Game == "S16_8") %>% select(Team)),
                           selected = NA
                         ),
                         h4("Elite 8"),
                         selectInput(
                           inputId = "Input_G09_Scenarios",
                           label = "Game 9 (Elite 8)",
                           choices = c(NA, mm_matchups %>% filter(Year == MAX_Year, Game %in% c("S16_1", "S16_2")) %>% select(Team)),
                           selected = NA
                         ),
                         selectInput(
                           inputId = "Input_G10_Scenarios",
                           label = "Game 10 (Elite 8)",
                           choices = c(NA, mm_matchups %>% filter(Year == MAX_Year, Game %in% c("S16_3", "S16_4")) %>% select(Team)),
                           selected = NA
                         ),
                         selectInput(
                           inputId = "Input_G11_Scenarios",
                           label = "Game 11 (Elite 8)",
                           choices = c(NA, mm_matchups %>% filter(Year == MAX_Year, Game %in% c("S16_5", "S16_6")) %>% select(Team)),
                           selected = NA
                         ),
                         selectInput(
                           inputId = "Input_G12_Scenarios",
                           label = "Game 12 (Elite 8)",
                           choices = c(NA, mm_matchups %>% filter(Year == MAX_Year, Game %in% c("S16_7", "S16_8")) %>% select(Team)),
                           selected = NA
                         ),
                         h4("Final 4"),
                         selectInput(
                           inputId = "Input_G13_Scenarios",
                           label = "Game 13 (Final 4)",
                           choices = c(NA, mm_matchups %>% filter(Year == MAX_Year, Game %in% c("S16_1", "S16_2", "S16_3", "S16_4")) %>% select(Team)),
                           selected = NA
                         ),
                         selectInput(
                           inputId = "Input_G14_Scenarios",
                           label = "Game 14 (Final 4)",
                           choices = c(NA, mm_matchups %>% filter(Year == MAX_Year, Game %in% c("S16_5", "S16_6", "S16_7", "S16_8")) %>% select(Team)),
                           selected = NA
                         ),
                         h4("Championship"),
                         selectInput(
                           inputId = "Input_G15_Scenarios",
                           label = "Game 15 (Championship)",
                           choices = c(NA, mm_matchups %>% filter(Year == MAX_Year, Game %in% c("S16_1", "S16_2", "S16_3", "S16_4", "S16_5", "S16_6", "S16_7", "S16_8")) %>% select(Team)),
                           selected = NA
                         )
                  )
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  dataTableOutput(outputId = "myeon")
                )
              )
      )
    )#tabItems
  )#dashboardBody
) #dashboardPage

serverx = function(input, output, session) {
  

  observeEvent(input$YearSelect, {
    updateSelectInput(session, inputId = "Input_G01",
                      choices = c(NA, mm_matchups %>% filter(Year == input$YearSelect, Game == "S16_1") %>% select(Team)))
  })
  observeEvent(input$YearSelect, {
    updateSelectInput(session, inputId = "Input_G02",
                      choices = c(NA, mm_matchups %>% filter(Year == input$YearSelect, Game == "S16_2") %>% select(Team)))
  })
  observeEvent(input$YearSelect, {
    updateSelectInput(session, inputId = "Input_G03",
                      choices = c(NA, mm_matchups %>% filter(Year == input$YearSelect, Game == "S16_3") %>% select(Team)))
  })
  observeEvent(input$YearSelect, {
    updateSelectInput(session, inputId = "Input_G04",
                      choices = c(NA, mm_matchups %>% filter(Year == input$YearSelect, Game == "S16_4") %>% select(Team)))
  })
  observeEvent(input$YearSelect, {
    updateSelectInput(session, inputId = "Input_G05",
                      choices = c(NA, mm_matchups %>% filter(Year == input$YearSelect, Game == "S16_5") %>% select(Team)))
  })
  observeEvent(input$YearSelect, {
    updateSelectInput(session, inputId = "Input_G06",
                      choices = c(NA, mm_matchups %>% filter(Year == input$YearSelect, Game == "S16_6") %>% select(Team)))
  })
  observeEvent(input$YearSelect, {
    updateSelectInput(session, inputId = "Input_G07",
                      choices = c(NA, mm_matchups %>% filter(Year == input$YearSelect, Game == "S16_7") %>% select(Team)))
  })
  observeEvent(input$YearSelect, {
    updateSelectInput(session, inputId = "Input_G08",
                      choices = c(NA, mm_matchups %>% filter(Year == input$YearSelect, Game == "S16_8") %>% select(Team)))
  })
  observeEvent(input$YearSelect, {
    updateSelectInput(session, inputId = "Input_G09",
                      choices = c(NA, mm_matchups %>% filter(Year == input$YearSelect, Game %in% c("S16_1", "S16_2")) %>% select(Team)))
  })
  observeEvent(input$YearSelect, {
    updateSelectInput(session, inputId = "Input_G10",
                      choices = c(NA, mm_matchups %>% filter(Year == input$YearSelect, Game %in% c("S16_3", "S16_4")) %>% select(Team)))
  })
  observeEvent(input$YearSelect, {
    updateSelectInput(session, inputId = "Input_G11",
                      choices = c(NA, mm_matchups %>% filter(Year == input$YearSelect, Game %in% c("S16_5", "S16_6")) %>% select(Team)))
  })
  observeEvent(input$YearSelect, {
    updateSelectInput(session, inputId = "Input_G12",
                      choices = c(NA, mm_matchups %>% filter(Year == input$YearSelect, Game %in% c("S16_7", "S16_8")) %>% select(Team)))
  })
  observeEvent(input$YearSelect, {
    updateSelectInput(session, inputId = "Input_G13",
                      choices = c(NA, mm_matchups %>% filter(Year == input$YearSelect, Game %in% c("S16_1", "S16_2", "S16_3", "S16_4")) %>% select(Team)))
  })
  observeEvent(input$YearSelect, {
    updateSelectInput(session, inputId = "Input_G14",
                      choices = c(NA, mm_matchups %>% filter(Year == input$YearSelect, Game %in% c("S16_5", "S16_6", "S16_7", "S16_8")) %>% select(Team)))
  })
  observeEvent(input$YearSelect, {
    updateSelectInput(session, inputId = "Input_G15",
                      choices = c(NA, mm_matchups %>% filter(Year == input$YearSelect, Game %in% c("S16_1", "S16_2", "S16_3", "S16_4", "S16_5", "S16_6", "S16_7", "S16_8")) %>% select(Team)))
  })
  
  
  Input_G01 = eventReactive(
    eventExpr = input$Input_G01,
    valueExpr = {input$Input_G01},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  Input_G02 = eventReactive(
    eventExpr = input$Input_G02,
    valueExpr = {input$Input_G02},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  Input_G03 = eventReactive(
    eventExpr = input$Input_G03,
    valueExpr = {input$Input_G03},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  Input_G04 = eventReactive(
    eventExpr = input$Input_G04,
    valueExpr = {input$Input_G04},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  Input_G05 = eventReactive(
    eventExpr = input$Input_G05,
    valueExpr = {input$Input_G05},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  Input_G06 = eventReactive(
    eventExpr = input$Input_G06,
    valueExpr = {input$Input_G06},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  Input_G07 = eventReactive(
    eventExpr = input$Input_G07,
    valueExpr = {input$Input_G07},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  Input_G08 = eventReactive(
    eventExpr = input$Input_G08,
    valueExpr = {input$Input_G08},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  Input_G09 = eventReactive(
    eventExpr = input$Input_G09,
    valueExpr = {input$Input_G09},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  Input_G10 = eventReactive(
    eventExpr = input$Input_G10,
    valueExpr = {input$Input_G10},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  Input_G11 = eventReactive(
    eventExpr = input$Input_G11,
    valueExpr = {input$Input_G11},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  Input_G12 = eventReactive(
    eventExpr = input$Input_G12,
    valueExpr = {input$Input_G12},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  Input_G13 = eventReactive(
    eventExpr = input$Input_G13,
    valueExpr = {input$Input_G13},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  Input_G14 = eventReactive(
    eventExpr = input$Input_G14,
    valueExpr = {input$Input_G14},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  Input_G15 = eventReactive(
    eventExpr = input$Input_G15,
    valueExpr = {input$Input_G15},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )

  
  output$all_data_XX = renderText(
    mm_picks %>%
      mutate(Pointss = StartingPoints
             + ifelse(test = S16_1 == Input_G01(), yes = Points_S16, no = 0)
             + ifelse(test = S16_2 == Input_G02(), yes = Points_S16, no = 0)
             + ifelse(test = S16_3 == Input_G03(), yes = Points_S16, no = 0)
             + ifelse(test = S16_4 == Input_G04(), yes = Points_S16, no = 0)
             + ifelse(test = S16_5 == Input_G05(), yes = Points_S16, no = 0)
             + ifelse(test = S16_6 == Input_G06(), yes = Points_S16, no = 0)
             + ifelse(test = S16_7 == Input_G07(), yes = Points_S16, no = 0)
             + ifelse(test = S16_8 == Input_G08(), yes = Points_S16, no = 0)
             
             + ifelse(test = E8_1 == Input_G09(), yes = Points_E8, no = 0)
             + ifelse(test = E8_2 == Input_G10(), yes = Points_E8, no = 0)
             + ifelse(test = E8_3 == Input_G11(), yes = Points_E8, no = 0)
             + ifelse(test = E8_4 == Input_G12(), yes = Points_E8, no = 0)
             
             + ifelse(test = F4_1 == Input_G13(), yes = Points_F4, no = 0)
             + ifelse(test = F4_2 == Input_G14(), yes = Points_F4, no = 0)
             
             + ifelse(test = Champion == Input_G15(), yes = Points_Champ, no = 0),
             
             MaxPointss = 999
             
             ) %>%
      filter(Year == input$YearSelect) %>%
      select(Player, Pointss, MaxPointss) %>%
      arrange(Pointss %>% desc()) %>%
      kable(format = "html", align = "ll",  col.names = c("Player", "Points", "Max Points")) %>%
      kable_styling(bootstrap_options = c("hover", "responsive", "striped")) %>%
      scroll_box(width = "100%", height = "100%")
  )
  
  
  

  
  T_1 = eventReactive(
    eventExpr = input$YearSelect_Scenarios,
    valueExpr = {mm_matchups %>% filter(Game == "S16_1", Year == input$YearSelect_Scenarios) %>% select(Team)},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  T_2 = eventReactive(
    eventExpr = input$YearSelect_Scenarios,
    valueExpr = {mm_matchups %>% filter(Game == "S16_2", Year == input$YearSelect_Scenarios) %>% select(Team)},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  T_3 = eventReactive(
    eventExpr = input$YearSelect_Scenarios,
    valueExpr = {mm_matchups %>% filter(Game == "S16_3", Year == input$YearSelect_Scenarios) %>% select(Team)},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  T_4 = eventReactive(
    eventExpr = input$YearSelect_Scenarios,
    valueExpr = {mm_matchups %>% filter(Game == "S16_4", Year == input$YearSelect_Scenarios) %>% select(Team)},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  T_5 = eventReactive(
    eventExpr = input$YearSelect_Scenarios,
    valueExpr = {mm_matchups %>% filter(Game == "S16_5", Year == input$YearSelect_Scenarios) %>% select(Team)},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  T_6 = eventReactive(
    eventExpr = input$YearSelect_Scenarios,
    valueExpr = {mm_matchups %>% filter(Game == "S16_6", Year == input$YearSelect_Scenarios) %>% select(Team)},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  T_7 = eventReactive(
    eventExpr = input$YearSelect_Scenarios,
    valueExpr = {mm_matchups %>% filter(Game == "S16_7", Year == input$YearSelect_Scenarios) %>% select(Team)},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  T_8 = eventReactive(
    eventExpr = input$YearSelect_Scenarios,
    valueExpr = {mm_matchups %>% filter(Game == "S16_8", Year == input$YearSelect_Scenarios) %>% select(Team)},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  mm_cy = eventReactive(
    eventExpr = input$YearSelect_Scenarios,
    valueExpr = {mm_picks %>% filter(Year == input$YearSelect_Scenarios)},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  T_S16 = reactiveValues(
    S16_1 = as.character(),
    S16_2 = as.character(),
    S16_3 = as.character(),
    S16_4 = as.character(),
    S16_5 = as.character(),
    S16_6 = as.character(),
    S16_7 = as.character(),
    S16_8 = as.character()
  )
  
  T_S16 = eventReactive(
    eventExpr = input$YearSelect_Scenarios,
    valueExpr = {cross_join(T_1(), T_2()) %>%
        cross_join(T_3()) %>%
        cross_join(T_4()) %>%
        cross_join(T_5()) %>%
        cross_join(T_6()) %>%
        cross_join(T_7()) %>%
        cross_join(T_8()) %>%
        rename(
          "S16_1" = Team.x,
          "S16_2" = Team.y,
          "S16_3" = Team.x.x,
          "S16_4" = Team.y.y,
          "S16_5" = Team.x.x.x,
          "S16_6" = Team.y.y.y,
          "S16_7" = Team.x.x.x.x,
          "S16_8" = Team.y.y.y.y,
        )
        
      },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  
  
  

  #Dictionary
  output$myeon = renderDataTable({
    T_S16()
  })
  
  
}




shinyApp(
  ui = uix,
  server = serverx
)