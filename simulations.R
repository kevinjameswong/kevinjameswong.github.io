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

set.seed(3)

uix = dashboardPage(
  skin = "purple",
  dashboardHeader(
    title = "Trivia Data",
    disable = FALSE),  
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        text = "Analysis",
        #icon = icon(name = "brain"),
        tabName = "Analysis",
        #badgeLabel = "NEW",
        selected = TRUE)#,
      #menuItem(
      #text = "Artists",
      #icon = icon(name = "music", lib = "font-awesome"),
      #tabName = "Artists",
      #badgeLabel = "NEW")#,
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
      tabItem(tabName = "Analysis",
              
              fluidRow(
                box(width = 6,
                    numericInput(
                      inputId = "n",
                      label = "Number of Games:",
                      value = 5,
                      step = 1,
                      min = 1,
                      max = 20
                    ),
                    numericInput(
                      inputId = "p",
                      label = "Probability of Success:",
                      value = 0.5,
                      step = 0.1,
                      min = ,
                      max = 1
                    ),
                    actionButton(inputId = "Simulate_TT", label = "Simulate", width = "100%")
                ),
                
                
              ),


              fluidRow(width = 12,
                       valueBoxOutput(outputId = "count_games"),
                       valueBoxOutput(outputId = "count_wins"),
                       valueBoxOutput(outputId = "count_losses")
              ),
              fluidRow(width = 12,
                       #column(width = 6, tableOutput(outputId = "MostAsked")),
                       column(width = 12, tableOutput(outputId = "ArtistDiscopgraphy1"))
              )
      )
    )
  )
)

serverx = function(input, output, session) {
  
  
  tt_n = eventReactive(
    eventExpr = input$Simulate_TT,
    valueExpr = {c(1:input$n)},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  tt_p = eventReactive(
    eventExpr = input$Simulate_TT,
    valueExpr = {input$p * 100},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  tt_xx = eventReactive(
    eventExpr = input$Simulate_TT,
    valueExpr = {sample(
      x = c(1:100),
      size = input$n,
      replace = TRUE
    )},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  
  tt = eventReactive(
    eventExpr = input$Simulate_TT,
    valueExpr = {
      data.frame(
        Game = tt_n(),
        Score = tt_xx(),
        Result = case_when(tt_xx() >= 100 - tt_p() ~ "Win", .default = "Loss")
      )
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )

  
  output$count_games = renderValueBox({
    tt() %>%
      #filter(Result == "Win") %>%
      n_distinct() %>%
      format(nsmall = 0, big.mark = ",") %>%
      valueBox(subtitle = "Game Total", color = "blue", icon = icon(name = "fab fa-spotify"), width = 4)
  })
  
  output$count_wins = renderValueBox({
    tt() %>%
      filter(Result == "Win") %>%
      n_distinct() %>%
      format(nsmall = 0, big.mark = ",") %>%
      valueBox(subtitle = "Win Total", color = "green", icon = icon(name = "fab fa-spotify"), width = 4)
  })
  
  output$count_losses = renderValueBox({
    tt() %>%
      filter(Result == "Loss") %>%
      n_distinct() %>%
      format(nsmall = 0, big.mark = ",") %>%
      valueBox(subtitle = "Loss Total", color = "red", icon = icon(name = "fab fa-spotify"), width = 4)
  })

  
  output$ArtistDiscopgraphy1 = renderText({
     tt() %>%
      kable(format = "html", align = "ll", col.names = c("Trial", "p", "Outcome")) %>%
      kable_styling(bootstrap_options = c("hover", "responsive", "striped")) %>%
      scroll_box(width = "100%", height = "100%") #%>%
    #row_spec(2, background = "#683659")
    
  })
  

  
}







shinyApp(
  ui = uix,
  server = serverx
)