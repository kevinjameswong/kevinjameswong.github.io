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


#https://mastering-shiny.org/basic-ui.html
#https://bookdown.org/loankimrobinson/rshinybook/stock-front-footer.html#stock-front-footer

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
  skin = "red",
  dashboardHeader(
    title = "Calculators",
    disable = FALSE),  
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        text = "Binomial Distribution",
        icon = icon(name = "calculator"),
        tabName = "Binomial",
        badgeLabel = "NEW",
        selected = TRUE),
      menuItem(
        text = "Normal Distrubtion",
        icon = icon(name = "calculator"),
        tabName = "Normal",
        badgeLabel = "NEW",
        selected = FALSE)
    )
  ),
  
  dashboardBody(

    tabItems(
      # First tab content
      tabItem(tabName = "Binomial",
              fluidRow(width = 12,
                box(title = "Input", width = 2, #background = "gray",
                column(width = 12,
                       numericInput(
                         inputId = "binom_p",
                         label = "Probability:",
                         value = 0.5,
                         min = 0,
                         max = 1,
                         step = 0.1
                       ),
                       numericInput(
                         inputId = "binom_n",
                         label = "Attempts:",
                         value = 10,
                         min = 0,
                         step = 1
                       ),
                       numericInput(
                         inputId = "binom_k",
                         label = "Successes:",
                         value = 3,
                         min = 0,
                         step = 1
                       ),
                       actionButton(inputId = "binom_update", label = "Update", width = "100%")
                       )
                ),
                box(title = "Output", width = 10,
                  valueBoxOutput("binom_p"),
                  valueBoxOutput("binom_n"),
                  valueBoxOutput("binom_k"),
                  valueBoxOutput("binom_ExpectedOutcome"),
                  valueBoxOutput("binom_LessThankSuccesses"),
                  valueBoxOutput("binom_AtMostkSuccesses"),
                  valueBoxOutput("binom_ExactlykSuccesses"),
                  valueBoxOutput("binom_GreaterThankSuccesses"),
                  valueBoxOutput("binom_AtLeastkSuccesses"),
                  )
                

              #actionButton("clear", "Update")
      )#,
      #fluidRow(width = 5,
         #      "TT")
      ),

      # Second tab content
      tabItem(tabName = "Normal",
              #br(), #"Yoing",
              fluidRow(width = 12,
                       box(title = "Input", width = 2, #background = "gray",
                           column(width = 12,
                                  numericInput(
                                    inputId = "normal_mean",
                                    label = "Mean:",
                                    value = 10,
                                    step = 0.1
                                  ),
                                  numericInput(
                                    inputId = "normal_sd",
                                    label = "Standard Deviation:",
                                    value = 1,
                                    step = 1
                                  ),
                                  numericInput(
                                    inputId = "normal_x",
                                    label = "Target:",
                                    value = 11,
                                    step = 1
                                  ),

                                  actionButton(inputId = "normal_update", label = "Update", width = "100%")
                           )
                       ),
                       box(title = "Output", width = 10,
                           valueBoxOutput("normal_mean"),
                           valueBoxOutput("normal_sd"),
                           valueBoxOutput("normal_x"),
                           valueBoxOutput("normal_LessThanx"),
                           valueBoxOutput("normal_GreaterThanx"),

                       )
                       
                       
                       #actionButton("clear", "Update")
              )
              #,fluidRow(width = 5,"TT")
              
      )


  ))
)

serverx = function(input, output, session) {

  
  binom_p = eventReactive(
    eventExpr = input$binom_update,
    valueExpr = {input$binom_p * 100},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  binom_n = eventReactive(
    eventExpr = input$binom_update,
    valueExpr = {input$binom_n},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  binom_k = eventReactive(
    eventExpr = input$binom_update,
    valueExpr = {input$binom_k},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  binom_ExpectedOutcome = eventReactive(
    eventExpr = input$binom_update,
    valueExpr = {input$binom_p * input$binom_n},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  binom_Prob_LessThan = eventReactive(
    eventExpr = input$binom_update,
    valueExpr = {pbinom(q = input$binom_k - 1, size = input$binom_n, prob = input$binom_p) * 100},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  binom_Prob_AtMost = eventReactive(
    eventExpr = input$binom_update,
    valueExpr = {pbinom(q = input$binom_k, size = input$binom_n, prob = input$binom_p) * 100},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )

  binom_Prob_Exactly = eventReactive(
    eventExpr = input$binom_update,
    valueExpr = {(pbinom(q = input$binom_k, size = input$binom_n, prob = input$binom_p) - pbinom(q = input$binom_k - 1, size = input$binom_n, prob = input$binom_p)) * 100},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  binom_Prob_GreaterThan = eventReactive(
    eventExpr = input$binom_update,
    valueExpr = {(1 - pbinom(q = input$binom_k, size = input$binom_n, prob = input$binom_p)) * 100},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  binom_Prob_AtLeast = eventReactive(
    eventExpr = input$binom_update,
    valueExpr = {(1 - pbinom(q = input$binom_k - 1, size = input$binom_n, prob = input$binom_p)) * 100},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )

  output$binom_p = renderValueBox({
    binom_p() %>%
      percent() %>%
      valueBox(subtitle = "Probability of Each Success", icon = icon(name = "p"), width = 4)
  })
  
  output$binom_n = renderValueBox({
    binom_n() %>%
      format(nsmall = 0, big.mark = ",") %>%
      valueBox(subtitle = "Number of Attempts", icon = icon(name = "n"), width = 4)
  })
  
  output$binom_k = renderValueBox({
    binom_k() %>%
      format(nsmall = 0, big.mark = ",") %>%
      valueBox(subtitle = "Number of Successes", icon = icon(name = "k"), width = 4)
  })
  
  output$binom_ExpectedOutcome = renderValueBox({
    binom_ExpectedOutcome() %>%
      valueBox(subtitle = "Expected Outcome", color = "light-blue", width = 4)
  })
  
  output$binom_LessThankSuccesses = renderValueBox({
    binom_Prob_LessThan() %>%
      percent() %>%
      valueBox(subtitle = paste("Less than ", input$binom_k, " Successes", sep = ""), color = "green", icon = icon(name = "less-than"), width = 4)
  })
  
  output$binom_AtMostkSuccesses = renderValueBox({
    binom_Prob_AtMost() %>%
      percent() %>%
      valueBox(subtitle = paste("At Most ", input$binom_k, " Successes", sep = ""), color = "green", icon = icon(name = "less-than-equal"), width = 4)
  })
  
  output$binom_ExactlykSuccesses = renderValueBox({
    binom_Prob_Exactly() %>%
      percent() %>%
      valueBox(subtitle = paste("Exactly ", input$binom_k, " Successes", sep = ""), color = "light-blue", icon = icon(name = "equals"), width = 4)
  })
  
  output$binom_GreaterThankSuccesses = renderValueBox({
    binom_Prob_GreaterThan() %>%
      percent() %>%
      valueBox(subtitle = paste("Greater than ", input$binom_k, " Successes", sep = ""), color = "yellow", icon = icon(name = "greater-than"), width = 4)
  })
  
  output$binom_AtLeastkSuccesses = renderValueBox({
    binom_Prob_AtLeast() %>%
      percent() %>%
      valueBox(subtitle = paste("At Least ", input$binom_k, " Successes", sep = ""), color = "yellow", icon = icon(name = "greater-than-equal"), width = 4)
  })
  
  
  
  
  
  
  
  
  
  
  normal_mean = eventReactive(
    eventExpr = input$normal_update,
    valueExpr = {input$normal_mean},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  normal_sd = eventReactive(
    eventExpr = input$normal_update,
    valueExpr = {input$normal_sd},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  
  normal_x = eventReactive(
    eventExpr = input$normal_update,
    valueExpr = {input$normal_x},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  normal_LessThanx = eventReactive(
    eventExpr = input$normal_update,
    valueExpr = {pnorm(q = input$normal_x, mean = input$normal_mean, sd = input$normal_sd) * 100},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  normal_GreaterThanx = eventReactive(
    eventExpr = input$normal_update,
    valueExpr = {(1 - pnorm(q = input$normal_x, mean = input$normal_mean, sd = input$normal_sd)) * 100},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  
  output$normal_mean = renderValueBox({
    normal_mean() %>%
      valueBox(subtitle = "Mean", icon = icon(name = "u"), width = 4)
  })
  
  output$normal_sd = renderValueBox({
    normal_sd() %>%
      valueBox(subtitle = "Standard Deviation", icon = icon(name = "o"), width = 4)
  })
  
  output$normal_x = renderValueBox({
    normal_x() %>%
      valueBox(subtitle = "Target", width = 4)
  })
  
  output$normal_LessThanx = renderValueBox({
    normal_LessThanx() %>%
      percent() %>%
      valueBox(subtitle = paste("Less than ", input$normal_x, "", sep = ""), color = "green", icon = icon(name = "less-than"), width = 4)
  })
  
  output$normal_GreaterThanx = renderValueBox({
    normal_GreaterThanx() %>%
      percent() %>%
      valueBox(subtitle = paste("Greater than ", input$normal_x, sep = ""), color = "yellow", icon = icon(name = "greater-than"), width = 4)
  })
  
  

}







shinyApp(
  ui = uix,
  server = serverx
)