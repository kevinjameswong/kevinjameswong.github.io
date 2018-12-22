library(shiny)

ui_bin = fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    
  titlePanel("Kevin's Binomial Formula Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Help Text Select the Function You Want"),
      numericInput(inputId = "n",
                   label = "Trials:",
                   value = 1,
                   min = 1,
                   step = 1),
      numericInput(inputId = "p",
                   label = "Probability:",
                   value = 0.5,
                   min = 0,
                   max = 1,
                   step = 0.01),
      numericInput(inputId = "k",
                   label = "Successes:",
                   value = 0,
                   min = 0,
                   step = 1),
      actionButton(inputId = "gobutton", label = "Calculate!")
    ),
    mainPanel(
      textOutput("aye")
    )
  )
)

server_a = function(input, output) {
  observeEvent(input$gobutton,
               {
                 text_reactive$text <- input$user_text
               })
  output$aye = renderText({
    values = reactiveValues(
      trials = input$n,
      successes = input$k,
      prob = input$p,
      prob_less = pbinom(q = input$k - 1, size = input$n, prob = input$p),
      prob_at_most = pbinom(q = input$k, size = input$n, prob = input$p),
      prob_equal = pbinom(q = input$k, size = input$n, prob = input$p) - pbinom(q = input$k, - 1, size = input$n, prob = input$p),
      prob_greater = 1 - pbinom(q = input$k, size = input$n, prob = input$p),
      prob_at_least = 1 - pbinom(q = input$k, - 1, size = input$n, prob = input$p))
    # input$goButton1
    # br()
    # br()
    # br()
    # br()
    # br()
    # input$n = trials
    # input$k = successes
    # input$p = prob
    # values$prob_less = pbinom(q = input$k - 1, size = input$n, prob = input$p)
    # values$prob_less_or = pbinom(q = input$k, size = input$n, prob = input$p)
    # values$prob_equal = pbinom(q = input$k, size = input$n, prob = input$p) - pbinom(q = input$k, - 1, size = input$n, prob = input$p)
    # vales$prob_greater = 1 - pbinom(q = input$k, size = input$n, prob = input$p)
    # values$prob_greater_or = 1 - pbinom(q = input$k, - 1, size = input$n, prob = input$p)
    # prob_between = pbinom(q = input$k, size = input$n, prob = input$p) - pbinom(q = input$k, - 1, size = input$n, prob = input$p)
    
    # results = list(values$trials, values$successes, values$prob, values$prob_less, values$prob_at_most, values$prob_equal, values$prob_at_least, values$prob_greater), # prob_between)
    # names(results) = c(
    #   "Trials",
    #   "Successes",
    #   "Probability of Each Success",
    #   paste("Less Than ", input$k, " Successes", sep = ""),
    #   paste("At Most ", input$k, " Successes", sep = ""),
    #   paste("Equal To ", input$k, " Successes", sep = ""),
    #   paste("At Least ", input$k, " Successes", sep = ""),
    #   paste("Greater Than ", input$k, " Successes", sep = "")
      #paste("Between ", successes[1], " and ", successes[2], " Successes", sep = "")
    )
    
    
  })
}

shinyApp(ui = ui_bin, server = server_a)

