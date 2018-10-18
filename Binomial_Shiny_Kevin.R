library(shiny)
library(ggplot2)

ui_bin = fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    
  titlePanel("Kevin's Binomial Formula Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Help Text Select the Function You Want"),
      selectInput("Function", "Choose a function:", choices = c("A", "B")),
      if (choices == "A") {
        numericInput(inputId = "N",
                   label = "Trials",
                   value = 1,
                   min = 1,
                   step = 1)
      }
      else {
        numericInput(inputId = "N",
                     label = "HG",
                     value = 1,
                     min = 1,
                     step = 1)
      }
      # actionButton("gobutton1", "Calculate!")
    ),
    mainPanel(
      textOutput("aye")
    )
  )
)

server_a = function(input, output) {
  output$aye = renderText({
    
  })
}

shinyApp(ui = ui_bin, server = server_a)

