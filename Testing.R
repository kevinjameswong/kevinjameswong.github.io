library(shiny)

ui_scatterplot = fluidPage(
  titlePanel(title = "title Panel yeet"),
  h1("h1 yeet"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "function", label = "Choose a function:", choices = c("A", "B")),
      numericInput(inputId = "N",
                   label = "Trials",
                   value = 1,
                   min = 1,
                   step = 1)
    ),
    mainPanel(
      textOutput("aye")
    )
  )
)

server_scatterplot = function(input, output) {
  
  observeEvent(input$A, {
    updateSliderInput(session = session, inputId = "B", value = 2)
  })
  observeEvent(input$B, {
    updateSliderInput(session = session, inputId = "A", value = 4)
  })

}

shinyApp(ui = ui_scatterplot, server = server_scatterplot)

