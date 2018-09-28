library(shiny)

ui_histogram = fluidPage(
  
  titlePanel("Margin of Victory Histogram"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

server_histogram = function(input, output) {
  
  output$distPlot = renderPlot({
    x    = ill_table[, "Margin of Victory"] 
    bins = seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "darkblue", border = "orange", main = "Margin of Victory", xlab = "Points")
  })
}

shinyApp(ui = ui_histogram, server = server_histogram)

