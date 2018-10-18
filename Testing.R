library(shiny)

ui_scatterplot = fluidPage(
  h1("Games vs. Points Scatterplot"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("nobs",
                  "Number of Games:",
                  min = 0,
                  max = length(ill_table$Month),
                  value = 0)
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

server_scatterplot = function(input, output) {
  
  output$plot = renderPlot({
    
  })
}

shinyApp(ui = ui_scatterplot, server = server_scatterplot)

