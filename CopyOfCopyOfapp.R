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
    ggplot(head(ill_table, input$nobs)) +
      geom_point(mapping = aes(x = `Illinois Score`, y = `Opponent Score`, color = ifelse(`Game Result` == "W", "Illinois Win", "Illinois Loss")), size = 2) + 
      scale_color_manual(name = "End Result:", values = c("Illinois Win" = "seagreen", "Illinois Loss" = "red")) +
      theme(legend.position = "bottom", axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15), axis.text.x = element_text(size = 18), axis.text.y = element_text(size = 18), legend.title = element_text(size = 15), legend.text = element_text(size=15))
  })
}

shinyApp(ui = ui_scatterplot, server = server_scatterplot)

