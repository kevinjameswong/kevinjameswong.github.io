library(shiny)
ui <- fluidPage(
  textInput("a",""),
  textInput("z", "")
)
server = function(input, output) {
    re = reactive(
      paste(input$a, input$b)) }

    output$b <- renderText({
        re()
      })
shinyApp(ui, server)