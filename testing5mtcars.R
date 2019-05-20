library(shiny)
library(ggplot2)

ui <- fluidPage(
  
  titlePanel("mtcars yeet"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      radioButtons(inputId = "typeInput",
                   label = "Variable:",
                   choices = c("Miles", "Calories Burned", "Time", "Runs"),
                   selected = "Miles"),
      
      selectInput(inputId = "varInput",
                  label = "")
      
    ),
    
    mainPanel(
      
      h3(textOutput("caption")),
      
      plotOutput("mpgPlot")
     
    )
    
  )
  
)

server = function(input, output) {
  
  # formulaText <- reactive({
  #   paste("mpg ~", input$variable)
  # })
  # 
  # output$caption <- renderText({
  #   formulaText()
  # })
  
  
  
  
  
  
  
  
  output$mpgPlot <- renderPlot({
    ggplot(data = groupby_year, aes(x = Year, y = Miles)) +
      geom_bar(stat = "identity", fill = "darkblue") +
      geom_text(aes(label = groupby_year$Miles), vjust = 1.6, color = "white", size = 5) +
      ggtitle("Year vs. Miles") +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
}
    

    
shinyApp(ui = ui, server = server)

