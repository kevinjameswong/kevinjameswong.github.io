library(shiny)
library(ggplot2)

running = read.csv("~/Documents/Kevin Stuff/Not Work/website/Website Files/more website files/shiny_data.csv")

ui <- fluidPage(
  
  titlePanel("BC Liquor Store prices title panel"),
  
  headerPanel("BC Liquor Store prices header panel"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      radioButtons(inputId = "typeInput",
                   label = "Variable:",
                   choices = c("Miles", "Calories Burned", "Time", "Runs"),
                   selected = "Miles"),
      
      selectInput(inputId = "filterInput",
                  label = "Filter By:",
                  choices = c("Year", "Month", "Weekday"),
                  selected = "Year")
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


groupby_weekday = running %>%
  group_by(filterInput) %>%
  summarise(Total_Runs = sum(RunDistance > 0),
            
            Distance = sum(RunDistance),
            
            Time = paste(ifelse(test = (sum(RunMinutes) + sum(RunSeconds) %/% 60) %/% 60 <= 9,
                                yes = paste("0", (sum(RunMinutes) + sum(RunSeconds) %/% 60) %/% 60, sep = ""),
                                no = as.character((sum(RunMinutes) + sum(RunSeconds) %/% 60) %/% 60)),
                         
                         ifelse(test = (sum(RunMinutes) + sum(RunSeconds) %/% 60) %% 60 <= 9,
                                yes = paste("0", (sum(RunMinutes) + sum(RunSeconds) %/% 60) %% 60, sep = ""),
                                no = as.character((sum(RunMinutes) + sum(RunSeconds) %/% 60) %% 60)),
                         
                         ifelse(test = sum(RunSeconds) %% 60 <= 9,
                                yes = paste("0", sum(RunSeconds) %% 60, sep = ""),
                                no = as.character(sum(RunSeconds) %% 60)), sep = ":"),
            
            Pace = paste((ceiling((sum(RunMinutes) * 60 + sum(RunSeconds)) / sum(RunDistance))) %/% 60, ifelse(test = (ceiling((sum(RunMinutes) * 60 + sum(RunSeconds)) / sum(RunDistance))) %% 60 <= 9,
                                                                                                               yes = paste("0", (ceiling((sum(RunMinutes) * 60 + sum(RunSeconds)) / sum(RunDistance))) %% 60, sep = ""),
                                                                                                               no = paste((ceiling((sum(RunMinutes) * 60 + sum(RunSeconds)) / sum(RunDistance))) %% 60)), sep = ":"))
