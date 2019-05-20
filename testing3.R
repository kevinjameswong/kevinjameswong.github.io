library(shiny)

t = read.csv("~/Documents/Kevin Stuff/Not Work/website/Website Files/more website files/sleepdata_5_13_2019.csv", sep = ";")

ui <- fluidPage(
  
  titlePanel("BC Liquor Store prices title panel"),
  
  headerPanel("BC Liquor Store prices header panel"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      sliderInput(inputId = "priceInput",
                  label = "Price",
                  min = 0,
                  max = 100,
                  value = c(25, 40),
                  pre = "$"),
      
      radioButtons(inputId = "typeInput",
                   label = "Product typeEEE",
                   choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                   selected = "BEER"),
      
      numericInput(inputId = "num",
                   label = "Number",
                   value = 5,
                   min = 1,
                   max = 10),
      
      selectInput(inputId = "countryInput",
                  label = "Country",
                  choices = c("CANADA", "FRANCE", "ITALY"),
                  selected = "FRANCE")
    ),
    
    mainPanel("the results will go here")
    
  )
)

server = function(input, output) {}
    

    
shinyApp(ui = ui, server = server)
