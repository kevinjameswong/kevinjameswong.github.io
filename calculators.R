library(dplyr)
library(ggplot2)
library(kableExtra)
library(knitr)
library(lubridate)
library(rlang)
library(shiny)
library(shinydashboard)
library(stringr)
library(tidyverse)


#https://mastering-shiny.org/basic-ui.html
#https://bookdown.org/loankimrobinson/rshinybook/stock-front-footer.html#stock-front-footer

#selectInput = dropdown
#sliderInput = slider
#textInput
#passwordInput
#textAreaInput
#numericInput("num", "Number one", value = 0, min = 0, max = 100),
#dateInput("dob", "When were you born?"),
#dateRangeInput

#navbarMenu is a navbar, except with multiple options in a dropdown. example is "Data Analysis Projects"


percent = function(x, digits = 2, format = "f", ...) {
  t = formatC(x * 100, format = format, digits = digits, ...) %>% paste("%", sep = "")
  return(t)
}

modify_stop_propagation = function(x) {
  x$children[[1]]$attribs$onclick = "event.stopPropagation()"
  x
} #https://forum.posit.co/t/shinydashboard-keep-sidebar-tab-expanded-while-other-tab-is-clicked-expanded/10192/2

uix = dashboardPage(
  skin = "red",
  dashboardHeader(
    title = "Calculators",
    disable = FALSE),  
  dashboardSidebar(
    sidebarMenu(
      modify_stop_propagation(
      menuItem(
        text = "Hypothesis Testing",
        startExpanded = TRUE,
        #tabName = "TxT",
        #https://forum.posit.co/t/shinydashboard-keep-sidebar-tab-expanded-while-other-tab-is-clicked-expanded/10192
        #badgeLabel = "NEW",
        menuSubItem(
          text = "One Sample - Mean (Normal)",
          icon = icon(name = "calculator"),
          tabName = "HypothesisTest_OneSample_MeanNormal",
          #badgeLabel = "NEW",
          selected = TRUE),
        menuSubItem(
          text = "One Sample - Mean (T)",
          icon = icon(name = "calculator"),
          tabName = "HypothesisTest_OneSample_MeanT",
          #badgeLabel = "NEW",
          selected = TRUE),
        menuSubItem(
          text = "One Sample - Proportion",
          icon = icon(name = "calculator"),
          tabName = "HypothesisTest_OneSample_Proportion",
          #badgeLabel = "NEW",
          selected = FALSE)#,
        #menuSubItem(
          #text = "One Sample - Variance",
          #icon = icon(name = "calculator"),
          #tabName = "HypothesisTest_OneSample_Variance",
          ##badgeLabel = "NEW",
          #selected = FALSE)
      )
      ),
      modify_stop_propagation(
      menuItem(
        text = "Probability Calculators",
        startExpanded = TRUE,
        #tabName = "TxT",
        #badgeLabel = "NEW",
        menuSubItem(
          text = "Binomial Distribution",
          icon = icon(name = "calculator"),
          tabName = "ProbabilityCalculator_Binomial",
          #badgeLabel = "NEW",
          selected = FALSE),
        menuSubItem(
          text = "Normal Distrubtion",
          icon = icon(name = "calculator"),
          tabName = "ProbabilityCalculator_Normal",
          #badgeLabel = "NEW",
          selected = FALSE),
        menuSubItem(
          text = "Geometric Distrubtion",
          icon = icon(name = "calculator"),
          tabName = "ProbabilityCalculator_Geometric",
          #badgeLabel = "NEW",
          selected = FALSE)
      )
      ),
      modify_stop_propagation(
      menuItem(
        text = "Confidence Intervals",
        startExpanded = TRUE,
        #tabName = "TxT",
        #badgeLabel = "NEW",
        menuSubItem(
          text = "One Sample - Mean (Normal)",
          icon = icon(name = "calculator"),
          tabName = "ConfidenceIntervals_OneSample_MeanNormal",
          #badgeLabel = "NEW",
          selected = FALSE),
        menuSubItem(
          text = "One Sample - Mean (T)",
          icon = icon(name = "calculator"),
          tabName = "ConfidenceIntervals_OneSample_MeanT",
          #badgeLabel = "NEW",
          selected = FALSE),
        menuSubItem(
          text = "One Sample - Proportion",
          icon = icon(name = "calculator"),
          tabName = "ConfidenceIntervals_OneSample_Proportion",
          #badgeLabel = "NEW",
          selected = FALSE),
        menuSubItem(
          text = "One Sample - Variance",
          icon = icon(name = "calculator"),
          tabName = "ConfidenceIntervals_OneSample_Variance",
          #badgeLabel = "NEW",
          selected = FALSE)
        )
      ),
      modify_stop_propagation(
        menuItem(
          text = "Geometry",
          startExpanded = TRUE,
          #tabName = "TxT",
          #badgeLabel = "NEW",
          menuSubItem(
            text = "Circle",
            icon = icon(name = "circle"),
            tabName = "Geometry_Circle",
            #badgeLabel = "NEW",
            selected = FALSE),
          menuSubItem(
            text = "Triangle",
            icon = icon(name = "triangle"),
            tabName = "Geometry_Triangle",
            #badgeLabel = "NEW",
            selected = FALSE),
          menuSubItem(
            text = "Rectangle",
            icon = icon(name = "rectangle"),
            tabName = "Geometry_Rectangle",
            #badgeLabel = "NEW",
            selected = FALSE)
        )
      )
    )
  ),
  
  dashboardBody(
    
    tags$script(HTML("$('body').addClass('fixed');")),
    
    tabItems(
      
      tabItem(tabName = "HypothesisTest_OneSample_MeanNormal",
              #br(), #"Yoing",
              fluidRow(width = 12,
                       box(title = "Input", width = 3, #background = "gray",
                           column(width = 12,
                                  numericInput(
                                    inputId = "HypothesisTest_OneSample_MeanNormal_MeanPopulation",
                                    label = "Population Mean:",
                                    value = 10,
                                    step = 1
                                  ),
                                  numericInput(
                                    inputId = "HypothesisTest_OneSample_MeanNormal_SDPopulation",
                                    label = "Population Standard Deviation:",
                                    value = 1,
                                    step = 1
                                  ),
                                  numericInput(
                                    inputId = "HypothesisTest_OneSample_MeanNormal_MeanSample",
                                    label = "Sample Mean:",
                                    value = 9,
                                    step = 1
                                  ),
                                  numericInput(
                                    inputId = "HypothesisTest_OneSample_MeanNormal_SampleSize",
                                    label = "Sample Size:",
                                    value = 25,
                                    step = 1,
                                    min = 0
                                  ),
                                  numericInput(
                                    inputId = "HypothesisTest_OneSample_MeanNormal_Alpha",
                                    label = "Alpha:",
                                    value = 0.05,
                                    step = 0.01,
                                    min = 0,
                                    max = 1
                                  ),
                                  selectInput(
                                    inputId = "HypothesisTest_OneSample_MeanNormal_TestType",
                                    label = "Test Type:",
                                    choices = c("One Sided - Left", "One Sided - Right", "Two Sided"),
                                    multiple = FALSE,
                                    selected = c("One Sided - Left"),
                                    selectize = FALSE
                                  ),
                                  
                                  actionButton(inputId = "HypothesisTest_OneSample_MeanNormal_UpdateButton", label = "Update", width = "100%")
                           )
                       ),
                       box(title = "Hypothesis Test - One Sample - Mean (Normal)", width = 9,
                           tableOutput(outputId = "HypothesisTest_OneSample_MeanNormal_Final"),
                           plotOutput(outputId = "HypothesisTest_OneSample_MeanNormal_Graph")
                       )
                       
                       
              )
              
      ),
      
      tabItem(tabName = "HypothesisTest_OneSample_MeanT",
              #br(), #"Yoing",
              fluidRow(width = 12,
                       box(title = "Input", width = 3, #background = "gray",
                           column(width = 12,
                                  numericInput(
                                    inputId = "HypothesisTest_OneSample_MeanT_MeanPopulation",
                                    label = "Population Mean:",
                                    value = 10,
                                    step = 1
                                  ),
                                  numericInput(
                                    inputId = "HypothesisTest_OneSample_MeanT_MeanSample",
                                    label = "Sample Mean:",
                                    value = 9,
                                    step = 1
                                  ),
                                  numericInput(
                                    inputId = "HypothesisTest_OneSample_MeanT_SDSample",
                                    label = "Sample Standard Deviation:",
                                    value = 1,
                                    step = 1
                                  ),
                                  numericInput(
                                    inputId = "HypothesisTest_OneSample_MeanT_SampleSize",
                                    label = "Sample Size:",
                                    value = 25,
                                    step = 1,
                                    min = 0
                                  ),
                                  numericInput(
                                    inputId = "HypothesisTest_OneSample_MeanT_Alpha",
                                    label = "Alpha:",
                                    value = 0.05,
                                    step = 0.01,
                                    min = 0,
                                    max = 1
                                  ),
                                  selectInput(
                                    inputId = "HypothesisTest_OneSample_MeanT_TestType",
                                    label = "Test Type:",
                                    choices = c("One Sided - Left", "One Sided - Right", "Two Sided"),
                                    multiple = FALSE,
                                    selected = c("One Sided - Left"),
                                    selectize = FALSE
                                  ),
                                  
                                  actionButton(inputId = "HypothesisTest_OneSample_MeanT_UpdateButton", label = "Update", width = "100%")
                           )
                       ),
                       box(title = "Hypothesis Test - One Sample - Mean (T)", width = 9,
                           tableOutput(outputId = "HypothesisTest_OneSample_MeanT_Final"),
                           plotOutput(outputId = "HypothesisTest_OneSample_MeanT_Graph")
                       )
                       
                       
              )
              
      ),
      
            tabItem(tabName = "HypothesisTest_OneSample_Proportion",
              #br(), #"Yoing",
              fluidRow(width = 12,
                       box(title = "Input", width = 3, #background = "gray",
                           column(width = 12,
                                  numericInput(
                                    inputId = "HypothesisTest_OneSample_Proportion_ProportionPopulation",
                                    label = "Population Proportion:",
                                    value = 0.5,
                                    step = 0.01,
                                    min = 0,
                                    max = 1
                                  ),
                                  numericInput(
                                    inputId = "HypothesisTest_OneSample_Proportion_ProportionSample",
                                    label = "Sample Proportion:",
                                    value = 0.64,
                                    step = 0.01,
                                    min = 0,
                                    max = 1
                                  ),
                                  numericInput(
                                    inputId = "HypothesisTest_OneSample_Proportion_SampleSize",
                                    label = "Sample Size:",
                                    value = 25,
                                    step = 1,
                                    min = 0
                                  ),
                                  numericInput(
                                    inputId = "HypothesisTest_OneSample_Proportion_Alpha",
                                    label = "Alpha:",
                                    value = 0.05,
                                    step = 0.01,
                                    min = 0,
                                    max = 1
                                  ),
                                  selectInput(
                                    inputId = "HypothesisTest_OneSample_Proportion_TestType",
                                    label = "Test Type:",
                                    choices = c("One Sided - Left", "One Sided - Right", "Two Sided"),
                                    multiple = FALSE,
                                    selected = c("One Sided - Right"),
                                    selectize = FALSE
                                  ),
                                  
                                  actionButton(inputId = "HypothesisTest_OneSample_Proportion_UpdateButton", label = "Update", width = "100%")
                           )
                       ),
                       box(title = "Hypothesis Test - One Sample - Proportion", width = 9,
                           tableOutput(outputId = "HypothesisTest_OneSample_Proportion_Final"),
                           plotOutput(outputId = "HypothesisTest_OneSample_Proportion_Graph")
                       )
                       
                       
              )
              
      ),
      
      tabItem(tabName = "ProbabilityCalculator_Binomial",
              fluidRow(width = 12,
                box(title = "Input", width = 3,
                column(width = 12,
                       numericInput(
                         inputId = "ProbabilityCalculator_Binomial_P",
                         label = "Probability:",
                         value = 0.5,
                         min = 0,
                         max = 1,
                         step = 0.1
                       ),
                       numericInput(
                         inputId = "ProbabilityCalculator_Binomial_N",
                         label = "Attempts:",
                         value = 10,
                         min = 0,
                         step = 1
                       ),
                       numericInput(
                         inputId = "ProbabilityCalculator_Binomial_K",
                         label = "Successes:",
                         value = 3,
                         min = 0,
                         step = 1
                       ),
                       actionButton(inputId = "ProbabilityCalculator_Binomial_UpdateButton", label = "Update", width = "100%")
                       )
                ),
                box(title = "Probability Calculator - Binomial Distribution", width = 9,
                  valueBoxOutput(outputId = "ProbabilityCalculator_Binomial_P"),
                  valueBoxOutput(outputId = "ProbabilityCalculator_Binomial_N"),
                  valueBoxOutput(outputId = "ProbabilityCalculator_Binomial_K"),
                  valueBoxOutput(outputId = "ProbabilityCalculator_Binomial_ExpectedOutcome"),
                  valueBoxOutput(outputId = "ProbabilityCalculator_Binomial_LessThanKSuccesses"),
                  valueBoxOutput(outputId = "ProbabilityCalculator_Binomial_AtMostKSuccesses"),
                  valueBoxOutput(outputId = "ProbabilityCalculator_Binomial_ExactlyKSuccesses"),
                  valueBoxOutput(outputId = "ProbabilityCalculator_Binomial_GreaterThanKSuccesses"),
                  valueBoxOutput(outputId = "ProbabilityCalculator_Binomial_AtLeastKSuccesses")
                  )
                
      )
      ),
      
      tabItem(tabName = "ProbabilityCalculator_Geometric",
              fluidRow(width = 12,
                       box(title = "Input", width = 3,
                           column(width = 12,
                                  numericInput(
                                    inputId = "ProbabilityCalculator_Geometric_P",
                                    label = "Probability:",
                                    value = 0.1,
                                    min = 0,
                                    max = 1,
                                    step = 0.1
                                  ),
                                  numericInput(
                                    inputId = "ProbabilityCalculator_Geometric_N",
                                    label = "Attempts:",
                                    value = 5,
                                    min = 1,
                                    step = 1
                                  ),
                                  actionButton(inputId = "ProbabilityCalculator_Geometric_UpdateButton", label = "Update", width = "100%")
                           )
                       ),
                       box(title = "Probability Calculator - Geometric Distribution", width = 9,
                           valueBoxOutput(outputId = "ProbabilityCalculator_Geometric_P"),
                           valueBoxOutput(outputId = "ProbabilityCalculator_Geometric_K"),
                           valueBoxOutput(outputId = "ProbabilityCalculator_Geometric_ExpectedOutcome"),
                           valueBoxOutput(outputId = "ProbabilityCalculator_Geometric_LessThanKSuccesses"),
                           valueBoxOutput(outputId = "ProbabilityCalculator_Geometric_AtMostKSuccesses"),
                           valueBoxOutput(outputId = "ProbabilityCalculator_Geometric_ExactlyKSuccesses"),
                           valueBoxOutput(outputId = "ProbabilityCalculator_Geometric_GreaterThanKSuccesses"),
                           valueBoxOutput(outputId = "ProbabilityCalculator_Geometric_AtLeastKSuccesses")
                       )
                       
              )
      ),

      # Second tab content
      tabItem(tabName = "ProbabilityCalculator_Normal",
              #br(), #"Yoing",
              fluidRow(width = 12,
                       box(title = "Input", width = 3, #background = "gray",
                           column(width = 12,
                                  numericInput(
                                    inputId = "ProbabilityCalculator_Normal_MeanPopulation",
                                    label = "Population Mean:",
                                    value = 10,
                                    step = 0.1
                                  ),
                                  numericInput(
                                    inputId = "ProbabilityCalculator_Normal_SDPopulation",
                                    label = "Population Standard Deviation:",
                                    value = 1,
                                    step = 1
                                  ),
                                  numericInput(
                                    inputId = "ProbabilityCalculator_Normal_LowerBound",
                                    label = "Lower Bound:",
                                    value = 8,
                                    step = 1
                                  ),
                                  numericInput(
                                    inputId = "ProbabilityCalculator_Normal_UpperBound",
                                    label = "Upper Bound:",
                                    value = 12,
                                    step = 1
                                  ),

                                  actionButton(inputId = "ProbabilityCalculator_Normal_UpdateButton", label = "Update", width = "100%")
                           )
                       ),
                       box(title = "Probability Calculator - Normal Distribution", width = 9,
                           valueBoxOutput(outputId = "ProbabilityCalculator_Normal_MeanPopulation"),
                           valueBoxOutput(outputId = "ProbabilityCalculator_Normal_SDPopulation"),
                           valueBoxOutput(outputId = "ProbabilityCalculator_Normal_LowerBound"),
                           valueBoxOutput(outputId = "ProbabilityCalculator_Normal_UpperBound"),
                           valueBoxOutput(outputId = "ProbabilityCalculator_Normal_Probability")
                       )
                       
                    
              )
              
      ),
      
      tabItem(tabName = "ProbabilityCalculator_T",
              #br(), #"Yoing",
              fluidRow(width = 12,
                       box(title = "Input", width = 3, #background = "gray",
                           column(width = 12,
                                  numericInput(
                                    inputId = "ProbabilityCalculator_T_MeanPopulation",
                                    label = "Population Mean:",
                                    value = 10,
                                    step = 0.1
                                  ),
                                  numericInput(
                                    inputId = "ProbabilityCalculator_T_DegreesOfFreedom",
                                    label = "Degrees of Freedom:",
                                    value = 1,
                                    step = 1
                                  ),
                                  numericInput(
                                    inputId = "ProbabilityCalculator_T_LowerBound",
                                    label = "Lower Bound:",
                                    value = 8,
                                    step = 1
                                  ),
                                  numericInput(
                                    inputId = "ProbabilityCalculator_T_UpperBound",
                                    label = "Upper Bound:",
                                    value = 12,
                                    step = 1
                                  ),
                                  
                                  actionButton(inputId = "ProbabilityCalculator_T_UpdateButton", label = "Update", width = "100%")
                           )
                       ),
                       box(title = "Probability Calculator - T Distribution", width = 9,
                           valueBoxOutput(outputId = "ProbabilityCalculator_T_MeanPopulation"),
                           valueBoxOutput(outputId = "ProbabilityCalculator_T_DegreesOfFreedom"),
                           valueBoxOutput(outputId = "ProbabilityCalculator_T_LowerBound"),
                           valueBoxOutput(outputId = "ProbabilityCalculator_T_UpperBound"),
                           valueBoxOutput(outputId = "ProbabilityCalculator_T_Probability")
                       )
                       
                       
              )
              
      ),
      
      tabItem(tabName = "ConfidenceIntervals_OneSample_MeanNormal",
              #br(), #"Yoing",
              fluidRow(width = 12,
                       box(title = "Input", width = 3, #background = "gray",
                           column(width = 12,
                                  numericInput(
                                    inputId = "ConfidenceIntervals_OneSample_MeanNormal_MeanSample",
                                    label = "Sample Mean:",
                                    value = 10,
                                    step = 0.1
                                  ),
                                  numericInput(
                                    inputId = "ConfidenceIntervals_OneSample_MeanNormal_SDPopulation",
                                    label = "Population Standard Deviation:",
                                    value = 1,
                                    step = 1
                                  ),
                                  numericInput(
                                    inputId = "ConfidenceIntervals_OneSample_MeanNormal_SampleSize",
                                    label = "Sample Size:",
                                    value = 25,
                                    step = 1,
                                    min = 0
                                  ),
                                  numericInput(
                                    inputId = "ConfidenceIntervals_OneSample_MeanNormal_ConfidenceLevel",
                                    label = "Confidence Level:",
                                    value = 95,
                                    step = 1,
                                    min = 0,
                                    max = 100
                                  ),
                                  
                                  actionButton(inputId = "ConfidenceIntervals_OneSample_MeanNormal_UpdateButton", label = "Update", width = "100%")
                           )
                       ),
                       box(title = "Confidence Interval - One Sample - Mean (Normal)", width = 9,
                           valueBoxOutput(outputId = "ConfidenceIntervals_OneSample_MeanNormal_LowerBound"),
                           valueBoxOutput(outputId = "ConfidenceIntervals_OneSample_MeanNormal_UpperBound"),
                           plotOutput(outputId = "ConfidenceIntervals_OneSample_MeanNormal_Plot")
                       )
                       
                       
              )
              
      ),
      
      tabItem(tabName = "ConfidenceIntervals_OneSample_MeanT",
              #br(), #"Yoing",
              fluidRow(width = 12,
                       box(title = "Input", width = 3, #background = "gray",
                           column(width = 12,
                                  numericInput(
                                    inputId = "ConfidenceIntervals_OneSample_MeanT_MeanSample",
                                    label = "Sample Mean:",
                                    value = 10,
                                    step = 0.1
                                  ),
                                  numericInput(
                                    inputId = "ConfidenceIntervals_OneSample_MeanT_SDSample",
                                    label = "Sample Standard Deviation:",
                                    value = 1,
                                    step = 1
                                  ),
                                  numericInput(
                                    inputId = "ConfidenceIntervals_OneSample_MeanT_SampleSize",
                                    label = "Sample Size:",
                                    value = 25,
                                    step = 1,
                                    min = 0
                                  ),
                                  numericInput(
                                    inputId = "ConfidenceIntervals_OneSample_MeanT_ConfidenceLevel",
                                    label = "Confidence Level:",
                                    value = 95,
                                    step = 1,
                                    min = 0,
                                    max = 100
                                  ),
                                  
                                  actionButton(inputId = "ConfidenceIntervals_OneSample_MeanT_UpdateButton", label = "Update", width = "100%")
                           )
                       ),
                       box(title = "Confidence Interval - One Sample - Mean (T)", width = 9,
                           valueBoxOutput(outputId = "ConfidenceIntervals_OneSample_MeanT_LowerBound"),
                           valueBoxOutput(outputId = "ConfidenceIntervals_OneSample_MeanT_UpperBound")
                           
                       )
                       
                       
              )
              
      ),
      
      tabItem(tabName = "ConfidenceIntervals_OneSample_Proportion",
              #br(), #"Yoing",
              fluidRow(width = 12,
                       box(title = "Input", width = 3, #background = "gray",
                           column(width = 12,
                                  numericInput(
                                    inputId = "ConfidenceIntervals_OneSample_Proportion_SampleProportion",
                                    label = "Sample Proportion:",
                                    value = 0.5,
                                    step = 0.01,
                                    min = 0,
                                    max = 1
                                  ),
                                  numericInput(
                                    inputId = "ConfidenceIntervals_OneSample_Proportion_SampleSize",
                                    label = "Sample Size:",
                                    value = 25,
                                    step = 1,
                                    min = 0
                                  ),
                                  numericInput(
                                    inputId = "ConfidenceIntervals_OneSample_Proportion_ConfidenceLevel",
                                    label = "Confidence Level:",
                                    value = 95,
                                    step = 1,
                                    min = 0,
                                    max = 100
                                  ),
                                  
                                  actionButton(inputId = "ConfidenceIntervals_OneSample_Proportion_UpdateButton", label = "Update", width = "100%")
                           )
                       ),
                       box(title = "Confidence Interval - One Sample - Mean (Proportion)", width = 9,
                           valueBoxOutput(outputId = "ConfidenceIntervals_OneSample_Proportion_LowerBound"),
                           valueBoxOutput(outputId = "ConfidenceIntervals_OneSample_Proportion_UpperBound")
                       )
                       
                       
              )
              
      ),
      
      tabItem(tabName = "ConfidenceIntervals_OneSample_Variance",
              #br(), #"Yoing",
              fluidRow(width = 12,
                       box(title = "Input", width = 3, #background = "gray",
                           column(width = 12,
                                  numericInput(
                                    inputId = "ConfidenceIntervals_OneSample_Variance_SampleVariance",
                                    label = "Sample Variance:",
                                    value = 10,
                                    step = 1,
                                    min = 0,
                                    max = 1
                                  ),
                                  numericInput(
                                    inputId = "ConfidenceIntervals_OneSample_Variance_SampleSize",
                                    label = "Sample Size:",
                                    value = 25,
                                    step = 1,
                                    min = 0
                                  ),
                                  numericInput(
                                    inputId = "ConfidenceIntervals_OneSample_Variance_ConfidenceLevel",
                                    label = "Confidence Level:",
                                    value = 95,
                                    step = 1,
                                    min = 0,
                                    max = 100
                                  ),
                                  
                                  actionButton(inputId = "ConfidenceIntervals_OneSample_Variance_UpdateButton", label = "Update", width = "100%")
                           )
                       ),
                       box(title = "Confidence Interval - One Sample - Mean (Variance)", width = 9,
                           valueBoxOutput(outputId = "ConfidenceIntervals_OneSample_Variance_LowerBound"),
                           valueBoxOutput(outputId = "ConfidenceIntervals_OneSample_Variance_UpperBound")
                           
                       )
                       
                       
              )
              
      ),
      
      tabItem(tabName = "Geometry_Circle",
              #br(), #"Yoing",
              fluidRow(width = 12,
                       box(title = "Input", width = 3, #background = "gray",
                           column(width = 12,
                                  numericInput(
                                    inputId = "Geometry_Circle_Radius",
                                    label = "Radius:",
                                    value = 5,
                                    step = 1,
                                    min = 0
                                  ),
                                  
                                  actionButton(inputId = "Geometry_Circle_UpdateButton", label = "Update", width = "100%")
                           )
                       ),
                       box(title = "Geometry (Circle)", width = 9,
                           tableOutput(outputId = "Geometry_Circle_Final")
                       )
              )
      ),
      
      tabItem(tabName = "Geometry_Triangle",
              #br(), #"Yoing",
              fluidRow(width = 12,
                       box(title = "Input", width = 3, #background = "gray",
                           column(width = 12,
                                  numericInput(
                                    inputId = "Geometry_Triangle_Height",
                                    label = "Height:",
                                    value = 10,
                                    step = 1,
                                    min = 0,
                                    max = 1
                                  ),
                                  numericInput(
                                    inputId = "Geometry_Triangle_Width",
                                    label = "Width:",
                                    value = 25,
                                    step = 1,
                                    min = 0
                                  ),
                                  
                                  actionButton(inputId = "Geometry_Triangle_UpdateButton", label = "Update", width = "100%")
                           )
                       ),
                       box(title = "Geometry (Triangle)", width = 9,
                           tableOutput(outputId = "Geometry_Triangle_Final")
                       )
              )
      )
      


  ))
)

serverx = function(input, output, session) {

  
  ProbabilityCalculator_Binomial_P = eventReactive(
    eventExpr = input$ProbabilityCalculator_Binomial_UpdateButton,
    valueExpr = {input$ProbabilityCalculator_Binomial_P},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  ProbabilityCalculator_Binomial_N = eventReactive(
    eventExpr = input$ProbabilityCalculator_Binomial_UpdateButton,
    valueExpr = {input$ProbabilityCalculator_Binomial_N},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  ProbabilityCalculator_Binomial_K = eventReactive(
    eventExpr = input$ProbabilityCalculator_Binomial_UpdateButtonButton,
    valueExpr = {input$ProbabilityCalculator_Binomial_K},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  ProbabilityCalculator_Binomial_ExpectedOutcome = eventReactive(
    eventExpr = input$ProbabilityCalculator_Binomial_UpdateButtonButton,
    valueExpr = {input$ProbabilityCalculator_Binomial_P * input$ProbabilityCalculator_Binomial_N},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  ProbabilityCalculator_Binomial_Prob_LessThan = eventReactive(
    eventExpr = input$ProbabilityCalculator_Binomial_UpdateButtonButton,
    valueExpr = {pbinom(q = input$ProbabilityCalculator_Binomial_K - 1, size = input$ProbabilityCalculator_Binomial_N, prob = input$ProbabilityCalculator_Binomial_P)},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  ProbabilityCalculator_Binomial_Prob_AtMost = eventReactive(
    eventExpr = input$ProbabilityCalculator_Binomial_UpdateButton,
    valueExpr = {pbinom(q = input$ProbabilityCalculator_Binomial_K, size = input$ProbabilityCalculator_Binomial_N, prob = input$ProbabilityCalculator_Binomial_P)},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )

  ProbabilityCalculator_Binomial_Prob_Exactly = eventReactive(
    eventExpr = input$ProbabilityCalculator_Binomial_UpdateButton,
    valueExpr = {pbinom(q = input$ProbabilityCalculator_Binomial_K, size = input$ProbabilityCalculator_Binomial_N, prob = input$ProbabilityCalculator_Binomial_P) - pbinom(q = input$ProbabilityCalculator_Binomial_K - 1, size = input$ProbabilityCalculator_Binomial_N, prob = input$ProbabilityCalculator_Binomial_P)},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  ProbabilityCalculator_Binomial_Prob_GreaterThan = eventReactive(
    eventExpr = input$ProbabilityCalculator_Binomial_UpdateButton,
    valueExpr = {1 - pbinom(q = input$ProbabilityCalculator_Binomial_K, size = input$ProbabilityCalculator_Binomial_N, prob = input$ProbabilityCalculator_Binomial_P)},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  ProbabilityCalculator_Binomial_Prob_AtLeast = eventReactive(
    eventExpr = input$ProbabilityCalculator_Binomial_UpdateButton,
    valueExpr = {1 - pbinom(q = input$ProbabilityCalculator_Binomial_K - 1, size = input$ProbabilityCalculator_Binomial_N, prob = input$ProbabilityCalculator_Binomial_P)},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )

  output$ProbabilityCalculator_Binomial_P = renderValueBox({
    ProbabilityCalculator_Binomial_P() %>%
      percent(digits = 2) %>%
      valueBox(subtitle = "Probability of Each Success", icon = icon(name = "p"), width = 4)
  })
  
  output$ProbabilityCalculator_Binomial_N = renderValueBox({
    ProbabilityCalculator_Binomial_N() %>%
      format(nsmall = 0, big.mark = ",") %>%
      valueBox(subtitle = "Number of Attempts", icon = icon(name = "n"), width = 4)
  })
  
  output$ProbabilityCalculator_Binomial_K = renderValueBox({
    ProbabilityCalculator_Binomial_K() %>%
      format(nsmall = 0, big.mark = ",") %>%
      valueBox(subtitle = "Number of Successes", icon = icon(name = "k"), width = 4)
  })
  
  output$ProbabilityCalculator_Binomial_ExpectedOutcome = renderValueBox({
    ProbabilityCalculator_Binomial_ExpectedOutcome() %>%
      valueBox(subtitle = "Expected Outcome", color = "light-blue", width = 4)
  })
  
  output$ProbabilityCalculator_Binomial_LessThanKSuccesses = renderValueBox({
    ProbabilityCalculator_Binomial_Prob_LessThan() %>%
      percent(digits = 2) %>%
      valueBox(subtitle = paste("Less than ", ProbabilityCalculator_Binomial_K(), " Successes", sep = ""), color = "green", icon = icon(name = "less-than"), width = 4)
  })
  
  output$ProbabilityCalculator_Binomial_AtMostKSuccesses = renderValueBox({
    ProbabilityCalculator_Binomial_Prob_AtMost() %>%
      percent(digits = 2) %>%
      valueBox(subtitle = paste("At Most ", ProbabilityCalculator_Binomial_K(), " Successes", sep = ""), color = "green", icon = icon(name = "less-than-equal"), width = 4)
  })
  
  output$ProbabilityCalculator_Binomial_ExactlyKSuccesses = renderValueBox({
    ProbabilityCalculator_Binomial_Prob_Exactly() %>%
      percent(digits = 2) %>%
      valueBox(subtitle = paste("Exactly ", ProbabilityCalculator_Binomial_K(), " Successes", sep = ""), color = "light-blue", icon = icon(name = "equals"), width = 4)
  })
  
  output$ProbabilityCalculator_Binomial_GreaterThanKSuccesses = renderValueBox({
    ProbabilityCalculator_Binomial_Prob_GreaterThan() %>%
      percent(digits = 2) %>%
      valueBox(subtitle = paste("Greater than ", ProbabilityCalculator_Binomial_K(), " Successes", sep = ""), color = "yellow", icon = icon(name = "greater-than"), width = 4)
  })
  
  output$ProbabilityCalculator_Binomial_AtLeastKSuccesses = renderValueBox({
    ProbabilityCalculator_Binomial_Prob_AtLeast() %>%
      percent(digits = 2) %>%
      valueBox(subtitle = paste("At Least ", ProbabilityCalculator_Binomial_K(), " Successes", sep = ""), color = "yellow", icon = icon(name = "greater-than-equal"), width = 4)
  })
  
  
  
  
  
  
  
  
  
  
  ProbabilityCalculator_Geometric_P = eventReactive(
    eventExpr = input$ProbabilityCalculator_Geometric_UpdateButton,
    valueExpr = {input$ProbabilityCalculator_Geometric_P},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  ProbabilityCalculator_Geometric_N = eventReactive(
    eventExpr = input$ProbabilityCalculator_Geometric_UpdateButton,
    valueExpr = {input$ProbabilityCalculator_Geometric_N},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  ProbabilityCalculator_Geometric_ExpectedOutcome = eventReactive(
    eventExpr = input$ProbabilityCalculator_Geometric_UpdateButtonButton,
    valueExpr = {1 / input$ProbabilityCalculator_Geometric_P},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  ProbabilityCalculator_Geometric_Prob_LessThan = eventReactive(
    eventExpr = input$ProbabilityCalculator_Geometric_UpdateButtonButton,
    valueExpr = {pgeom(q = input$ProbabilityCalculator_Geometric_N - 2, prob = input$ProbabilityCalculator_Geometric_P)},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  ProbabilityCalculator_Geometric_Prob_AtMost = eventReactive(
    eventExpr = input$ProbabilityCalculator_Geometric_UpdateButton,
    valueExpr = {pgeom(q = input$ProbabilityCalculator_Geometric_N - 1, prob = input$ProbabilityCalculator_Geometric_P)},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  ProbabilityCalculator_Geometric_Prob_Exactly = eventReactive(
    eventExpr = input$ProbabilityCalculator_Geometric_UpdateButton,
    valueExpr = {pgeom(q = input$ProbabilityCalculator_Geometric_N - 1, prob = input$ProbabilityCalculator_Geometric_P) - pgeom(q = input$ProbabilityCalculator_Geometric_N - 2, prob = input$ProbabilityCalculator_Geometric_P)},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  ProbabilityCalculator_Geometric_Prob_GreaterThan = eventReactive(
    eventExpr = input$ProbabilityCalculator_Geometric_UpdateButton,
    valueExpr = {1 - pgeom(q = input$ProbabilityCalculator_Geometric_N - 1, prob = input$ProbabilityCalculator_Geometric_P)},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  ProbabilityCalculator_Geometric_Prob_AtLeast = eventReactive(
    eventExpr = input$ProbabilityCalculator_Geometric_UpdateButton,
    valueExpr = {1 - pgeom(q = input$ProbabilityCalculator_Geometric_N - 2, prob = input$ProbabilityCalculator_Geometric_P)},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  output$ProbabilityCalculator_Geometric_P = renderValueBox({
    ProbabilityCalculator_Geometric_P() %>%
      percent(digits = 2) %>%
      valueBox(subtitle = "Probability of Each Success", icon = icon(name = "p"), width = 4)
  })
  
  output$ProbabilityCalculator_Geometric_N = renderValueBox({
    ProbabilityCalculator_Geometric_N() %>%
      format(nsmall = 0, big.mark = ",") %>%
      valueBox(subtitle = "Number of Attempts", icon = icon(name = "n"), width = 4)
  })
  
  output$ProbabilityCalculator_Geometric_ExpectedOutcome = renderValueBox({
    ProbabilityCalculator_Geometric_ExpectedOutcome() %>%
      valueBox(subtitle = "Expected Outcome", color = "light-blue", width = 4)
  })
  
  output$ProbabilityCalculator_Geometric_LessThanKSuccesses = renderValueBox({
    ProbabilityCalculator_Geometric_Prob_LessThan() %>%
      percent(digits = 2) %>%
      valueBox(subtitle = paste("Less than ", ProbabilityCalculator_Geometric_N(), " Attempts", sep = ""), color = "green", icon = icon(name = "less-than"), width = 4)
  })
  
  output$ProbabilityCalculator_Geometric_AtMostKSuccesses = renderValueBox({
    ProbabilityCalculator_Geometric_Prob_AtMost() %>%
      percent(digits = 2) %>%
      valueBox(subtitle = paste("At Most ", ProbabilityCalculator_Geometric_N(), " Attempts", sep = ""), color = "green", icon = icon(name = "less-than-equal"), width = 4)
  })
  
  output$ProbabilityCalculator_Geometric_ExactlyKSuccesses = renderValueBox({
    ProbabilityCalculator_Geometric_Prob_Exactly() %>%
      percent(digits = 2) %>%
      valueBox(subtitle = paste("Exactly ", ProbabilityCalculator_Geometric_N(), " Attempts", sep = ""), color = "light-blue", icon = icon(name = "equals"), width = 4)
  })
  
  output$ProbabilityCalculator_Geometric_GreaterThanKSuccesses = renderValueBox({
    ProbabilityCalculator_Geometric_Prob_GreaterThan() %>%
      percent(digits = 2) %>%
      valueBox(subtitle = paste("Greater than ", ProbabilityCalculator_Geometric_N(), " Attempts", sep = ""), color = "yellow", icon = icon(name = "greater-than"), width = 4)
  })
  
  output$ProbabilityCalculator_Geometric_AtLeastKSuccesses = renderValueBox({
    ProbabilityCalculator_Geometric_Prob_AtLeast() %>%
      percent(digits = 2) %>%
      valueBox(subtitle = paste("At Least ", ProbabilityCalculator_Geometric_N(), " Attempts", sep = ""), color = "yellow", icon = icon(name = "greater-than-equal"), width = 4)
  })
  
  
  
  
  
  
  
  ProbabilityCalculator_Normal_MeanPopulation = eventReactive(
    eventExpr = input$ProbabilityCalculator_Normal_UpdateButton,
    valueExpr = {input$ProbabilityCalculator_Normal_MeanPopulation},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  ProbabilityCalculator_Normal_SDPopulation = eventReactive(
    eventExpr = input$ProbabilityCalculator_Normal_UpdateButton,
    valueExpr = {input$ProbabilityCalculator_Normal_SDPopulation},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  ProbabilityCalculator_Normal_LowerBound = eventReactive(
    eventExpr = input$ProbabilityCalculator_Normal_UpdateButton,
    valueExpr = {input$ProbabilityCalculator_Normal_LowerBound},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  ProbabilityCalculator_Normal_UpperBound = eventReactive(
    eventExpr = input$ProbabilityCalculator_Normal_UpdateButton,
    valueExpr = {input$ProbabilityCalculator_Normal_UpperBound},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  ProbabilityCalculator_Normal_Probability = eventReactive(
    eventExpr = input$ProbabilityCalculator_Normal_UpdateButton,
    valueExpr = {pnorm(q = input$ProbabilityCalculator_Normal_UpperBound, mean = input$ProbabilityCalculator_Normal_MeanPopulation, sd = input$ProbabilityCalculator_Normal_SDPopulation) - pnorm(q = input$ProbabilityCalculator_Normal_LowerBound, mean = input$ProbabilityCalculator_Normal_MeanPopulation, sd = input$ProbabilityCalculator_Normal_SDPopulation)},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  output$ProbabilityCalculator_Normal_MeanPopulation = renderValueBox({
    ProbabilityCalculator_Normal_MeanPopulation() %>%
      valueBox(subtitle = "Mean", icon = icon(name = "u"), width = 4)
  })
  
  output$ProbabilityCalculator_Normal_SDPopulation = renderValueBox({
    ProbabilityCalculator_Normal_SDPopulation() %>%
      valueBox(subtitle = "Standard Deviation", icon = icon(name = "o"), width = 4)
  })
  
  output$ProbabilityCalculator_Normal_LowerBound = renderValueBox({
    ProbabilityCalculator_Normal_LowerBound() %>%
      valueBox(subtitle = "Lower Bound", width = 4)
  })
  
  output$ProbabilityCalculator_Normal_UpperBound = renderValueBox({
    ProbabilityCalculator_Normal_UpperBound() %>%
      valueBox(subtitle = "Upper Bound", width = 4)
  })
  
  output$ProbabilityCalculator_Normal_Probability = renderValueBox({
    ProbabilityCalculator_Normal_Probability() %>%
      percent(digits = 2) %>%
      valueBox(subtitle = paste("Probabilty", "", sep = ""), color = "green", width = 4)
  })
  
  output$ProbabilityCalculator_Normal_Plot = renderPlot({
    dataX = data.frame(x = seq(
      from = ProbabilityCalculator_Normal_MeanPopulation() - (5 * ProbabilityCalculator_Normal_SDPopulation()),
      to = ProbabilityCalculator_Normal_MeanPopulation() + (5 * ProbabilityCalculator_Normal_SDPopulation()), length.out = 100)) %>% mutate(y = dnorm(x = x, mean = ConfidenceIntervals_OneSample_MeanNormal_MeanSample(), sd = ProbabilityCalculator_Normal_SDPopulation()))
    
    ggplot(data = dataX, aes(x = x, y = y)) +
      geom_area(fill = "lightgreen") +
      gghighlight(x > ProbabilityCalculator_Normal_LowerBound(), x < ProbabilityCalculator_Normal_UpperBound()) +
      ylab(label = "Probability Density")
  })

  

  
  
  
  
  
  ConfidenceIntervals_OneSample_MeanNormal_MeanSample = eventReactive(
    eventExpr = input$ConfidenceIntervals_OneSample_MeanNormal_UpdateButton,
    valueExpr = {input$ConfidenceIntervals_OneSample_MeanNormal_MeanSample},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  ConfidenceIntervals_OneSample_MeanNormal_SDPopulation = eventReactive(
    eventExpr = input$ConfidenceIntervals_OneSample_MeanNormal_UpdateButton,
    valueExpr = {input$ConfidenceIntervals_OneSample_MeanNormal_SDPopulation},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  ConfidenceIntervals_OneSample_MeanNormal_SampleSize = eventReactive(
    eventExpr = input$ConfidenceIntervals_OneSample_MeanNormal_UpdateButton,
    valueExpr = {input$ConfidenceIntervals_OneSample_MeanNormal_SampleSize},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  ConfidenceIntervals_OneSample_MeanNormal_StandardError = eventReactive(
    eventExpr = input$ConfidenceIntervals_OneSample_MeanNormal_UpdateButton,
    valueExpr = {input$ConfidenceIntervals_OneSample_MeanNormal_SDPopulation / sqrt(input$ConfidenceIntervals_OneSample_MeanNormal_SampleSize)},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  ConfidenceIntervals_OneSample_MeanNormal_LowerBound = eventReactive(
    eventExpr = input$ConfidenceIntervals_OneSample_MeanNormal_UpdateButton,
    valueExpr = {
      (input$ConfidenceIntervals_OneSample_MeanNormal_MeanSample - (abs(qnorm(p = (100 - input$ConfidenceIntervals_OneSample_MeanNormal_ConfidenceLevel) / 200, mean = 0, sd = 1) * input$ConfidenceIntervals_OneSample_MeanNormal_SDPopulation) / sqrt(input$ConfidenceIntervals_OneSample_MeanNormal_SampleSize))) %>%
        round(digits = 3)
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  ConfidenceIntervals_OneSample_MeanNormal_UpperBound = eventReactive(
    eventExpr = input$ConfidenceIntervals_OneSample_MeanNormal_UpdateButton,
    valueExpr = {
      (input$ConfidenceIntervals_OneSample_MeanNormal_MeanSample + (abs(qnorm(p = (100 - input$ConfidenceIntervals_OneSample_MeanNormal_ConfidenceLevel) / 200, mean = 0, sd = 1) * input$ConfidenceIntervals_OneSample_MeanNormal_SDPopulation) / sqrt(input$ConfidenceIntervals_OneSample_MeanNormal_SampleSize))) %>%
        round(digits = 3)
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  output$ConfidenceIntervals_OneSample_MeanNormal_LowerBound = renderValueBox({
    ConfidenceIntervals_OneSample_MeanNormal_LowerBound() %>%
      valueBox(subtitle = "Lower Bound", icon = icon(name = "l"), width = 4)
  })

  output$ConfidenceIntervals_OneSample_MeanNormal_UpperBound = renderValueBox({
    ConfidenceIntervals_OneSample_MeanNormal_UpperBound() %>%
      valueBox(subtitle = "Upper Bound", icon = icon(name = "u"), width = 4)
  })

  output$ConfidenceIntervals_OneSample_MeanNormal_Plot = renderPlot({
    dataX = data.frame(x = seq(
      from = ConfidenceIntervals_OneSample_MeanNormal_MeanSample() - (5 * ConfidenceIntervals_OneSample_MeanNormal_StandardError()),
      to = ConfidenceIntervals_OneSample_MeanNormal_MeanSample() + (5 * ConfidenceIntervals_OneSample_MeanNormal_StandardError()), length.out = 100)) %>% mutate(y = dnorm(x = x, mean = ConfidenceIntervals_OneSample_MeanNormal_MeanSample(), sd = ConfidenceIntervals_OneSample_MeanNormal_StandardError()))
    
    ggplot(data = dataX, aes(x = x, y = y)) +
      geom_area(fill = "lightgreen") +
      gghighlight(x > ConfidenceIntervals_OneSample_MeanNormal_LowerBound(), x < ConfidenceIntervals_OneSample_MeanNormal_UpperBound()) +
      ylab(label = "Probability Density")
  })
  

  
  ##
  ConfidenceIntervals_OneSample_MeanT_LowerBound = eventReactive(
    eventExpr = input$ConfidenceIntervals_OneSample_MeanT_UpdateButton,
    valueExpr = {
      (input$ConfidenceIntervals_OneSample_MeanT_MeanSample - (abs(qt(p = (100 - input$ConfidenceIntervals_OneSample_MeanT_ConfidenceLevel) / 200, df = input$ConfidenceIntervals_OneSample_MeanT_SampleSize - 1) * input$ConfidenceIntervals_OneSample_MeanT_SDSample) / sqrt(input$ConfidenceIntervals_OneSample_MeanT_SampleSize))) %>%
        round(digits = 3)
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  ConfidenceIntervals_OneSample_MeanT_UpperBound = eventReactive(
    eventExpr = input$ConfidenceIntervals_OneSample_MeanT_UpdateButton,
    valueExpr = {
      (input$ConfidenceIntervals_OneSample_MeanT_MeanSample + (abs(qt(p = (100 - input$ConfidenceIntervals_OneSample_MeanT_ConfidenceLevel) / 200, df = input$ConfidenceIntervals_OneSample_MeanT_SampleSize - 1) * input$ConfidenceIntervals_OneSample_MeanT_SDSample) / sqrt(input$ConfidenceIntervals_OneSample_MeanT_SampleSize))) %>%
        round(digits = 3)
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  output$ConfidenceIntervals_OneSample_MeanT_LowerBound = renderValueBox({
    ConfidenceIntervals_OneSample_MeanT_LowerBound() %>%
      valueBox(subtitle = "Lower Bound", icon = icon(name = "l"), width = 4)
  })
  
  output$ConfidenceIntervals_OneSample_MeanT_UpperBound = renderValueBox({
    ConfidenceIntervals_OneSample_MeanT_UpperBound() %>%
      valueBox(subtitle = "Upper Bound", icon = icon(name = "u"), width = 4)
  })
  
  
  
  
  
  ##
  ConfidenceIntervals_OneSample_Proportion_LowerBound = eventReactive(
    eventExpr = input$ConfidenceIntervals_OneSample_Proportion_UpdateButton,
    valueExpr = {
      (input$ConfidenceIntervals_OneSample_Proportion_SampleProportion - (abs((qnorm(p = (100 - input$ConfidenceIntervals_OneSample_Proportion_ConfidenceLevel) / 200, mean = 0, sd = 1) * sqrt(input$ConfidenceIntervals_OneSample_Proportion_SampleProportion * (1 - input$ConfidenceIntervals_OneSample_Proportion_SampleProportion) / input$ConfidenceIntervals_OneSample_Proportion_SampleSize))))) %>%
        round(digits = 3)
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  ConfidenceIntervals_OneSample_Proportion_UpperBound = eventReactive(
    eventExpr = input$ConfidenceIntervals_OneSample_Proportion_UpdateButton,
    valueExpr = {
      (input$ConfidenceIntervals_OneSample_Proportion_SampleProportion + (abs((qnorm(p = (100 - input$ConfidenceIntervals_OneSample_Proportion_ConfidenceLevel) / 200, mean = 0, sd = 1) * sqrt(input$ConfidenceIntervals_OneSample_Proportion_SampleProportion * (1 - input$ConfidenceIntervals_OneSample_Proportion_SampleProportion) / input$ConfidenceIntervals_OneSample_Proportion_SampleSize))))) %>%
        round(digits = 3)
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  output$ConfidenceIntervals_OneSample_Proportion_LowerBound = renderValueBox({
    ConfidenceIntervals_OneSample_Proportion_LowerBound() %>%
      valueBox(subtitle = "Lower Bound", icon = icon(name = "l"), width = 4)
  })
  
  output$ConfidenceIntervals_OneSample_Proportion_UpperBound = renderValueBox({
    ConfidenceIntervals_OneSample_Proportion_UpperBound() %>%
      valueBox(subtitle = "Upper Bound", icon = icon(name = "u"), width = 4)
  })
  
  
  ##
  ConfidenceIntervals_OneSample_Variance_LowerBound = eventReactive(
    eventExpr = input$ConfidenceIntervals_OneSample_Variance_UpdateButton,
    valueExpr = {
      ((input$ConfidenceIntervals_OneSample_Variance_SampleSize - 1) * (input$ConfidenceIntervals_OneSample_Variance_SampleVariance) / qchisq(p = 1 - ((1 - input$ConfidenceIntervals_OneSample_Variance_ConfidenceLevel / 100) / 2), df = input$ConfidenceIntervals_OneSample_Variance_SampleSize - 1)) %>%
        round(digits = 3)
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  ConfidenceIntervals_OneSample_Variance_UpperBound = eventReactive(
    eventExpr = input$ConfidenceIntervals_OneSample_Variance_UpdateButton,
    valueExpr = {
      ((input$ConfidenceIntervals_OneSample_Variance_SampleSize - 1) * (input$ConfidenceIntervals_OneSample_Variance_SampleVariance) / qchisq(p = (1 - input$ConfidenceIntervals_OneSample_Variance_ConfidenceLevel / 100) / 2, df = input$ConfidenceIntervals_OneSample_Variance_SampleSize - 1)) %>%
        round(digits = 3)
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  output$ConfidenceIntervals_OneSample_Variance_LowerBound = renderValueBox({
    ConfidenceIntervals_OneSample_Variance_LowerBound() %>%
      valueBox(subtitle = "Lower Bound", icon = icon(name = "l"), width = 4)
  })
  
  output$ConfidenceIntervals_OneSample_Variance_UpperBound = renderValueBox({
    ConfidenceIntervals_OneSample_Variance_UpperBound() %>%
      valueBox(subtitle = "Upper Bound", icon = icon(name = "u"), width = 4)
  })
  
  
  
  
  ##
  HypothesisTest_OneSample_MeanNormal_TestType = eventReactive(
    eventExpr = input$HypothesisTest_OneSample_MeanNormal_UpdateButton,
    valueExpr = {input$HypothesisTest_OneSample_MeanNormal_TestType},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  HypothesisTest_OneSample_MeanNormal_MeanPopulation = eventReactive(
    eventExpr = input$HypothesisTest_OneSample_MeanNormal_UpdateButton,
    valueExpr = {input$HypothesisTest_OneSample_MeanNormal_MeanPopulation},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  HypothesisTest_OneSample_MeanNormal_MeanSample = eventReactive(
    eventExpr = input$HypothesisTest_OneSample_MeanNormal_UpdateButton,
    valueExpr = {input$HypothesisTest_OneSample_MeanNormal_MeanSample},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  HypothesisTest_OneSample_MeanNormal_SDPopulation = eventReactive(
    eventExpr = input$HypothesisTest_OneSample_MeanNormal_UpdateButton,
    valueExpr = {input$HypothesisTest_OneSample_MeanNormal_SDPopulation},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  HypothesisTest_OneSample_MeanNormal_SampleSize = eventReactive(
    eventExpr = input$HypothesisTest_OneSample_MeanNormal_UpdateButton,
    valueExpr = {input$HypothesisTest_OneSample_MeanNormal_SampleSize},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  HypothesisTest_OneSample_MeanNormal_TestStatistic = eventReactive(
    eventExpr = input$HypothesisTest_OneSample_MeanNormal_UpdateButton,
    valueExpr = {
      ((HypothesisTest_OneSample_MeanNormal_MeanSample() - HypothesisTest_OneSample_MeanNormal_MeanPopulation()) /
        (HypothesisTest_OneSample_MeanNormal_SDPopulation() / sqrt(HypothesisTest_OneSample_MeanNormal_SampleSize()))) %>%
        round(digits = 3)
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  HypothesisTest_OneSample_MeanNormal_Alpha = eventReactive(
    eventExpr = input$HypothesisTest_OneSample_MeanNormal_UpdateButton,
    valueExpr = {input$HypothesisTest_OneSample_MeanNormal_Alpha},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  HypothesisTest_OneSample_MeanNormal_HypothesisNull = eventReactive(
    eventExpr = input$HypothesisTest_OneSample_MeanNormal_UpdateButton,
    valueExpr = {paste("\u03H\u2080", ": \u03BC = ", HypothesisTest_OneSample_MeanNormal_MeanPopulation(), sep = "")},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  HypothesisTest_OneSample_MeanNormal_HypothesisAlternative = eventReactive(
    eventExpr = input$HypothesisTest_OneSample_MeanNormal_UpdateButton,
    valueExpr = {
      if (HypothesisTest_OneSample_MeanNormal_TestType() == "One Sided - Left") {
        paste("\u03H\u2090", ": \u03BC < ", HypothesisTest_OneSample_MeanNormal_MeanPopulation(), sep = "")
      } else if (HypothesisTest_OneSample_MeanNormal_TestType() == "One Sided - Right") {
        paste("\u03H\u2090", ": \u03BC > ", HypothesisTest_OneSample_MeanNormal_MeanPopulation(), sep = "")
      } else if (HypothesisTest_OneSample_MeanNormal_TestType() == "Two Sided") {
        paste("\u03H\u2090", ": \u03BC ≠ ", HypothesisTest_OneSample_MeanNormal_MeanPopulation(), sep = "")
      }
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  HypothesisTest_OneSample_MeanNormal_CriticalValue = eventReactive(
    eventExpr = input$HypothesisTest_OneSample_MeanNormal_UpdateButton,
    valueExpr = {
      if (HypothesisTest_OneSample_MeanNormal_TestType() == "One Sided - Left") {
        qnorm(p = HypothesisTest_OneSample_MeanNormal_Alpha(), mean = 0, sd = 1) %>%
          round(digits = 4)
      } else if (HypothesisTest_OneSample_MeanNormal_TestType() == "One Sided - Right") {
        qnorm(p = 1 - HypothesisTest_OneSample_MeanNormal_Alpha(), mean = 0, sd = 1) %>%
          round(digits = 4)
      } else if (HypothesisTest_OneSample_MeanNormal_TestType() == "Two Sided") {
        paste(
        qnorm(p = HypothesisTest_OneSample_MeanNormal_Alpha() / 2, mean = 0, sd = 1) %>%
          round(digits = 4),
        ", ",
        qnorm(p = 1 - (HypothesisTest_OneSample_MeanNormal_Alpha() / 2), mean = 0, sd = 1) %>%
          round(digits = 4),
        sep = "")
      }
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  HypothesisTest_OneSample_MeanNormal_PValue = eventReactive(
    eventExpr = input$HypothesisTest_OneSample_MeanNormal_UpdateButton,
    valueExpr = {
      if (HypothesisTest_OneSample_MeanNormal_TestType() == "One Sided - Left") {
        pnorm(q = HypothesisTest_OneSample_MeanNormal_TestStatistic(), mean = 0, sd = 1) %>%
          round(digits = 4)
      } else if (HypothesisTest_OneSample_MeanNormal_TestType() == "One Sided - Right") {
        (1 - pnorm(q = HypothesisTest_OneSample_MeanNormal_TestStatistic(), mean = 0, sd = 1)) %>%
          round(digits = 4)
      } else if (HypothesisTest_OneSample_MeanNormal_TestType() == "Two Sided") {
        if (HypothesisTest_OneSample_MeanNormal_TestStatistic() <= 0) {
          (2 * pnorm(q = HypothesisTest_OneSample_MeanNormal_TestStatistic(), mean = 0, sd = 1)) %>%
            round(digits = 4)
        } else if (HypothesisTest_OneSample_MeanNormal_TestStatistic() > 0) {
          (2 * (1 - pnorm(q = HypothesisTest_OneSample_MeanNormal_TestStatistic(), mean = 0, sd = 1))) %>%
            round(digits = 4)
        }
      }
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  HypothesisTest_OneSample_MeanNormal_FinalDecision = eventReactive(
    eventExpr = input$HypothesisTest_OneSample_MeanNormal_UpdateButton,
    valueExpr = {
    if (HypothesisTest_OneSample_MeanNormal_PValue() >= HypothesisTest_OneSample_MeanNormal_Alpha() &&
        input$HypothesisTest_OneSample_MeanNormal_TestType == "One Sided - Left") {
      paste("FAIL TO REJECT the Null Hypothesis. There is NOT enough evidence to conclude that the Population Mean (\u03BC) is less than than ", HypothesisTest_OneSample_MeanNormal_MeanPopulation(), ".", sep = "")
    } else if (HypothesisTest_OneSample_MeanNormal_PValue() < HypothesisTest_OneSample_MeanNormal_Alpha() &&
        input$HypothesisTest_OneSample_MeanNormal_TestType == "One Sided - Left") {
      paste("REJECT the Null Hypothesis. There IS enough evidence to conclude that the Population Mean (\u03BC) is lower than ", HypothesisTest_OneSample_MeanNormal_MeanPopulation(), ".", sep = "")
    } else if (HypothesisTest_OneSample_MeanNormal_PValue() >= HypothesisTest_OneSample_MeanNormal_Alpha() &&
              input$HypothesisTest_OneSample_MeanNormal_TestType == "One Sided - Right") {
      paste("FAIL TO REJECT the Null Hypothesis. There is NOT enough evidence to conclude that the Population Mean (\u03BC) is greater than ", HypothesisTest_OneSample_MeanNormal_MeanPopulation(), ".", sep = "")
    } else if (HypothesisTest_OneSample_MeanNormal_PValue() < HypothesisTest_OneSample_MeanNormal_Alpha() &&
               input$HypothesisTest_OneSample_MeanNormal_TestType == "One Sided - Right") {
      paste("REJECT the Null Hypothesis. There IS enough evidence to conclude that the Population Mean (\u03BC) is greater than ", HypothesisTest_OneSample_MeanNormal_MeanPopulation(), ".", sep = "")
    } else if (HypothesisTest_OneSample_MeanNormal_PValue() >= HypothesisTest_OneSample_MeanNormal_Alpha() &&
               input$HypothesisTest_OneSample_MeanNormal_TestType == "Two Sided") {
      paste("FAIL TO REJECT the Null Hypothesis. There is NOT enough evidence to conclude that the Population Mean (\u03BC) is different than ", HypothesisTest_OneSample_MeanNormal_MeanPopulation(), ".", sep = "")
    } else if (HypothesisTest_OneSample_MeanNormal_PValue() < HypothesisTest_OneSample_MeanNormal_Alpha() &&
              input$HypothesisTest_OneSample_MeanNormal_TestType == "Two Sided") {
      paste("REJECT the Null Hypothesis. There IS enough evidence to conclude that the Population Mean (\u03BC) is different than ", HypothesisTest_OneSample_MeanNormal_MeanPopulation(), ".", sep = "")
    }
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  output$HypothesisTest_OneSample_MeanNormal_Final = renderText({
    tt = data.frame(
      Metric = c(
        "Test Type",
        "Null Hypothesis",
        "Alternative Hypothesis",
        "Population Mean",
        "Population Standard Deviation",
        "Sample Mean",
        "Sample Size",
        "Test Statistic",
        "Critical Value(s)",
        "Alpha",
        "P Value",
        "Final Decision"
      ),
      Value = c(
        HypothesisTest_OneSample_MeanNormal_TestType(),
        HypothesisTest_OneSample_MeanNormal_HypothesisNull(),
        HypothesisTest_OneSample_MeanNormal_HypothesisAlternative(),
        HypothesisTest_OneSample_MeanNormal_MeanPopulation(),
        HypothesisTest_OneSample_MeanNormal_SDPopulation(),
        HypothesisTest_OneSample_MeanNormal_MeanSample(),
        HypothesisTest_OneSample_MeanNormal_SampleSize(),
        HypothesisTest_OneSample_MeanNormal_TestStatistic(),
        HypothesisTest_OneSample_MeanNormal_CriticalValue(),
        HypothesisTest_OneSample_MeanNormal_Alpha(),
        HypothesisTest_OneSample_MeanNormal_PValue(),
        HypothesisTest_OneSample_MeanNormal_FinalDecision()
      )
    )
    
    last_rowTT = length(tt$Metric)
    
    tt %>%
      kable(format = "html", align = "ll", col.names = c("Metric", "Value")) %>%
      kable_styling(bootstrap_options = c("hover", "responsive", "striped")) %>%
      scroll_box(width = "100%", height = "100%") %>%
      row_spec(row = last_rowTT, bold = TRUE, hline_after = TRUE)
  })
  
  
  
  

  
  
  
  
  
  ##
  HypothesisTest_OneSample_MeanT_TestType = eventReactive(
    eventExpr = input$HypothesisTest_OneSample_MeanT_UpdateButton,
    valueExpr = {input$HypothesisTest_OneSample_MeanT_TestType},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  HypothesisTest_OneSample_MeanT_MeanPopulation = eventReactive(
    eventExpr = input$HypothesisTest_OneSample_MeanT_UpdateButton,
    valueExpr = {input$HypothesisTest_OneSample_MeanT_MeanPopulation},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  HypothesisTest_OneSample_MeanT_MeanSample = eventReactive(
    eventExpr = input$HypothesisTest_OneSample_MeanT_UpdateButton,
    valueExpr = {input$HypothesisTest_OneSample_MeanT_MeanSample},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  HypothesisTest_OneSample_MeanT_SDSample = eventReactive(
    eventExpr = input$HypothesisTest_OneSample_MeanT_UpdateButton,
    valueExpr = {input$HypothesisTest_OneSample_MeanT_SDSample},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  HypothesisTest_OneSample_MeanT_SampleSize = eventReactive(
    eventExpr = input$HypothesisTest_OneSample_MeanT_UpdateButton,
    valueExpr = {input$HypothesisTest_OneSample_MeanT_SampleSize},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  HypothesisTest_OneSample_MeanT_DegreesOfFreedom = eventReactive(
    eventExpr = input$HypothesisTest_OneSample_MeanT_UpdateButton,
    valueExpr = {input$HypothesisTest_OneSample_MeanT_SampleSize - 1},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  HypothesisTest_OneSample_MeanT_TestStatistic = eventReactive(
    eventExpr = input$HypothesisTest_OneSample_MeanT_UpdateButton,
    valueExpr = {
      ((HypothesisTest_OneSample_MeanT_MeanSample() - HypothesisTest_OneSample_MeanT_MeanPopulation()) /
         (HypothesisTest_OneSample_MeanT_SDSample() / sqrt(HypothesisTest_OneSample_MeanT_SampleSize()))) %>%
        round(digits = 3)
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  HypothesisTest_OneSample_MeanT_Alpha = eventReactive(
    eventExpr = input$HypothesisTest_OneSample_MeanT_UpdateButton,
    valueExpr = {input$HypothesisTest_OneSample_MeanT_Alpha},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  HypothesisTest_OneSample_MeanT_HypothesisNull = eventReactive(
    eventExpr = input$HypothesisTest_OneSample_MeanT_UpdateButton,
    valueExpr = {paste("\u03H\u2080", ": \u03BC = ", HypothesisTest_OneSample_MeanT_MeanPopulation(), sep = "")},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  HypothesisTest_OneSample_MeanT_HypothesisAlternative = eventReactive(
    eventExpr = input$HypothesisTest_OneSample_MeanT_UpdateButton,
    valueExpr = {
      if (HypothesisTest_OneSample_MeanT_TestType() == "One Sided - Left") {
        paste("\u03H\u2090", ": \u03BC < ", HypothesisTest_OneSample_MeanT_MeanPopulation(), sep = "")
      } else if (HypothesisTest_OneSample_MeanT_TestType() == "One Sided - Right") {
        paste("\u03H\u2090", ": \u03BC > ", HypothesisTest_OneSample_MeanT_MeanPopulation(), sep = "")
      } else if (HypothesisTest_OneSample_MeanT_TestType() == "Two Sided") {
        paste("\u03H\u2090", ": \u03BC ≠ ", HypothesisTest_OneSample_MeanT_MeanPopulation(), sep = "")
      }
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  HypothesisTest_OneSample_MeanT_CriticalValue = eventReactive(
    eventExpr = input$HypothesisTest_OneSample_MeanT_UpdateButton,
    valueExpr = {
      if (HypothesisTest_OneSample_MeanT_TestType() == "One Sided - Left") {
        qt(p = HypothesisTest_OneSample_MeanT_Alpha(), df = HypothesisTest_OneSample_MeanT_DegreesOfFreedom()) %>%
          round(digits = 4)
      } else if (HypothesisTest_OneSample_MeanT_TestType() == "One Sided - Right") {
        qt(p = 1 - HypothesisTest_OneSample_MeanT_Alpha(), df = HypothesisTest_OneSample_MeanT_DegreesOfFreedom()) %>%
          round(digits = 4)
      } else if (HypothesisTest_OneSample_MeanT_TestType() == "Two Sided") {
        paste(
          qt(p = HypothesisTest_OneSample_MeanT_Alpha() / 2, df = HypothesisTest_OneSample_MeanT_DegreesOfFreedom()) %>%
            round(digits = 4),
          ", ",
          qt(p = 1 - (HypothesisTest_OneSample_MeanT_Alpha() / 2), df = HypothesisTest_OneSample_MeanT_DegreesOfFreedom()) %>%
            round(digits = 4),
          sep = "")
      }
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  HypothesisTest_OneSample_MeanT_PValue = eventReactive(
    eventExpr = input$HypothesisTest_OneSample_MeanT_UpdateButton,
    valueExpr = {
      if (HypothesisTest_OneSample_MeanT_TestType() == "One Sided - Left") {
        pt(q = HypothesisTest_OneSample_MeanT_TestStatistic(), df = HypothesisTest_OneSample_MeanT_DegreesOfFreedom()) %>%
          round(digits = 4)
      } else if (HypothesisTest_OneSample_MeanT_TestType() == "One Sided - Right") {
        (1 - pt(q = HypothesisTest_OneSample_MeanT_TestStatistic(), df = HypothesisTest_OneSample_MeanT_DegreesOfFreedom())) %>%
          round(digits = 4)
      } else if (HypothesisTest_OneSample_MeanT_TestType() == "Two Sided") {
        if (HypothesisTest_OneSample_MeanT_TestStatistic() <= 0) {
          (2 * pt(q = HypothesisTest_OneSample_MeanT_TestStatistic(), df = HHypothesisTest_OneSample_MeanT_DegreesOfFreedom())) %>%
            round(digits = 4)
        } else if (HypothesisTest_OneSample_MeanT_TestStatistic() > 0) {
          (2 * (1 - pt(q = HypothesisTest_OneSample_MeanT_TestStatistic(), df = HypothesisTest_OneSample_MeanT_DegreesOfFreedom()))) %>%
            round(digits = 4)
        }
      }
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  HypothesisTest_OneSample_MeanT_FinalDecision = eventReactive(
    eventExpr = input$HypothesisTest_OneSample_MeanT_UpdateButton,
    valueExpr = {
      if (HypothesisTest_OneSample_MeanT_PValue() >= HypothesisTest_OneSample_MeanT_Alpha() &&
          input$HypothesisTest_OneSample_MeanT_TestType == "One Sided - Left") {
        paste("FAIL TO REJECT the Null Hypothesis. There is NOT enough evidence to conclude that the Population Mean (\u03BC) is less than than ", HypothesisTest_OneSample_MeanT_MeanPopulation(), ".", sep = "")
      } else if (HypothesisTest_OneSample_MeanT_PValue() < HypothesisTest_OneSample_MeanT_Alpha() &&
                 input$HypothesisTest_OneSample_MeanT_TestType == "One Sided - Left") {
        paste("REJECT the Null Hypothesis. There IS enough evidence to conclude that the Population Mean (\u03BC) is lower than ", HypothesisTest_OneSample_MeanT_MeanPopulation(), ".", sep = "")
      } else if (HypothesisTest_OneSample_MeanT_PValue() >= HypothesisTest_OneSample_MeanT_Alpha() &&
                 input$HypothesisTest_OneSample_MeanT_TestType == "One Sided - Right") {
        paste("FAIL TO REJECT the Null Hypothesis. There is NOT enough evidence to conclude that the Population Mean (\u03BC) is greater than ", HypothesisTest_OneSample_MeanT_MeanPopulation(), ".", sep = "")
      } else if (HypothesisTest_OneSample_MeanT_PValue() < HypothesisTest_OneSample_MeanT_Alpha() &&
                 input$HypothesisTest_OneSample_MeanT_TestType == "One Sided - Right") {
        paste("REJECT the Null Hypothesis. There IS enough evidence to conclude that the Population Mean (\u03BC) is greater than ", HypothesisTest_OneSample_MeanT_MeanPopulation(), ".", sep = "")
      } else if (HypothesisTest_OneSample_MeanT_PValue() >= HypothesisTest_OneSample_MeanT_Alpha() &&
                 input$HypothesisTest_OneSample_MeanT_TestType == "Two Sided") {
        paste("FAIL TO REJECT the Null Hypothesis. There is NOT enough evidence to conclude that the Population Mean (\u03BC) is different than ", HypothesisTest_OneSample_MeanT_MeanPopulation(), ".", sep = "")
      } else if (HypothesisTest_OneSample_MeanT_PValue() < HypothesisTest_OneSample_MeanT_Alpha() &&
                 input$HypothesisTest_OneSample_MeanT_TestType == "Two Sided") {
        paste("REJECT the Null Hypothesis. There IS enough evidence to conclude that the Population Mean (\u03BC) is different than ", HypothesisTest_OneSample_MeanT_MeanPopulation(), ".", sep = "")
      }
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  output$HypothesisTest_OneSample_MeanT_Final = renderText({
    tt = data.frame(
      Metric = c(
        "Test Type",
        "Null Hypothesis",
        "Alternative Hypothesis",
        "Population Mean",
        "Sample Mean",
        "Sample Standard Deviation",
        "Sample Size",
        "Degrees of Freedom",
        "Test Statistic",
        "Critical Value(s)",
        "Alpha",
        "P Value",
        "Final Decision"
      ),
      Value = c(
        HypothesisTest_OneSample_MeanT_TestType(),
        HypothesisTest_OneSample_MeanT_HypothesisNull(),
        HypothesisTest_OneSample_MeanT_HypothesisAlternative(),
        HypothesisTest_OneSample_MeanT_MeanPopulation(),
        HypothesisTest_OneSample_MeanT_MeanSample(),
        HypothesisTest_OneSample_MeanT_SDSample(),
        HypothesisTest_OneSample_MeanT_SampleSize(),
        HypothesisTest_OneSample_MeanT_DegreesOfFreedom(),
        HypothesisTest_OneSample_MeanT_TestStatistic(),
        HypothesisTest_OneSample_MeanT_CriticalValue(),
        HypothesisTest_OneSample_MeanT_Alpha(),
        HypothesisTest_OneSample_MeanT_PValue(),
        HypothesisTest_OneSample_MeanT_FinalDecision()
      )
    )
    
    last_rowTT = length(tt$Metric)
    
    tt %>%
      kable(format = "html", align = "ll", col.names = c("Metric", "Value")) %>%
      kable_styling(bootstrap_options = c("hover", "responsive", "striped")) %>%
      scroll_box(width = "100%", height = "100%") %>%
      row_spec(row = last_rowTT, bold = TRUE, hline_after = TRUE)
  })
  
  
  
  
  
  
  
  ## One Sample Proportion
  HypothesisTest_OneSample_Proportion_TestType = eventReactive(
    eventExpr = input$HypothesisTest_OneSample_Proportion_UpdateButton,
    valueExpr = {input$HypothesisTest_OneSample_Proportion_TestType},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  HypothesisTest_OneSample_Proportion_ProportionPopulation = eventReactive(
    eventExpr = input$HypothesisTest_OneSample_Proportion_UpdateButton,
    valueExpr = {input$HypothesisTest_OneSample_Proportion_ProportionPopulation},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )

  HypothesisTest_OneSample_Proportion_SampleSize = eventReactive(
    eventExpr = input$HypothesisTest_OneSample_Proportion_UpdateButton,
    valueExpr = {input$HypothesisTest_OneSample_Proportion_SampleSize},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  HypothesisTest_OneSample_Proportion_ProportionSample = eventReactive(
    eventExpr = input$HypothesisTest_OneSample_Proportion_UpdateButton,
    valueExpr = {input$HypothesisTest_OneSample_Proportion_ProportionSample},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  HypothesisTest_OneSample_Proportion_TestStatistic = eventReactive(
    eventExpr = input$HypothesisTest_OneSample_Proportion_UpdateButton,
    valueExpr = {
      ((HypothesisTest_OneSample_Proportion_ProportionSample() - HypothesisTest_OneSample_Proportion_ProportionPopulation()) /
        sqrt((HypothesisTest_OneSample_Proportion_ProportionPopulation() * (1 - HypothesisTest_OneSample_Proportion_ProportionPopulation())) / HypothesisTest_OneSample_Proportion_SampleSize())) %>%
         round(digits = 3)
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  HypothesisTest_OneSample_Proportion_Alpha = eventReactive(
    eventExpr = input$HypothesisTest_OneSample_Proportion_UpdateButton,
    valueExpr = {input$HypothesisTest_OneSample_Proportion_Alpha},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  HypothesisTest_OneSample_Proportion_HypothesisNull = eventReactive(
    eventExpr = input$HypothesisTest_OneSample_Proportion_UpdateButton,
    valueExpr = {paste("\u03H\u2080", ": p = ", HypothesisTest_OneSample_Proportion_ProportionPopulation(), sep = "")},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  HypothesisTest_OneSample_Proportion_HypothesisAlternative = eventReactive(
    eventExpr = input$HypothesisTest_OneSample_Proportion_UpdateButton,
    valueExpr = {
      if (HypothesisTest_OneSample_Proportion_TestType() == "One Sided - Left") {
        paste("\u03H\u2090", ": \u0070\u0302 < ", HypothesisTest_OneSample_Proportion_ProportionPopulation(), sep = "")
      } else if (HypothesisTest_OneSample_Proportion_TestType() == "One Sided - Right") {
        paste("\u03H\u2090", ": \u0070\u0302 > ", HypothesisTest_OneSample_Proportion_ProportionPopulation(), sep = "")
      } else if (HypothesisTest_OneSample_Proportion_TestType() == "Two Sided") {
        paste("\u03H\u2090", ": \u0070\u0302 ≠ ", HypothesisTest_OneSample_Proportion_ProportionPopulation(), sep = "")
      }
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  HypothesisTest_OneSample_Proportion_CriticalValue = eventReactive(
    eventExpr = input$HypothesisTest_OneSample_Proportion_UpdateButton,
    valueExpr = {
      if (HypothesisTest_OneSample_Proportion_TestType() == "One Sided - Left") {
        qnorm(p = HypothesisTest_OneSample_Proportion_Alpha(), mean = 0, sd = 1) %>%
          round(digits = 4)
      } else if (HypothesisTest_OneSample_Proportion_TestType() == "One Sided - Right") {
        qnorm(p = 1 - HypothesisTest_OneSample_Proportion_Alpha(), mean = 0, sd = 1) %>%
          round(digits = 4)
      } else if (HypothesisTest_OneSample_Proportion_TestType() == "Two Sided") {
        paste(
          qnorm(p = HypothesisTest_OneSample_Proportion_Alpha() / 2, mean = 0, sd = 1) %>%
            round(digits = 4),
          ", ",
          qnorm(p = 1 - (HypothesisTest_OneSample_Proportion_Alpha() / 2), mean = 0, sd = 1) %>%
            round(digits = 4),
          sep = "")
      }
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  HypothesisTest_OneSample_Proportion_PValue = eventReactive(
    eventExpr = input$HypothesisTest_OneSample_Proportion_UpdateButton,
    valueExpr = {
      if (HypothesisTest_OneSample_Proportion_TestType() == "One Sided - Left") {
        pnorm(q = HypothesisTest_OneSample_Proportion_TestStatistic(), mean = 0, sd = 1) %>%
          round(digits = 4)
      } else if (HypothesisTest_OneSample_Proportion_TestType() == "One Sided - Right") {
        (1 - pnorm(q = HypothesisTest_OneSample_Proportion_TestStatistic(), mean = 0, sd = 1)) %>%
          round(digits = 4)
      } else if (HypothesisTest_OneSample_Proportion_TestType() == "Two Sided") {
        if (HypothesisTest_OneSample_Proportion_TestStatistic() <= 0) {
          (2 * pnorm(q = HypothesisTest_OneSample_Proportion_TestStatistic(), mean = 0, sd = 1)) %>%
            round(digits = 4)
        } else if (HypothesisTest_OneSample_Proportion_TestStatistic() > 0) {
          (2 * (1 - pnorm(q = HypothesisTest_OneSample_Proportion_TestStatistic(), mean = 0, sd = 1))) %>%
            round(digits = 4)
        }
      }
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  HypothesisTest_OneSample_Proportion_FinalDecision = eventReactive(
    eventExpr = input$HypothesisTest_OneSample_Proportion_UpdateButton,
    valueExpr = {
      if (HypothesisTest_OneSample_Proportion_PValue() >= HypothesisTest_OneSample_Proportion_Alpha() &&
          input$HypothesisTest_OneSample_Proportion_TestType == "One Sided - Left") {
        paste("FAIL TO REJECT the Null Hypothesis. There is NOT enough evidence to conclude that the Population Proportion (p) is less than than ", HypothesisTest_OneSample_Proportion_ProportionPopulation(), ".", sep = "")
      } else if (HypothesisTest_OneSample_Proportion_PValue() < HypothesisTest_OneSample_Proportion_Alpha() &&
                 input$HypothesisTest_OneSample_Proportion_TestType == "One Sided - Left") {
        paste("REJECT the Null Hypothesis. There IS enough evidence to conclude that the Population Proportion (p) is lower than ", HypothesisTest_OneSample_Proportion_ProportionPopulation(), ".", sep = "")
      } else if (HypothesisTest_OneSample_Proportion_PValue() >= HypothesisTest_OneSample_Proportion_Alpha() &&
                 input$HypothesisTest_OneSample_Proportion_TestType == "One Sided - Right") {
        paste("FAIL TO REJECT the Null Hypothesis. There is NOT enough evidence to conclude that the Population Proportion (p) is greater than ", HypothesisTest_OneSample_Proportion_ProportionPopulation(), ".", sep = "")
      } else if (HypothesisTest_OneSample_Proportion_PValue() < HypothesisTest_OneSample_Proportion_Alpha() &&
                 input$HypothesisTest_OneSample_Proportion_TestType == "One Sided - Right") {
        paste("REJECT the Null Hypothesis. There IS enough evidence to conclude that the Population Proportion (p) is greater than ", HypothesisTest_OneSample_Proportion_ProportionPopulation(), ".", sep = "")
      } else if (HypothesisTest_OneSample_Proportion_PValue() >= HypothesisTest_OneSample_Proportion_Alpha() &&
                 input$HypothesisTest_OneSample_Proportion_TestType == "Two Sided") {
        paste("FAIL TO REJECT the Null Hypothesis. There is NOT enough evidence to conclude that the Population Proportion (p) is different than ", HypothesisTest_OneSample_Proportion_ProportionPopulation(), ".", sep = "")
      } else if (HypothesisTest_OneSample_Proportion_PValue() < HypothesisTest_OneSample_Proportion_Alpha() &&
                 input$HypothesisTest_OneSample_Proportion_TestType == "Two Sided") {
        paste("REJECT the Null Hypothesis. There IS enough evidence to conclude that the Population Proportion (p) is different than ", HypothesisTest_OneSample_Proportion_ProportionPopulation(), ".", sep = "")
      }
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  output$HypothesisTest_OneSample_Proportion_Final = renderText({
    tt = data.frame(
      Metric = c(
        "Test Type",
        "Null Hypothesis",
        "Alternative Hypothesis",
        "Population Proportion",
        "Sample Proportion",
        "Sample Size",
        "Test Statistic",
        "Critical Value(s)",
        "Alpha",
        "P Value",
        "Final Decision"
      ),
      Value = c(
        HypothesisTest_OneSample_Proportion_TestType(),
        HypothesisTest_OneSample_Proportion_HypothesisNull(),
        HypothesisTest_OneSample_Proportion_HypothesisAlternative(),
        HypothesisTest_OneSample_Proportion_ProportionPopulation(),
        HypothesisTest_OneSample_Proportion_ProportionSample(),
        HypothesisTest_OneSample_Proportion_SampleSize(),
        HypothesisTest_OneSample_Proportion_TestStatistic(),
        HypothesisTest_OneSample_Proportion_CriticalValue(),
        HypothesisTest_OneSample_Proportion_Alpha(),
        HypothesisTest_OneSample_Proportion_PValue(),
        HypothesisTest_OneSample_Proportion_FinalDecision()
      )
    )
    
    last_rowTT = length(tt$Metric)
    
    tt %>%
      kable(format = "html", align = "ll", col.names = c("Metric", "Value")) %>%
      kable_styling(bootstrap_options = c("hover", "responsive", "striped")) %>%
      scroll_box(width = "100%", height = "100%") %>%
      row_spec(row = last_rowTT, bold = TRUE, hline_after = TRUE) %>%
      #row_spec(row = 3, extra_css = "border-bottom: 1px solid") %>%
      column_spec(1:2, width_max = "3in")
  })
  
  
  
  
  
  Geometry_Triangle_Height = eventReactive(
    eventExpr = input$Geometry_Triangle_UpdateButton,
    valueExpr = {input$Geometry_Triangle_Height},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  Geometry_Triangle_Width = eventReactive(
    eventExpr = input$Geometry_Triangle_UpdateButton,
    valueExpr = {input$Geometry_Triangle_Width},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  Geometry_Triangle_Area = eventReactive(
    eventExpr = input$Geometry_Triangle_UpdateButton,
    valueExpr = {input$Geometry_Triangle_Height * input$Geometry_Triangle_Width / 2},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  output$Geometry_Triangle_Final = renderText({
    tt = data.frame(
      Metric = c(
        "Height",
        "Width",
        "Area (A = Height * Width / 2)"
      ),
      
      Value = c(
        Geometry_Triangle_Height(),
        Geometry_Triangle_Width(),
        Geometry_Triangle_Area()
      )
    )
    
    last_rowTT = length(tt$Metric)
    
    tt %>%
      kable(format = "html", align = "ll", col.names = c("Metric", "Value")) %>%
      kable_styling(bootstrap_options = c("hover", "responsive", "striped")) %>%
      scroll_box(width = "100%", height = "100%") %>%
      row_spec(row = last_rowTT, bold = TRUE, hline_after = TRUE) %>%
      #row_spec(row = 3, extra_css = "border-bottom: 1px solid") %>%
      column_spec(1:2, width_max = "3in")
  })
  
  
  
  Geometry_Circle_Radius = eventReactive(
    eventExpr = input$Geometry_Circle_UpdateButton,
    valueExpr = {input$Geometry_Circle_Radius},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  Geometry_Circle_Diameter = eventReactive(
    eventExpr = input$Geometry_Circle_UpdateButton,
    valueExpr = {2 * input$Geometry_Circle_Radius},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  Geometry_Circle_Circumference = eventReactive(
    eventExpr = input$Geometry_Circle_UpdateButton,
    valueExpr = {2 * pi * input$Geometry_Circle_Radius},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  Geometry_Circle_Area = eventReactive(
    eventExpr = input$Geometry_Circle_UpdateButton,
    valueExpr = {pi * (input$Geometry_Circle_Radius ^ 2)},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  output$Geometry_Circle_Final = renderText({
    tt = data.frame(
      Metric = c(
        "Radius",
        "Diameter (D = 2r)",
        "Circumference (D = 2πr)",
        "Area (A = πr²)"
      ),
      Value = c(
        Geometry_Circle_Radius(),
        Geometry_Circle_Diameter(),
        Geometry_Circle_Circumference(),
        Geometry_Circle_Area()
      )
    )
    
    last_rowTT = length(tt$Metric)
    
    tt %>%
      kable(format = "html", align = "ll", col.names = c("Metric", "Value")) %>%
      kable_styling(bootstrap_options = c("hover", "responsive", "striped")) %>%
      scroll_box(width = "100%", height = "100%") %>%
      row_spec(row = last_rowTT, bold = TRUE, hline_after = TRUE) %>%
      #row_spec(row = 3, extra_css = "border-bottom: 1px solid") %>%
      column_spec(1:2, width_max = "3in")
  })
  
  
  
  
  Geometry_Circle_Radius = eventReactive(
    eventExpr = input$Geometry_Circle_UpdateButton,
    valueExpr = {input$Geometry_Circle_Radius},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  Geometry_Circle_Diameter = eventReactive(
    eventExpr = input$Geometry_Circle_UpdateButton,
    valueExpr = {2 * input$Geometry_Circle_Radius},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  Geometry_Circle_Circumference = eventReactive(
    eventExpr = input$Geometry_Circle_UpdateButton,
    valueExpr = {2 * pi * input$Geometry_Circle_Radius},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  Geometry_Circle_Area = eventReactive(
    eventExpr = input$Geometry_Circle_UpdateButton,
    valueExpr = {pi * (input$Geometry_Circle_Radius ^ 2)},
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  output$Geometry_Circle_Final = renderText({
    tt = data.frame(
      Metric = c(
        "Radius",
        "Diameter (D = 2r)",
        "Circumference (D = 2πr)",
        "Area (A = πr²)"
      ),
      Value = c(
        Geometry_Circle_Radius(),
        Geometry_Circle_Diameter(),
        Geometry_Circle_Circumference(),
        Geometry_Circle_Area()
      )
    )
    
    last_rowTT = length(tt$Metric)
    
    tt %>%
      kable(format = "html", align = "ll", col.names = c("Metric", "Value")) %>%
      kable_styling(bootstrap_options = c("hover", "responsive", "striped")) %>%
      scroll_box(width = "100%", height = "100%") %>%
      row_spec(row = last_rowTT, bold = TRUE, hline_after = TRUE) %>%
      #row_spec(row = 3, extra_css = "border-bottom: 1px solid") %>%
      column_spec(1:2, width_max = "3in")
  })
  
  
}







shinyApp(
  ui = uix,
  server = serverx
)