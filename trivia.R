library(dplyr)
library(gghighlight)
library(ggplot2)
library(googlesheets4)
library(kableExtra)
library(knitr)
library(lubridate)
library(rlang)
library(scales)
library(shiny)
library(shinydashboard)
library(stringr)
library(tidyverse)

library(randomForest)
library(generics)
library(caret)

gs4_deauth()

Parts = read_sheet(ss = "https://docs.google.com/spreadsheets/d/1k4vAQll8rTX4jHBNEOxlEYl1MEra502bfDBahPyEYQQ", sheet = "# of Participants")

trivia = read_sheet(ss = "https://docs.google.com/spreadsheets/d/1k4vAQll8rTX4jHBNEOxlEYl1MEra502bfDBahPyEYQQ", sheet = "Trivia Questions with Answers") %>%
  arrange(Index) %>%
  filter(`Data Version` == "C", is.na(Date) == FALSE, is.na(Category) == FALSE, is.na(TeamAnswerCorrect) == FALSE) %>%
  mutate(
    Date = Date %>% as.Date(),
    `Question Number` = `Question Number` %>% as.character() %>% as.factor(),
    Round = case_when(
      `Question Number` %in% c("1", "2", "3") ~ "1",
      `Question Number` %in% c("4", "5", "6") ~ "2",
      `Question Number` %in% c("7", "8", "9") ~ "3",
      `Question Number` %in% c("Bonus") ~ "Bonus",
      `Question Number` %in% c("Halftime") ~ "Halftime",
      `Question Number` %in% c("11", "12", "13") ~ "4",
      `Question Number` %in% c("14", "15", "16") ~ "5",
      `Question Number` %in% c("17", "18", "19") ~ "6",
      .default = `Question Number`
    ) %>% as.factor(),
    `Question within Round` = case_when(
      `Question Number` %in% c("1", "4", "7", "11", "14", "17") ~ "1",
      `Question Number` %in% c("2", "5", "8", "12", "15", "18") ~ "2",
      `Question Number` %in% c("3", "6", "9", "13", "16", "19") ~ "3",
      `Question Number` %in% c("Bonus") ~ "Bonus",
      `Question Number` %in% c("Halftime") ~ "Halftime",
      .default = `Question Number`
    ) %>% as.factor(),
    `Trivia Half` = case_when(
      Round %in% c("1", "2", "3") ~ "First Half",
      Round %in% c("4", "5", "6") ~ "Second Half",
      .default = `Question Number`
    ),
    `Correct Answer` = `Correct Answer` %>% as.character(),
    `Submitted Answer` = `Submitted Answer` %>% as.character(),
    
    Points_Wagered_Original = `Points Wagered`,
    Points_Scored_Original = case_when(
      TeamAnswerCorrect == 1  ~ Points_Wagered_Original,
      .default = 0
    ),
    Points_Wasted_Original = case_when(
      TeamAnswerCorrect == 0 ~ Points_Wagered_Original,
      .default = 0
    ),
    Points_Net_Original = case_when(
      TeamAnswerCorrect == 1 ~ Points_Wagered_Original,
      .default = - Points_Wagered_Original
    ),
    
    Points_Wagered_Weighted = case_when(
      #`Trivia Half` == "First Half" ~ Points_Wagered_Original,
      `Trivia Half` == "Second Half" & Points_Wagered_Original == "2" ~ 1,
      `Trivia Half` == "Second Half" & Points_Wagered_Original == "4" ~ 3,
      `Trivia Half` == "Second Half" & Points_Wagered_Original == "6" ~ 5,
      .default = Points_Wagered_Original
    ),
    Points_Scored_Weighted = case_when(
      TeamAnswerCorrect == 1  ~ Points_Wagered_Weighted,
      .default = 0
    ),
    Points_Wasted_Weighted = case_when(
      TeamAnswerCorrect == 0 ~ Points_Wagered_Weighted,
      .default = 0
    ),
    Points_Net_Weighted = case_when(
      TeamAnswerCorrect == 1 ~ Points_Wagered_Weighted,
      .default = - Points_Wagered_Weighted
    ),
    
    
    
    Category_Level02 = case_when(
      # make a grouping for business and advertising
      str_detect(string = toupper(Category), pattern = "GEOGRAPHY|PARKS|LANDMARKS") == TRUE ~ "Geography",
      str_detect(string = toupper(Category), pattern = "BASEBALL|BASKETBALL|BOWLING|BOXING|FOOTBALL|GOLF|HOCKEY|RUNNING|SOCCER|SPORTS") == TRUE ~ "Sports",
      str_detect(string = toupper(Category), pattern = "LITERATURE|BOOKS") == TRUE ~ "Literature",
      str_detect(string = toupper(Category), pattern = "HISTORY|PRESIDENTS") == TRUE ~ "History",
      str_detect(string = toupper(Category), pattern = "MOVIES|MOVIE LINES") == TRUE ~ "Movies",
      toupper(Category) == "TV" ~ "TV",
      str_detect(string = toupper(Category), pattern = "TELEVISION") == TRUE ~ "TV",
      toupper(Category) == "DOGS" ~ "Animals",
      .default = "Other"
    ),
    Category_Level03 = case_when(
      Category_Level02 == "TV" ~ 1,
      .default = 2
    ),
    
    Freebie = case_when(
      Freebie == 1 ~ 1,
      .default = 0
    ),
    
    TeamAnswerCorrect_Text = case_when(
      TeamAnswerCorrect == 1 ~ "Correct",
      .default = "Incorrect"
    ) %>% as.factor()
    
  ) %>%
  left_join(y = Parts, b = "Date") %>%
  select(Index, Date, `Question Number`, Round, `Question within Round`, `Trivia Half`, Category, Question, `Correct Answer`, `Submitted Answer`, TeamAnswerCorrect, Points_Wagered_Original, Points_Scored_Original, Points_Wasted_Original, Points_Net_Original, Points_Wagered_Weighted, Points_Scored_Weighted, Points_Wasted_Weighted, Points_Net_Weighted, `Question Type`, `Question Type 2`, Freebie, Notes, `Top 5 Finish`, Category_Level02, Category_Level03, NumberofTeamMembers, TeamAnswerCorrect_Text) %>%
  filter(Round %in% c("Bonus", "Halftime", "Final", "Tiebreaker") == FALSE)

percent = function(x, digits = 2, format = "f", ...) {
  t = formatC(x * 100, format = format, digits = digits, ...) %>% paste("%", sep = "")
  return(t)
}

decimal = function(x) {
  t = sub(x = x, pattern = "%", replacement = "") %>% as.numeric() / 100
  return(t)
}


seed = 3
set.seed(seed = seed)

trivia_trn_index = sample(x = trivia$Index, size = nrow(trivia) * .7, replace = FALSE) %>% sort(decreasing = FALSE)

trivia_tst_index = trivia %>% filter(Index %in% trivia_trn_index == FALSE) %>% select(Index) %>% unlist() %>% unname() %>% sort(decreasing = FALSE)

trivia_trn = trivia %>% filter(Index %in% trivia_trn_index)
trivia_tst = trivia %>% filter(Index %in% trivia_tst_index)

rf_fit = train(TeamAnswerCorrect ~ `Question Type` + Round + `Question within Round` + `Trivia Half` + Category + Category_Level02 + Freebie + Points_Wagered_Weighted + NumberofTeamMembers,
               data = trivia_trn,
               method = "rf",
               trControl = trainControl(method = "cv", number = 5),
               tuneLength = 5
)

rf_fit$xlevels[["Category_Level02"]] = union(rf_fit$xlevels[["Category_Level02"]], trivia_tst$Category_Level02)
rf_fit$xlevels[["Category"]] = union(rf_fit$xlevels[["Category"]], trivia_tst$Category)
rf_fit$xlevels[["Question Type"]] = union(rf_fit$xlevels[["Question Type"]], trivia_tst$`Question Type`)
rf_fit$xlevels[["Question Type 2"]] = union(rf_fit$xlevels[["Question Type 2"]], trivia_tst$`Question Type 2`)

test_data_probabilities = predict(rf_fit,  newdata = trivia_tst)

cutoffs = seq(from = 0.5, to = 1, by = 0.01)
score_acc = rep(0, length(cutoffs))

score = function(actual, predicted) {
  1   * sum(predicted == 0 & actual == 0) +
    1   * sum(predicted == 1 & actual == 1) +
    -1  * sum(predicted == 0 & actual == 1) +
    -1  * sum(predicted == 1 & actual == 0)
}

# test cutoffs for accuracy tuned forest
#for (c in test_data_probabilities) {
#  pred = ifelse(test_data_probabilities > 0.5, yes = 1, no = 0)
#}

trivia_tst$prediction_probability = test_data_probabilities
trivia_tst$prediction = case_when(
  trivia_tst$prediction_probability > 0.5 ~ 1,
  .default = 0
)

#trivia_tst$prediction = trivia_tst$prediction %>% as.factor()

trivia_tst$model_accurate = case_when(
  trivia_tst$prediction == trivia_tst$TeamAnswerCorrect ~ 1,
  .default = 0
)



set.seed(seed = seed)
Table_Train_Int = sample(x = trivia_trn_index, size = 20, replace = FALSE)
Table_Train_Sample = trivia_trn %>%
  filter(Index %in% Table_Train_Int) %>%
  select(Date, Category, `Question Type`, Freebie, NumberofTeamMembers, TeamAnswerCorrect_Text) %>%
  arrange(Date, Category, `Question Type`, Freebie, NumberofTeamMembers, TeamAnswerCorrect_Text)

names(Table_Train_Sample) = c("Date", "Category", "Question Type", "Freebie", "Team Members", "Team Answer Correct")

kable(Table_Train_Sample, format = "html", align = "llllll") %>%
  kable_styling(bootstrap_options = c("responsive"), full_width = TRUE) %>%
  column_spec(column = c(1:ncol(Table_Train_Sample)), background = "white", color = "black", include_thead = TRUE)



Table_Test_Int = sample(x = trivia_tst_index, size = 10, replace = FALSE)
Table_Test_Sample = trivia_tst %>%
  filter(Index %in% Table_Test_Int) %>%
  mutate(
    ModelPredictionTeamAnswer  = ifelse(prediction == 1, "Correct", "Incorrect"),
    ModelPredictionProbability = prediction_probability %>% percent(),
    ModelPredictionAccuracy    = ifelse(model_accurate == 1, "Accurate", "Inaccurate")
  ) %>%
  select(Date, Category, `Question Type`, Freebie, NumberofTeamMembers, TeamAnswerCorrect_Text, ModelPredictionTeamAnswer, ModelPredictionProbability, ModelPredictionAccuracy) %>%
  arrange(Date, Category, `Question Type`, Freebie, NumberofTeamMembers, TeamAnswerCorrect_Text, ModelPredictionTeamAnswer, ModelPredictionProbability, ModelPredictionAccuracy)

names(Table_Test_Sample) = c("Date", "Category", "Question Type", "Freebie", "Team Members", "Team Answer Correct", "Model Prediction of Team Answer", "Model Prediction Probability", "Model Prediction Accuracy")

kable(Table_Test_Sample, format = "html", align = "lllllllll") %>%
  kable_styling(bootstrap_options = c("responsive"), full_width = TRUE) %>%
  column_spec(column = c(1:ncol(Table_Test_Sample)), background = "white", color = "black", include_thead = TRUE) %>%
  column_spec(column = ncol(Table_Test_Sample), background = case_when(Table_Test_Sample$`Model Prediction Accuracy` == "Accurate" ~ "green", .default = "red"), include_thead = FALSE)






#https://mastering-shiny.org/basic-ui.html

#selectInput = dropdown
#sliderInput = slider
#textInput
#passwordInput
#textAreaInput
#numericInput("num", "Number one", value = 0, min = 0, max = 100),
#dateInput("dob", "When were you born?"),
#dateRangeInput

#navbarMenu is a navbar, except with multiple options in a dropdown. example is "Data Analysis Projects"

uix = dashboardPage(
  skin = "green",
  dashboardHeader(
    title = "Trivia Data",
    disable = FALSE),  
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        text = "Analysis",
        #icon = icon(name = "brain"),
        tabName = "Analysis",
        badgeLabel = "NEW",
        selected = TRUE)
      
      
      #menuItem(
      #text = "Random Forest",
      #icon = icon(name = "music", lib = "font-awesome"),
      #tabName = "RandomForest",
      #badgeLabel = "NEW",
      #selected = FALSE)#,
      
      #menuItem(
      #text = "Question List",
      #icon = icon(name = "music", lib = "font-awesome"),
      #tabName = "Questions",
      #badgeLabel = "NEW",
      #selected = FALSE)#,
      
      
      #menuItem(
      #text = "Listening Statistics", #view most songs by artist in single year (top 5), highest score in a single year (top 5), most songs all time, highest score all time, most years appearing in spotify wrapped
      #icon = icon(name = "th"),
      #tabName = "Records",
      #badgeLabel = "NEW")
    )
  ),
  
  dashboardBody(
    #tags$head(
    #tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    #),
    tags$script(HTML("$('body').addClass('fixed');")),
    
    tabItems(
      # First tab content
      tabItem(tabName = "Analysis",
              
              fluidRow(
                box(width = 6,
                    radioButtons(
                      inputId = "Include_Bonus",
                      label = "Include Question of the Week:",
                      choiceNames = c("Include Question of the Week", "Exclude Question of the Week"),
                      choiceValues = c(1, 2),
                      #selected = c(1, 2)
                      selected = c(2)
                    ),
                    actionButton(inputId = "UpdateTT", label = "Update", width = "100%")
                ),
                
                
              ),
              
              fluidRow(width = 12,
                       valueBoxOutput(outputId = "ValueBox_TotalTriviaOutings"),
                       valueBoxOutput(outputId = "ValueBox_TotalTop5Finishes"),
                       valueBoxOutput(outputId = "ValueBox_AveragePointsScored_Original")
              ),
              
              fluidRow(width = 12,
                       column(width = 5, height = 5, plotOutput(outputId = "BarGraph_TotalQuestions")),
                       column(width = 7, height = 5, plotOutput(outputId = "BarGraph_Date"))
              ),
              
              br(),
              
              fluidRow(width = 12,
                       column(width = 6, tableOutput(outputId = "Table_MostCommonlyAsked")),
                       column(width = 6, tableOutput(outputId = "Table_Accuracy_Category"))
              ),
              
              br(),
              
              fluidRow(width = 12,
                       column(width = 6, tableOutput(outputId = "Table_TotalPoints_Wasted_Weighted"))
              ),
              
              br(),
              
              fluidRow(width = 12,
                       column(width = 6, tableOutput(outputId = "Table_AvgPoints_Scored_Weighted_Category_Best")),
                       column(width = 6, tableOutput(outputId = "Table_AvgPoints_Scored_Weighted_Category_Worst"))
              ),
              
              br(),
              
              fluidRow(width = 12,
                       column(width = 6, tableOutput(outputId = "Table_AvgPoints_Net_Weighted_Best")),
                       column(width = 6, tableOutput(outputId = "Table_AvgPoints_Net_Weighted_Worst"))
              ),
              
              
              br(),
              
              fluidRow(width = 12,
                       column(width = 6, tableOutput(outputId = "Table_AvgPoints_Net_Weighted_Best_10_Questions")),
                       column(width = 6, tableOutput(outputId = "Table_AvgPoints_Net_Weighted_Worst_10_Questions"))
              ),
              
              br(),
              
              fluidRow(width = 12,
                       column(width = 6, tableOutput(outputId = "Table_TotalPoints_Net_Weighted_Best")),
                       column(width = 6, tableOutput(outputId = "Table_TotalPoints_Net_Weighted_Worst"))
              ),
              
              br(),
              
              fluidRow(width = 12,
                       column(width = 6, tableOutput(outputId = "Table_Accuracy_Round")),
                       column(width = 6, tableOutput(outputId = "Table_Accuracy_Half"))
              ),
              
      ),
      
      
      tabItem(tabName = "Questions",
              
              # fluidRow(
              #   box(width = 6,
              #       radioButtons(
              #         inputId = "Include_Bunusx",
              #         label = "Include QUESTIONS of the Week:",
              #         choiceNames = c("Include Question of the Week", "Exclude Question of the Week"),
              #         choiceValues = c(1, 2),
              #         #selected = c(1, 2)
              #         selected = c(2)
              #       ),
              #       actionButton(inputId = "UpdateTTt", label = "Update", width = "100%")
              #   ),
              
              
              fluidRow(width = 12,
                       
                       box(width = 4,
                           actionButton(inputId = "NewQuestion", label = "New Question", width = "100%", style = "color: #000000; background-color: #8FC8DC; border-color: #000000; font-size: 120%"),
                           br(),
                           br(),
                           actionButton(inputId = "RevealAnswer", label = "Reveal Answer", width = "100%", style = "color: #000000; background-color: #8FC8DC; border-color: #000000; font-size: 120%"),
                           br(),
                           br(),
                           actionButton(inputId = "ResetTrivia", label = "Reset Trivia", width = "100%", style = "color: #FFFFFF; background-color: #990000; border-color: #000000; font-size: 120%")
                       ),
                       box(width = 4,
                           h4("Question Timer"),
                           textOutput(outputId = "Stopwatch")
                       )
              ),
              
              # actionButton(inputId = "NewQ", label = "NewQ", width = "100%"),actionButton(inputId = "Answer", label = "Answer", width = "100%"),actionButton(inputId = "Reset", label = "Reset", width = "100%")
              
              fluidRow(
                box(width = 12,
                    h4("Question List"),
                    tableOutput(outputId = "all_data_XX")
                )
              )
              
              
              
      ),
      
      
      
      tabItem(tabName = "RandomForest",
              
              
              
              fluidRow(width = 12,
                       
                       box(width = 12,
                           actionButton(inputId = "NewQuestion", label = "New Question", width = "100%", style = "color: #000000; background-color: #8FC8DC; border-color: #000000; font-size: 120%"),
                           br(),
                           br(),
                           actionButton(inputId = "RevealAnswer", label = "Reveal Answer", width = "100%", style = "color: #000000; background-color: #8FC8DC; border-color: #000000; font-size: 120%"),
                           br(),
                           br(),
                           actionButton(inputId = "ResetTrivia", label = "Reset Trivia", width = "100%", style = "color: #FFFFFF; background-color: #990000; border-color: #000000; font-size: 120%")
                       )
              ),
              
              
              
              
              
      )
      
      
      
      
      
      
    )
  )
)

serverx = function(input, output, session) {
  
  trivia_TT = eventReactive(input$UpdateTT, {
    trivia %>%
      filter(Freebie <= case_when(
        input$Include_Bonus == 2 ~ 0, #include = 0 or 1 for freebee. exclude = 0 only
        .default = 1)
      )
  }, ignoreNULL = FALSE)
  
  
  output$ArtistDiscopgraphy1 = renderText({
    spotify %>%
      select(Year, SongRanking, Song, ArtistName, Album) %>%
      filter(ArtistName == input$SelectedArtist) %>%
      ungroup() %>% #this line and the line afterwards are optional if I want to remove Aritst name as a row
      select(Year, SongRanking, Song, ArtistName, Album) %>%
      kable(format = "html", align = "llll", col.names = c("Year", "Song Ranking", "Song", "Artist Name", "Album")) %>%
      kable_styling(bootstrap_options = c("hover", "responsive", "striped")) %>%
      scroll_box(width = "100%", height = "100%") #%>%
    #row_spec(2, background = "#683659")
    
  })
  
  output$ValueBox_TotalTriviaOutings = renderValueBox({
    trivia_TT() %>%
      group_by(Date) %>%
      select(Date) %>%
      n_distinct() %>%
      format(nsmall = 0, big.mark = ",") %>%
      valueBox(subtitle = "Total Trivia Outings", color = "red", width = 4)
  })
  
  output$ValueBox_TotalTop5Finishes = renderValueBox({
    0 %>%
      format(nsmall = 0, big.mark = ",") %>%
      valueBox(subtitle = "Top 5 Finishes (Prize)", color = "green", width = 4)
  })
  
  output$ValueBox_AveragePointsScored_Original = renderValueBox({
    trivia_TT() %>%
      group_by(Date) %>%
      summarize(Average = sum(Points_Scored_Original)) %>%
      ungroup() %>%
      select(Average) %>%
      unlist() %>%
      mean() %>%
      format(nsmall = 0, big.mark = ",") %>%
      valueBox(subtitle = "Average Points Scored per Outing", color = "blue", width = 4)
  })
  
  output$Table_MostCommonlyAsked = renderText({
    trivia_TT() %>%
      filter(Round %in% c("Bonus", "Halftime", "Final", "Tiebreaker") == FALSE) %>%
      group_by(Category) %>%
      summarize(TotalQuestions = n(),
                Correct = sum(TeamAnswerCorrect),
                Accuracy = (Correct / TotalQuestions) %>% percent(),
                Points_Wagered_Original = sum(Points_Wagered_Original),
                Points_Scored_Original  = sum(Points_Scored_Original),
                Points_Wasted_Original  = sum(Points_Wasted_Original),
                Points_Net_Original     = sum(Points_Net_Original),
                Points_Wagered_Weighted = sum(Points_Wagered_Weighted),
                Points_Scored_Weighted  = sum(Points_Scored_Weighted),
                Points_Wasted_Weighted  = sum(Points_Wasted_Weighted),
                Points_Net_Weighted     = sum(Points_Net_Weighted),
                AvgPoints_Scored_Weighted = sum(Points_Scored_Weighted) / TotalQuestions,
                AvgPoints_Net_Weighted = sum(Points_Net_Weighted) / TotalQuestions
      ) %>%
      arrange(desc(TotalQuestions), desc(Accuracy %>% decimal()), desc(Points_Scored_Weighted), Category) %>%
      head(n = 10) %>%
      ungroup() %>%
      select(Category, TotalQuestions, Correct, Accuracy, Points_Scored_Weighted) %>%
      kable(format = "html", align = "lllll", caption = "Top 10 Most Common Categories", col.names = c("Category", "Total Questions", "Total Correct", "Accuracy", "Points Scored (Total, Weighted)")) %>%
      kable_styling(bootstrap_options = c("hover", "responsive", "striped")) %>%
      scroll_box(width = "100%", height = "100%")
  })
  
  output$Table_Accuracy_Category = renderText({
    trivia_TT() %>%
      filter(Round %in% c("Bonus", "Halftime", "Final", "Tiebreaker") == FALSE) %>%
      group_by(Category) %>%
      summarize(TotalQuestions = n(),
                Correct = sum(TeamAnswerCorrect),
                Accuracy = (Correct / TotalQuestions) %>% percent(),
                Points_Wagered_Original = sum(Points_Wagered_Original),
                Points_Scored_Original  = sum(Points_Scored_Original),
                Points_Wasted_Original  = sum(Points_Wasted_Original),
                Points_Net_Original     = sum(Points_Net_Original),
                Points_Wagered_Weighted = sum(Points_Wagered_Weighted),
                Points_Scored_Weighted  = sum(Points_Scored_Weighted),
                Points_Wasted_Weighted  = sum(Points_Wasted_Weighted),
                Points_Net_Weighted     = sum(Points_Net_Weighted),
                AvgPoints_Scored_Weighted = sum(Points_Scored_Weighted) / TotalQuestions,
                AvgPoints_Net_Weighted = sum(Points_Net_Weighted) / TotalQuestions
      ) %>%
      filter(TotalQuestions > 1) %>%
      arrange(desc(Accuracy %>% decimal()), desc(TotalQuestions), desc(Points_Scored_Weighted), Category) %>%
      head(n = 10) %>%
      ungroup() %>%
      select(Category, TotalQuestions, Correct, Accuracy, Points_Scored_Weighted) %>%
      kable(format = "html", align = "lllll", caption = "Top 10 Best Categories (Accuracy) (2+ Questions)", col.names = c("Category", "Total Questions", "Total Correct", "Accuracy", "Points Scored (Total, Weighted)")) %>%
      kable_styling(bootstrap_options = c("hover", "responsive", "striped")) %>%
      scroll_box(width = "100%", height = "100%")
  })
  
  output$Table_AvgPoints_Scored_Weighted_Category_Best = renderText({
    trivia_TT() %>%
      filter(Round %in% c("Bonus", "Halftime", "Final", "Tiebreaker") == FALSE) %>%
      group_by(Category) %>%
      summarize(TotalQuestions = n(),
                Correct = sum(TeamAnswerCorrect),
                Accuracy = (Correct / TotalQuestions) %>% percent(),
                Points_Wagered_Original = sum(Points_Wagered_Original),
                Points_Scored_Original  = sum(Points_Scored_Original),
                Points_Wasted_Original  = sum(Points_Wasted_Original),
                Points_Net_Original     = sum(Points_Net_Original),
                Points_Wagered_Weighted = sum(Points_Wagered_Weighted),
                Points_Scored_Weighted  = sum(Points_Scored_Weighted),
                Points_Wasted_Weighted  = sum(Points_Wasted_Weighted),
                Points_Net_Weighted     = sum(Points_Net_Weighted),
                AvgPoints_Scored_Weighted = sum(Points_Scored_Weighted) / TotalQuestions,
                AvgPoints_Net_Weighted = sum(Points_Net_Weighted) / TotalQuestions
      ) %>%
      filter(TotalQuestions > 1) %>%
      arrange(desc(AvgPoints_Scored_Weighted), desc(TotalQuestions), Category) %>%
      head(n = 10) %>%
      ungroup() %>%
      select(Category, Accuracy, TotalQuestions, AvgPoints_Scored_Weighted) %>%
      kable(format = "html", align = "llll", caption = "Top 10 Best Categories (Average Points, Weighted) (2+ Questions)", col.names = c("Category", "Accuracy", "Total Questions", "Points Scored (Avg, Weighted)")) %>%
      kable_styling(bootstrap_options = c("hover", "responsive", "striped")) %>%
      scroll_box(width = "100%", height = "100%")
  })
  
  output$Table_AvgPoints_Scored_Weighted_Category_Worst = renderText({
    trivia_TT() %>%
      filter(Round %in% c("Bonus", "Halftime", "Final", "Tiebreaker") == FALSE) %>%
      group_by(Category) %>%
      summarize(TotalQuestions = n(),
                Correct = sum(TeamAnswerCorrect),
                Accuracy = (Correct / TotalQuestions) %>% percent(),
                Points_Wagered_Original = sum(Points_Wagered_Original),
                Points_Scored_Original  = sum(Points_Scored_Original),
                Points_Wasted_Original  = sum(Points_Wasted_Original),
                Points_Net_Original     = sum(Points_Net_Original),
                Points_Wagered_Weighted = sum(Points_Wagered_Weighted),
                Points_Scored_Weighted  = sum(Points_Scored_Weighted),
                Points_Wasted_Weighted  = sum(Points_Wasted_Weighted),
                Points_Net_Weighted     = sum(Points_Net_Weighted),
                AvgPoints_Scored_Weighted = sum(Points_Scored_Weighted) / TotalQuestions,
                AvgPoints_Net_Weighted = sum(Points_Net_Weighted) / TotalQuestions
      ) %>%
      filter(TotalQuestions > 1) %>%
      arrange(AvgPoints_Scored_Weighted, desc(TotalQuestions), Category) %>%
      head(n = 10) %>%
      ungroup() %>%
      select(Category, Accuracy, TotalQuestions, AvgPoints_Scored_Weighted) %>%
      kable(format = "html", align = "llll", caption = "Top 10 Worst Categories (Average Points, Weighted) (2+ Questions)", col.names = c("Category", "Accuracy", "Total Questions", "Points Scored (Avg, Weighted)")) %>%
      kable_styling(bootstrap_options = c("hover", "responsive", "striped")) %>%
      scroll_box(width = "100%", height = "100%")
  })
  
  
  
  output$Table_AvgPoints_Net_Weighted_Best = renderText({
    trivia_TT() %>%
      filter(Round %in% c("Bonus", "Halftime", "Final", "Tiebreaker") == FALSE) %>%
      group_by(Category) %>%
      summarize(TotalQuestions = n(),
                Correct = sum(TeamAnswerCorrect),
                Accuracy = (Correct / TotalQuestions) %>% percent(),
                Points_Wagered_Original = sum(Points_Wagered_Original),
                Points_Scored_Original  = sum(Points_Scored_Original),
                Points_Wasted_Original  = sum(Points_Wasted_Original),
                Points_Net_Original     = sum(Points_Net_Original),
                Points_Wagered_Weighted = sum(Points_Wagered_Weighted),
                Points_Scored_Weighted  = sum(Points_Scored_Weighted),
                Points_Wasted_Weighted  = sum(Points_Wasted_Weighted),
                Points_Net_Weighted     = sum(Points_Net_Weighted),
                AvgPoints_Scored_Weighted = sum(Points_Scored_Weighted) / TotalQuestions,
                AvgPoints_Net_Weighted = sum(Points_Net_Weighted) / TotalQuestions
      ) %>%
      filter(TotalQuestions > 1) %>%
      arrange(desc(AvgPoints_Net_Weighted), desc(TotalQuestions), Category) %>%
      head(n = 10) %>%
      ungroup() %>%
      select(Category, TotalQuestions, Correct, Accuracy, AvgPoints_Net_Weighted) %>%
      kable(format = "html", align = "lllll", caption = "Top 10 Categories (Avg Net Points, Weighted) (2+ Questions)", col.names = c("Category", "Total Questions", "Total Correct", "Accuracy", "Net Points (Avg, Weighted)")) %>%
      kable_styling(bootstrap_options = c("hover", "responsive", "striped")) %>%
      scroll_box(width = "100%", height = "100%")
  })
  
  output$Table_AvgPoints_Net_Weighted_Worst = renderText({
    trivia_TT() %>%
      filter(Round %in% c("Bonus", "Halftime", "Final", "Tiebreaker") == FALSE) %>%
      group_by(Category) %>%
      summarize(TotalQuestions = n(),
                Correct = sum(TeamAnswerCorrect),
                Accuracy = (Correct / TotalQuestions) %>% percent(),
                Points_Wagered_Original = sum(Points_Wagered_Original),
                Points_Scored_Original  = sum(Points_Scored_Original),
                Points_Wasted_Original  = sum(Points_Wasted_Original),
                Points_Net_Original     = sum(Points_Net_Original),
                Points_Wagered_Weighted = sum(Points_Wagered_Weighted),
                Points_Scored_Weighted  = sum(Points_Scored_Weighted),
                Points_Wasted_Weighted  = sum(Points_Wasted_Weighted),
                Points_Net_Weighted     = sum(Points_Net_Weighted),
                AvgPoints_Scored_Weighted = sum(Points_Scored_Weighted) / TotalQuestions,
                AvgPoints_Net_Weighted = sum(Points_Net_Weighted) / TotalQuestions
      ) %>%
      filter(TotalQuestions > 1) %>%
      arrange(AvgPoints_Net_Weighted, desc(TotalQuestions), Category) %>%
      head(n = 10) %>%
      ungroup() %>%
      select(Category, TotalQuestions, Correct, Accuracy, AvgPoints_Net_Weighted) %>%
      kable(format = "html", align = "lllll", caption = "Bottom 10 Categories (Avg Net Points, Weighted) (2+ Questions)", col.names = c("Category", "Total Questions", "Total Correct", "Accuracy", "Net Points (Avg, Weighted)")) %>%
      kable_styling(bootstrap_options = c("hover", "responsive", "striped")) %>%
      scroll_box(width = "100%", height = "100%")
  })
  
  
  
  output$Table_AvgPoints_Net_Weighted_Best_10_Questions = renderText({
    trivia_TT() %>%
      filter(Round %in% c("Bonus", "Halftime", "Final", "Tiebreaker") == FALSE) %>%
      group_by(Category) %>%
      summarize(TotalQuestions = n(),
                Correct = sum(TeamAnswerCorrect),
                Accuracy = (Correct / TotalQuestions) %>% percent(),
                Points_Wagered_Original = sum(Points_Wagered_Original),
                Points_Scored_Original  = sum(Points_Scored_Original),
                Points_Wasted_Original  = sum(Points_Wasted_Original),
                Points_Net_Original     = sum(Points_Net_Original),
                Points_Wagered_Weighted = sum(Points_Wagered_Weighted),
                Points_Scored_Weighted  = sum(Points_Scored_Weighted),
                Points_Wasted_Weighted  = sum(Points_Wasted_Weighted),
                Points_Net_Weighted     = sum(Points_Net_Weighted),
                AvgPoints_Scored_Weighted = sum(Points_Scored_Weighted) / TotalQuestions,
                AvgPoints_Net_Weighted = sum(Points_Net_Weighted) / TotalQuestions
      ) %>%
      filter(TotalQuestions > 7) %>%
      arrange(desc(AvgPoints_Net_Weighted), desc(TotalQuestions), Category) %>%
      head(n = 10) %>%
      ungroup() %>%
      select(Category, TotalQuestions, Correct, Accuracy, AvgPoints_Net_Weighted) %>%
      kable(format = "html", align = "lllll", caption = "Top 10 Categories (Avg Net Points, Weighted) (10+ Questions)", col.names = c("Category", "Total Questions", "Total Correct", "Accuracy", "Net Points (Avg, Weighted)")) %>%
      kable_styling(bootstrap_options = c("hover", "responsive", "striped")) %>%
      scroll_box(width = "100%", height = "100%")
  })
  
  output$Table_AvgPoints_Net_Weighted_Worst_10_Questions = renderText({
    trivia_TT() %>%
      filter(Round %in% c("Bonus", "Halftime", "Final", "Tiebreaker") == FALSE) %>%
      group_by(Category) %>%
      summarize(TotalQuestions = n(),
                Correct = sum(TeamAnswerCorrect),
                Accuracy = (Correct / TotalQuestions) %>% percent(),
                Points_Wagered_Original = sum(Points_Wagered_Original),
                Points_Scored_Original  = sum(Points_Scored_Original),
                Points_Wasted_Original  = sum(Points_Wasted_Original),
                Points_Net_Original     = sum(Points_Net_Original),
                Points_Wagered_Weighted = sum(Points_Wagered_Weighted),
                Points_Scored_Weighted  = sum(Points_Scored_Weighted),
                Points_Wasted_Weighted  = sum(Points_Wasted_Weighted),
                Points_Net_Weighted     = sum(Points_Net_Weighted),
                AvgPoints_Scored_Weighted = sum(Points_Scored_Weighted) / TotalQuestions,
                AvgPoints_Net_Weighted = sum(Points_Net_Weighted) / TotalQuestions
      ) %>%
      filter(TotalQuestions > 7) %>%
      arrange(AvgPoints_Net_Weighted, desc(TotalQuestions), Category) %>%
      head(n = 10) %>%
      ungroup() %>%
      select(Category, TotalQuestions, Correct, Accuracy, AvgPoints_Net_Weighted) %>%
      kable(format = "html", align = "lllll", caption = "Bottom 10 Categories (Avg Net Points, Weighted) (10+ Questions)", col.names = c("Category", "Total Questions", "Total Correct", "Accuracy", "Net Points (Avg, Weighted)")) %>%
      kable_styling(bootstrap_options = c("hover", "responsive", "striped")) %>%
      scroll_box(width = "100%", height = "100%")
  })
  
  
  
  output$Table_TotalPoints_Net_Weighted_Best = renderText({
    trivia_TT() %>%
      filter(Round %in% c("Bonus", "Halftime", "Final", "Tiebreaker") == FALSE) %>%
      group_by(Category) %>%
      summarize(TotalQuestions = n(),
                Correct = sum(TeamAnswerCorrect),
                Accuracy = (Correct / TotalQuestions) %>% percent(),
                Points_Wagered_Original = sum(Points_Wagered_Original),
                Points_Scored_Original  = sum(Points_Scored_Original),
                Points_Wasted_Original  = sum(Points_Wasted_Original),
                Points_Net_Original     = sum(Points_Net_Original),
                Points_Wagered_Weighted = sum(Points_Wagered_Weighted),
                Points_Scored_Weighted  = sum(Points_Scored_Weighted),
                Points_Wasted_Weighted  = sum(Points_Wasted_Weighted),
                Points_Net_Weighted     = sum(Points_Net_Weighted),
                AvgPoints_Scored_Weighted = sum(Points_Scored_Weighted) / TotalQuestions,
                AvgPoints_Net_Weighted = sum(Points_Net_Weighted) / TotalQuestions
      ) %>%
      filter(TotalQuestions > 1) %>%
      arrange(desc(Points_Net_Weighted), desc(TotalQuestions), Category) %>%
      head(n = 10) %>%
      ungroup() %>%
      select(Category, TotalQuestions, Correct, Accuracy, Points_Net_Weighted) %>%
      kable(format = "html", align = "lllll", caption = "Top 10 Categories (Total Net Points, Weighted) (2+ Questions)", col.names = c("Category", "Total Questions", "Total Correct", "Accuracy", "Net Points (Total, Weighted)")) %>%
      kable_styling(bootstrap_options = c("hover", "responsive", "striped")) %>%
      scroll_box(width = "100%", height = "100%")
  })
  
  output$Table_TotalPoints_Net_Weighted_Worst = renderText({
    trivia_TT() %>%
      filter(Round %in% c("Bonus", "Halftime", "Final", "Tiebreaker") == FALSE) %>%
      group_by(Category) %>%
      summarize(TotalQuestions = n(),
                Correct = sum(TeamAnswerCorrect),
                Accuracy = (Correct / TotalQuestions) %>% percent(),
                Points_Wagered_Original = sum(Points_Wagered_Original),
                Points_Scored_Original  = sum(Points_Scored_Original),
                Points_Wasted_Original  = sum(Points_Wasted_Original),
                Points_Net_Original     = sum(Points_Net_Original),
                Points_Wagered_Weighted = sum(Points_Wagered_Weighted),
                Points_Scored_Weighted  = sum(Points_Scored_Weighted),
                Points_Wasted_Weighted  = sum(Points_Wasted_Weighted),
                Points_Net_Weighted     = sum(Points_Net_Weighted),
                AvgPoints_Scored_Weighted = sum(Points_Scored_Weighted) / TotalQuestions,
                AvgPoints_Net_Weighted = sum(Points_Net_Weighted) / TotalQuestions
      ) %>%
      filter(TotalQuestions > 1) %>%
      arrange(Points_Net_Weighted, desc(TotalQuestions), Category) %>%
      head(n = 10) %>%
      ungroup() %>%
      select(Category, TotalQuestions, Correct, Accuracy, Points_Net_Weighted) %>%
      kable(format = "html", align = "lllll", caption = "Bottom 10 Categories (Total Net Points, Weighted) (2+ Questions)", col.names = c("Category", "Total Questions", "Total Correct", "Accuracy", "Net Points (Total, Weighted)")) %>%
      kable_styling(bootstrap_options = c("hover", "responsive", "striped")) %>%
      scroll_box(width = "100%", height = "100%")
  })
  
  
  
  
  
  output$Table_Accuracy_Round = renderText({
    trivia_TT() %>%
      #filter(Round %in% c("Bonus", "Halftime", "Final", "Tiebreaker") == FALSE) %>%
      group_by(Round) %>%
      summarize(TotalQuestions = n(),
                Correct = sum(TeamAnswerCorrect),
                Accuracy = (Correct / TotalQuestions) %>% percent(),
                Points_Wagered_Original = sum(Points_Wagered_Original),
                Points_Scored_Original  = sum(Points_Scored_Original),
                Points_Wasted_Original  = sum(Points_Wasted_Original),
                Points_Net_Original     = sum(Points_Net_Original),
                Points_Wagered_Weighted = sum(Points_Wagered_Weighted),
                Points_Scored_Weighted  = sum(Points_Scored_Weighted),
                Points_Wasted_Weighted  = sum(Points_Wasted_Weighted),
                Points_Net_Weighted     = sum(Points_Net_Weighted),
                AvgPoints_Scored_Weighted = sum(Points_Scored_Weighted) / TotalQuestions,
                AvgPoints_Net_Weighted = sum(Points_Net_Weighted) / TotalQuestions
      ) %>%
      #arrange(desc(Accuracy %>% decimal()), desc(TotalQuestions), desc(PointsScored), Round) %>%
      arrange(Round %>% factor(levels = c("1", "2", "3", "Bonus", "Halftime", "4", "5", "6", "Final", "Tiebreaker"))) %>%
      head(n = 10) %>%
      ungroup() %>%
      select(Round, TotalQuestions, Correct, Accuracy, Points_Scored_Weighted) %>%
      kable(format = "html", align = "lllll", caption = "Round Performance (Accuracy)", col.names = c("Round", "Total Questions", "Total Correct", "Accuracy", "Points Scored (Total, Weighted)")) %>%
      kable_styling(bootstrap_options = c("hover", "responsive", "striped")) %>%
      scroll_box(width = "100%", height = "100%")
  })
  
  output$Table_Accuracy_Half = renderText({
    trivia_TT() %>%
      #filter(Round %in% c("Bonus", "Halftime", "Final", "Tiebreaker") == FALSE) %>%
      group_by(`Trivia Half`) %>%
      summarize(TotalQuestions = n(),
                Correct = sum(TeamAnswerCorrect),
                Accuracy = (Correct / TotalQuestions) %>% percent(),
                Points_Wagered_Original = sum(Points_Wagered_Original),
                Points_Scored_Original  = sum(Points_Scored_Original),
                Points_Wasted_Original  = sum(Points_Wasted_Original),
                Points_Net_Original     = sum(Points_Net_Original),
                Points_Wagered_Weighted = sum(Points_Wagered_Weighted),
                Points_Scored_Weighted  = sum(Points_Scored_Weighted),
                Points_Wasted_Weighted  = sum(Points_Wasted_Weighted),
                Points_Net_Weighted     = sum(Points_Net_Weighted),
                AvgPoints_Scored_Weighted = sum(Points_Scored_Weighted) / TotalQuestions,
                AvgPoints_Net_Weighted = sum(Points_Net_Weighted) / TotalQuestions
      ) %>%
      #arrange(desc(Accuracy %>% decimal()), desc(TotalQuestions), desc(PointsScored), `Trivia Half`) %>%
      arrange(`Trivia Half` %>% factor(levels = c("First Half", "Bonus", "Halftime", "Second Half", "Final", "Tiebreaker"))) %>%
      head(n = 10) %>%
      ungroup() %>%
      select(`Trivia Half`, TotalQuestions, Correct, Accuracy, Points_Scored_Weighted) %>%
      kable(format = "html", align = "lllll", caption = "Half Performance (Accuracy)", col.names = c("Trivia Half", "Total Questions", "Total Correct", "Accuracy", "Points Scored (Total, Weighted)")) %>%
      kable_styling(bootstrap_options = c("hover", "responsive", "striped")) %>%
      scroll_box(width = "100%", height = "100%")
  })
  
  
  
  
  
  
  output$Keeps = renderUI({
    
    t1 = TopArtists_AllTime[which(TopArtists_AllTime$ArtistName == input$SelectedArtist), 5] #try to make the 5 dynamic
    xxx1 = paste("According to Spotify Wrapped, ", input$SelectedArtist, " is my ", t1, " favorite artist overall. Here are my favorite songs from ", input$SelectedArtist, ", according to the data.", sep = "")
    
    t11 = TopArtists_AllTime %>% filter(TopArtists_AllTime$ArtistName == input$SelectedArtist) %>% select(Song_Count)
    xxx2 = paste(t11, " songs from ", input$SelectedArtist, " have appeared in my Spotify Wrapped all time.", sep = "")
    
    HTML(paste(xxx1, "</br>", "</br>", xxx2, "</br>", "</br>", sep = ""))
  })
  
  
  
  output$sayyouloveme = renderPlot({
    spotify %>%
      filter(Year == input$SelectedYear) %>%
      group_by(Year, AlbumYear) %>%
      mutate(TT = n()) %>%
      ggplot(aes(x = AlbumYear, y = TT)) + 
      geom_bar(stat = "identity", fill = "darkblue", width = 1) +
      #geom_text(aes(label = Year_Rank_Label %>% format(nsmall = 2)), vjust = 1.6, color = "white", size = 1) +
      xlab(label = "Album Year") +
      ylab(label = "Song Count") +
      ggtitle(label = "Song Count by Album Year") +
      theme(plot.title = element_text(hjust = 0.5),
            #axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()
      )
  })
  
  
  
  output$plot2 = renderPlot({
    
    ggplot(data = TopArtists_ByYear %>% filter(ArtistName == input$SelectedArtist), aes(x = Year, y = NewRank)) +
      geom_bar(stat = "identity", fill = "darkblue") +
      geom_text(aes(label = Year_Rank_Label %>% format(nsmall = 2)), vjust = 1.6, color = "white", size = 5) +
      xlab(label = "Year") +
      ylab(label = "Rank") +
      ggtitle(label = "Artist Rank by Year") +
      theme(plot.title = element_text(hjust = 0.5),
            #axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()
      ) +
      #scale_x_discrete(limits = c(2020, 2021, 2022, 2023, 2024))
      scale_x_continuous(breaks = min(TopArtists_ByYear$Year):max(TopArtists_ByYear$Year))
  })
  
  output$X123 = renderText({
    TopArtists_ByYear %>%
      filter(Year == input$SelectedYear, Rank <= 10, Rank > 0) %>%
      ungroup() %>%
      select(ArtistName, Score, Song_Count) %>%
      arrange(desc(Score), desc(Song_Count)) %>%
      kable(format = "html", align = "llrr", col.names = c("Artist Name", "Score", "Song Count")) %>%
      kable_styling(bootstrap_options = c("hover", "responsive", "striped")) %>%
      scroll_box(width = "100%", height = "100%")
  })
  
  
  output$BarGraph_TotalQuestions = renderPlot({
    
    KEP1 =
      trivia_TT() %>% filter(Category %in% (trivia_TT() %>%
                                              filter(Round %in% c("Bonus", "Halftime", "Final", "Tiebreaker") == FALSE) %>%
                                              group_by(Category) %>%
                                              summarize(TotalQuestions = n(),
                                                        Correct = sum(TeamAnswerCorrect),
                                                        Accuracy = (Correct / TotalQuestions) %>% percent()) %>%
                                              #AvgPoints = sum(PointsScored) / TotalQuestions) %>%
                                              arrange(desc(TotalQuestions), desc(Correct)) %>%
                                              head(n = 10) %>%
                                              ungroup() %>% select(Category) %>% unlist() %>% unname())) %>%
      #end of filter
      
      mutate(
        Incorrect_x = case_when(
          TeamAnswerCorrect == 1 ~ "Correct",
          .default = "Incorrect"
        )) %>%
      filter(Round %in% c("Bonus", "Halftime", "Final", "Tiebreaker") == FALSE) %>%
      group_by(Category, Incorrect_x) %>%
      summarize(TotalQuestions = n()) %>%
      pivot_wider(names_from = Incorrect_x, values_from = TotalQuestions) %>%
      mutate(
        TotalQuestions =
          case_when(
            is.na(Correct) == TRUE ~ 0,
            .default = Correct
          ) + 
          case_when(
            is.na(Incorrect) == TRUE ~ 0,
            .default = Incorrect
          )
      ) %>%
      arrange(desc(TotalQuestions), desc(Correct), Category)
    
    
    
    kep1_long = KEP1 %>%
      pivot_longer(cols = c(Correct, Incorrect), 
                   names_to = "Result",
                   values_to = "Games")
    
    sortdream = factor(x = kep1_long$Category, levels = rev(x = unique(x = KEP1$Category)))
    
    
    
    ggplot(data = kep1_long, aes(x = Games, y = sortdream, fill = Result)) +
      geom_bar(stat = "identity", position = position_stack(reverse = TRUE), width = 0.75) +
      #position = position_stack is to get it so that correct appears first. otherwise Incorrect appears first
      scale_fill_manual("Legend", values = c("Correct" = "#08A45C", "Incorrect" = "#E03A3E", "C" = "blue")) +
      geom_text(aes(label = Games %>% format(nsmall = 0)), position = position_stack(reverse = TRUE, vjust = .5), color = "white", size = 4) +
      xlab(label = "Number of Total Questions") +
      #ylab(label = "Genre") +
      ggtitle(label = "Top 10 Trivia Categories (Number of Total Questions)") +
      theme(plot.title = element_text(hjust = 0.5),
            axis.title.y = element_blank(),
            #axis.title.x = element_blank(),
            #axis.text.y = element_blank(),
            #axis.text.y = Genre,
            axis.ticks.y = element_blank()
            #axis.text.x = 
      )
  })
  
  
  output$BarGraph_Date = renderPlot({
    
    ggplot(data = trivia %>%
             filter(Round %in% c("Bonus", "Halftime", "Final", "Tiebreaker") == FALSE) %>%
             group_by(Date) %>%
             summarize(TotalQuestions = n(),
                       Correct = sum(TeamAnswerCorrect),
                       Points_Wagered_Original = sum(Points_Wagered_Original),
                       Points_Scored_Original  = sum(Points_Scored_Original),
                       Points_Wasted_Original  = sum(Points_Wasted_Original),
                       Points_Net_Original     = sum(Points_Net_Original),
                       Points_Wagered_Weighted = sum(Points_Wagered_Weighted),
                       Points_Scored_Weighted  = sum(Points_Scored_Weighted),
                       Points_Wasted_Weighted  = sum(Points_Wasted_Weighted),
                       Points_Net_Weighted     = sum(Points_Net_Weighted)
             ) %>%
             arrange(Date) %>%
             head(n = 10) %>%
             ungroup() %>%
             select(Date, Points_Scored_Original)
           , aes(x = Date, y = Points_Scored_Original)) +
      geom_line(color = "#08A45C", alpha = 0.75) +
      geom_point() +
      geom_text(aes(label = Points_Scored_Original %>% format(nsmall = 0)), hjust = 2, vjust = 0.5, color = "black", size = 4) +
      xlab(label = "Date") +
      #ylab(label = "Genre") +
      ggtitle(label = "Points Score (Original) by Date") +
      theme(plot.title = element_text(hjust = 0.5),
            axis.title.y = element_blank(),
            #axis.title.x = element_blank(),
            #axis.text.y = element_blank(),
            #axis.text.y = Genre,
            axis.ticks.y = element_blank()
            #axis.text.x = 
      )
  })
  
  
  output$Count_NewArtist = renderValueBox({
    Data_NewArtist = Year_Artist %>%
      filter(Year.y == input$SelectedYear) %>%
      ungroup() %>%
      select("NewArtist2") %>%
      sum() %>%
      format(nsmall = 0, big.mark = ",")
    Data_NewArtist %>%
      valueBox(subtitle = "New Artists", color = "green", icon = icon(name = "fab fa-spotify"), width = 4)
  })
  
  output$Count_RepeatArtist = renderValueBox({
    Data_RepeatArtist = Year_Artist %>%
      filter(Year.y == input$SelectedYear) %>%
      ungroup() %>%
      select("RepeatArtist2") %>%
      sum() %>%
      format(nsmall = 0, big.mark = ",")
    Data_RepeatArtist %>%
      valueBox(subtitle = "Repeat Artists", color = "darkgreen", icon = icon(name = "fab fa-spotify"), width = 4)
  })
  
  output$UUU3 = renderValueBox({
    spotify %>%
      filter(Year == input$SelectedYear) %>%
      select(Year) %>%
      sum() %>%
      format(nsmall = 0, big.mark = ",") %>%
      valueBox(subtitle = "Minutes Streamed", color = "darkgreen", icon = icon(name = "fab fa-spotify"), width = 4)
  })
  
  
  output$pie_Chart22 = renderPlot({
    Data_PieChart =
      TopArtists_ByYear %>%
      ungroup() %>%
      group_by(NewArtistFlag) %>%
      #arrange(Year, Song_Count) %>%
      filter(Year == input$SelectedYear, is.na(NewArtistFlag) == FALSE) %>%
      
      summarize(`Song Count` = sum(Song_Count)) %>%
      arrange(NewArtistFlag)
    #arrange(desc(`Song Count`)) %>%
    #ungroup()
    
    
    ggplot(data = Data_PieChart, aes(x = "", y = `Song Count`, group = NewArtistFlag, fill = NewArtistFlag)) +
      #geom_bar(stat = "identity", width = 1, fill = c("#08A45C", "#D9D9D9")) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y", start = 0) +
      #geom_col(color = "black", position = "stack", orientation = "x") +
      scale_fill_manual(values = c("Repeat Artist" = c("#333333"), "New Artist" = c("#08A45C"))) +
      theme_void() +
      #theme_minimal() +
      #labs(fill = "TTX") + 
      geom_text(aes(label = `Song Count`), color = "white", size = 5, position = position_stack(vjust = 0.5)) +
      #xlab(label = "Song Count") +
      #ylab(label = "Genre") +
      ggtitle(label = "Songs by Artist Breakdown") +
      theme(plot.title = element_text(hjust = 0.5),
            axis.title.y = element_blank(),
            legend.position = "right",
            panel.grid = element_blank(),
            axis.ticks = element_blank(),
            #axis.title.x = element_blank(),
            #axis.text.y = element_blank(),
            #axis.text.y = Genre,
            axis.ticks.y = element_blank(),
            axis.text.x = element_blank(),
            legend.key.size = unit(x = 1, units = "cm"), #change legend key size
            legend.key.height = unit(x = 1, units = "cm"), #change legend key height
            legend.key.width = unit(x = 1, units = "cm"), #change legend key width
            legend.title = element_text(size = 14), #change legend title font size
            legend.text = element_text(size = 10) #change legend text font size
      ) +
      guides(fill = guide_legend(title = "Artist Type"))
  })
  
  
  output$plot_genreByear = renderPlot({
    
    ggplot(data = TopGenres_ByYear, aes(x = Year, y = Song_Count, fill = Genre)) +
      geom_bar(position = "fill", stat = "identity", width = 0.75, aes(alpha = Year == input$SelectedYear)) +
      #geom_text(aes(label = Song_Count %>% format(nsmall = 0)), hjust = 2, vjust = 0.5, color = "white", size = 4) +
      #xlab(label = "Year") +
      ylab(label = "Song Percentage") +
      #ggtitle(label = "Genre Count by Year") +
      theme(plot.title = element_text(hjust = 0.5),
            #axis.title.y = element_blank(),
            #axis.title.x = element_blank(),
            #axis.text.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()
            #axis.text.x = 
      ) +
      scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.5), guide = F)
  })
  
  
  
  R_submission_written_start = reactiveValues(R_NEW_INTMORE = 1)
  CCC = reactiveValues(CC = "Korean: 시작")
  R_submission_written_testnumber = reactiveValues(start = 0)
  
  R_submission_written_correctanswer = reactiveValues(start = "sijag")
  
  
  
  
  
  Trivia_Question_Counter = reactiveValues(start = 0, NewQ_LastClicked = 0, Reveal_LastClicked = 0)
  
  
  
  
  rv = reactiveValues(
    df = data.frame(
      QuestionNumber  = as.integer(character()),
      Category        = as.character(character()),
      Question        = as.character(character()),
      Answer          = as.character(character())
    )
  )
  
  
  observeEvent(input$NewQuestion, {
    Trivia_Question_Counter$NewQ_LastClicked = 1
  })
  
  observeEvent(input$NewQuestion, {
    Trivia_Question_Counter$Reveal_LastClicked = 0
  })
  
  observeEvent(input$RevealAnswer, {
    Trivia_Question_Counter$NewQ_LastClicked = 0
  })
  
  observeEvent(input$RevealAnswer, {
    Trivia_Question_Counter$Reveal_LastClicked = 1
  })
  
  observeEvent(input$NewQuestion, {
    Trivia_Question_Counter$start = case_when(
      Trivia_Question_Counter$NewQ_LastClicked == 0 ~ Trivia_Question_Counter$start + 1,
      .default = Trivia_Question_Counter$start
    )
  })
  
  
  observeEvent(input$NewQuestion, {
    rv$df = rbind(rv$df,
                  data.frame(
                    QuestionNumber = Trivia_Question_Counter$start,
                    Category       = "different", #korean_duo[R_submission_written_start$R_NEW_INTMORE, ]$Korean,
                    Question       = "TT",#R_submission_written_correctanswer$start,
                    Answer         = "",
                    TT = Trivia_Question_Counter$NewQ_LastClicked
                  )
    )
  })
  
  output$all_data_XX = renderText(
    rv$df %>%
      arrange(QuestionNumber) %>%
      kable(format = "html", align = "llll", col.names = c("QuestionNumber", "Category", "Question", "Answer", "TT")) %>%
      kable_styling(bootstrap_options = c("hover", "responsive", "striped")) %>%
      scroll_box(width = "100%", height = "100%")
  )
  
  
  
  
  
  
  observeEvent(input$ResetTrivia, {
    rv$df = data.frame(
      QuestionNumber  = as.integer(character()),
      Category        = as.character(character()),
      Question        = as.character(character()),
      Answer          = as.character(character()),
      TT = as.integer(character())
    )
  })
  
  observeEvent(input$ResetTrivia, {
    Trivia_Question_Counter$start = 0
  })
  
  
  
  
  
  
  
  
  
  # Initialize the timer, not active.
  timer = reactiveVal(0)
  active = reactiveVal(FALSE)
  update_interval = 0.1 # How many seconds between timer updates?
  
  # Output the time left.
  output$Stopwatch = renderText({
    paste("Time passed: ", seconds_to_period(timer()), sep = "")
  })
  
  # observer that invalidates every second. If timer is active, decrease by one.
  observe({
    invalidateLater(100, session)
    isolate({
      if(active())
      {
        timer(round(timer() + update_interval, 1))
      }
    })
  })
  
  # observers for actionbuttons
  observeEvent(input$NewQuestion, {active(TRUE)}) #Starts/continues the timer
  observeEvent(input$NewQuestion, {timer(0)}) #Stops the timer
  
  observeEvent(input$RevealAnswer, {active(FALSE)}) #Stops the timer
  
  observeEvent(input$ResetTrivia, {active(FALSE)}) #Stops the timer
  observeEvent(input$ResetTrivia, {timer(0)}) #Resets the timer
  
  
  
  
  output$UUU3 = renderValueBox({
    spotify %>%
      filter(Year == input$SelectedYear) %>%
      select(Year) %>%
      sum() %>%
      format(nsmall = 0, big.mark = ",") %>%
      valueBox(subtitle = "Minutes Streamed", color = "darkgreen", icon = icon(name = "fab fa-spotify"), width = 4)
  })
  
  output$Top5Latest = renderText({
    spotify %>%
      filter(Year == input$SelectedYear) %>%
      arrange(AlbumYear) %>%
      head(n = 5) %>%
      ungroup() %>%
      select(SongRanking, Song, ArtistName, Album, AlbumYear) %>%
      kable(format = "html", align = "lllr", caption = "Top 5 Latest Songs", col.names = c("Song Ranking", "Song", "Artist Name", "Album", "Album Year")) %>%
      kable_styling(bootstrap_options = c("hover", "responsive", "striped")) %>%
      scroll_box(width = "100%", height = "100%")
  })
  
}







shinyApp(
  ui = uix,
  server = serverx
)