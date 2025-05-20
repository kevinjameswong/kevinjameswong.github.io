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

gs4_deauth()
trivia = read_sheet(ss = "https://docs.google.com/spreadsheets/d/1k4vAQll8rTX4jHBNEOxlEYl1MEra502bfDBahPyEYQQ", sheet = "Trivia Questions with Answers") %>%
  filter(is.na(Date) == FALSE) %>%
  mutate(
    Date = Date %>% as.Date(),
    `Question Number` = `Question Number` %>% as.character(),
    Round = Round %>% as.character(),
    `Question within Round` = `Question within Round` %>% as.character(),
    `Correct Answer` = `Correct Answer` %>% as.character(),
    `Submitted Answer` = `Submitted Answer` %>% as.character()
  ) %>%
  arrange(Index)

trivia$PointsScored = case_when(
  trivia$`Correct?` == 1 ~ trivia$Confidence,
  .default = 0
)

trivia$PointsWasted = case_when(
  trivia$`Correct?` == 0 ~ trivia$Confidence,
  .default = 0
)

trivia$NetPoints = case_when(
  trivia$`Correct?` == 1 ~ trivia$Confidence,
  .default = - trivia$Confidence
)


percent = function(x, digits = 2, format = "f", ...) {
  t = formatC(x * 100, format = format, digits = digits, ...) %>% paste("%", sep = "")
  return(t)
}

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
        #badgeLabel = "NEW",
        selected = TRUE)#,
      #menuItem(
        #text = "Artists",
        #icon = icon(name = "music", lib = "font-awesome"),
        #tabName = "Artists",
        #badgeLabel = "NEW")#,
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
                      inputId = "Include_Bunus",
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
                       valueBoxOutput(outputId = "TotalTriviaOutings"),
                       valueBoxOutput(outputId = "TotalTop5Finishes"),
                       valueBoxOutput(outputId = "AveragePointsScored")
              ),
              
              fluidRow(width = 12,
                       column(width = 5, height = 5, plotOutput(outputId = "BarGraph_TotalQuestions")),
                       column(width = 7, height = 5, plotOutput(outputId = "BarGraph_Date"))
              ),
              
              br(),

              fluidRow(width = 12,
                       column(width = 6, tableOutput(outputId = "MostAsked")),
                       column(width = 6, tableOutput(outputId = "TotalPoints_Correct"))
              ),
              
              br(),
              
              fluidRow(width = 12,
                       column(width = 6, tableOutput(outputId = "TotalPoints_Wasted")),
                       column(width = 6, tableOutput(outputId = "AvgPoints_Correct"))
              ),
              )
      )
    )
)

serverx = function(input, output, session) {
  
  
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
  
  output$TotalTriviaOutings = renderValueBox({
    trivia %>%
      group_by(Date) %>%
      select(Date) %>%
      n_distinct() %>%
      format(nsmall = 0, big.mark = ",") %>%
      valueBox(subtitle = "Total Trivia Outings", color = "red", width = 4)
  })
  
  output$TotalTop5Finishes = renderValueBox({
    0 %>%
      format(nsmall = 0, big.mark = ",") %>%
      valueBox(subtitle = "Top 5 Finishes (Prize)", color = "green", width = 4)
  })
  
  output$AveragePointsScored = renderValueBox({
    trivia %>%
      group_by(Date) %>%
      summarize(Average = sum(PointsScored)) %>%
      ungroup() %>%
      select(Average) %>%
      unlist() %>%
      mean() %>%
      format(nsmall = 0, big.mark = ",") %>%
      valueBox(subtitle = "Average Points Scored per Outing", color = "blue", width = 4)
  })
  
  output$MostAsked = renderText({
    trivia %>%
      filter(Round %in% c("Bonus", "Halftime", "Final") == FALSE) %>%
      filter(is.na(Freebie)) %>%
      group_by(Category) %>%
      summarize(TotalQuestions = n(),
                Correct = sum(`Correct?`),
                Accuracy = (Correct / TotalQuestions) %>% percent(),
                PointsScored = sum(PointsScored),
                PointsWasted = sum(PointsWasted)) %>%
      arrange(desc(TotalQuestions), desc(Accuracy), Category) %>%
      head(n = 10) %>%
      ungroup() %>%
      select(Category, TotalQuestions, Correct, Accuracy, PointsScored) %>%
      kable(format = "html", align = "lllll", caption = "Top 10 Most Common Categories", col.names = c("Category", "Total Questions", "Total Correct", "Accuracy", "Points Scored (Total)")) %>%
      kable_styling(bootstrap_options = c("hover", "responsive", "striped")) %>%
      scroll_box(width = "100%", height = "100%")
  })
  
  output$TotalPoints_Correct = renderText({
    trivia %>%
      filter(Round %in% c("Bonus", "Halftime", "Final") == FALSE) %>%
      filter(is.na(Freebie)) %>%
      group_by(Category) %>%
      summarize(TotalQuestions = n(),
                Correct = sum(`Correct?`),
                Accuracy = (Correct / TotalQuestions) %>% percent(),
                PointsScored = sum(PointsScored),
                PointsWasted = sum(PointsWasted)) %>%
      arrange(desc(Accuracy), desc(TotalQuestions), PointsScored, Category) %>%
      head(n = 10) %>%
      ungroup() %>%
      select(Category, TotalQuestions, Correct, Accuracy, PointsScored) %>%
      kable(format = "html", align = "lllll", caption = "Top 10 Best Categories (Accuracy)", col.names = c("Category", "Total Questions", "Total Correct", "Accuracy", "Points Scored (Total)")) %>%
      kable_styling(bootstrap_options = c("hover", "responsive", "striped")) %>%
      scroll_box(width = "100%", height = "100%")
  })
  
  output$TotalPoints_Wasted = renderText({
    trivia %>%
      filter(Round %in% c("Bonus", "Halftime", "Final") == FALSE) %>%
      filter(is.na(Freebie)) %>%
      group_by(Category) %>%
      summarize(TotalQuestions = n(),
                Correct = sum(`Correct?`),
                Accuracy = (Correct / TotalQuestions) %>% percent(),
                PointsScored = sum(PointsScored),
                PointsWasted = sum(PointsWasted)) %>%
      arrange(desc(PointsWasted), desc(TotalQuestions), Category) %>%
      head(n = 10) %>%
      ungroup() %>%
      select(Category, TotalQuestions, Correct, Accuracy, PointsWasted) %>%
      kable(format = "html", align = "lllll", caption = "Top 10 Wasted Categories", col.names = c("Category", "Total Questions", "Total Correct", "Accuracy", "Points Wasted (Total)")) %>%
      kable_styling(bootstrap_options = c("hover", "responsive", "striped")) %>%
      scroll_box(width = "100%", height = "100%")
  })
  
  output$TotalPoints_Wasted_Date = renderText({
    trivia %>%
      filter(Round %in% c("Bonus", "Halftime", "Final") == FALSE) %>%
      filter(is.na(Freebie)) %>%
      group_by(Date) %>%
      summarize(TotalQuestions = n(),
                Correct = sum(`Correct?`),
                Accuracy = (Correct / TotalQuestions) %>% percent(),
                PointsScored = sum(PointsScored),
                PointsWasted = sum(PointsWasted)) %>%
      arrange(Date) %>%
      head(n = 10) %>%
      ungroup() %>%
      select(Date, TotalQuestions, Correct, Accuracy, PointsWasted) %>%
      kable(format = "html", align = "lllll", caption = "Top 10 Wasted Categories", col.names = c("Category", "Total Questions", "Total Correct", "Accuracy", "Points Wasted (Total)")) %>%
      kable_styling(bootstrap_options = c("hover", "responsive", "striped")) %>%
      scroll_box(width = "100%", height = "100%")
  })
  
  output$AvgPoints_Correct = renderText({
    trivia %>%
      filter(Round %in% c("Bonus", "Halftime", "Final") == FALSE) %>%
      filter(is.na(Freebie)) %>%
      group_by(Category) %>%
      summarize(TotalQuestions = n(),
                Correct = sum(`Correct?`),
                Accuracy = (Correct / TotalQuestions) %>% percent(),
                PointsScored = sum(PointsScored),
                PointsWasted = sum(PointsWasted),
                AvgPoints = sum(PointsScored) / TotalQuestions) %>%
      arrange(desc(AvgPoints), desc(TotalQuestions), Category) %>%
      head(n = 10) %>%
      ungroup() %>%
      select(Category, Accuracy, TotalQuestions, AvgPoints) %>%
      kable(format = "html", align = "llll", caption = "Top 10 Best Categories (Average Points)", col.names = c("Category", "Accuracy", "Total Questions", "Points Scored (Avg)")) %>%
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
    
    ggplot(data = trivia %>%
             filter(Round %in% c("Bonus", "Halftime", "Final") == FALSE) %>%
             filter(is.na(Freebie)) %>%
             group_by(Category) %>%
             summarize(TotalQuestions = n(),
                       Correct = sum(`Correct?`),
                       Accuracy = (Correct / TotalQuestions) %>% percent(),
                       AvgPoints = sum(PointsScored) / TotalQuestions) %>%
             arrange(desc(TotalQuestions), Category) %>%
             head(n = 10) %>%
             ungroup() %>%
             select(Category, Correct, TotalQuestions)
             , aes(x = TotalQuestions, y = Category, fill = Correct)) +
      geom_bar(stat = "identity", fill = "#08A45C", width = 0.75) +
      geom_text(aes(label = Correct %>% format(nsmall = 0)), hjust = 2, vjust = 0.5, color = "white", size = 4) +
      xlab(label = "Number of Total Questions") +
      #ylab(label = "Genre") +
      ggtitle(label = "Top 10 Trivia Categories") +
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
             filter(Round %in% c("Bonus", "Halftime", "Final") == FALSE) %>%
             filter(is.na(Freebie)) %>%
             group_by(Date) %>%
             summarize(TotalQuestions = n(),
                       Correct = sum(`Correct?`),
                       Accuracy = (Correct / TotalQuestions) %>% percent(),
                       PointsScored = sum(PointsScored),
                       PointsWasted = sum(PointsWasted),
                       AvgPoints = sum(PointsScored) / TotalQuestions) %>%
             arrange(Date) %>%
             head(n = 10) %>%
             ungroup() %>%
             select(Date, PointsScored)
           , aes(x = Date, y = PointsScored)) +
      geom_line(color = "#08A45C", alpha = 0.75) +
      geom_point() +
      geom_text(aes(label = PointsScored %>% format(nsmall = 0)), hjust = 2, vjust = 0.5, color = "black", size = 4) +
      xlab(label = "Date") +
      #ylab(label = "Genre") +
      ggtitle(label = "Points Score by Date") +
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
  
  
}







shinyApp(
  ui = uix,
  server = serverx
)